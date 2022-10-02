use crate::{
    config::{EnumId, Script, Var},
    script::{
        ast::{get_script_config, CaseCond, Expr, ExprId, Scripto, Stmt, StmtBlock, WriteCx},
        ins::{GenericIns, Variable},
        visit::{default_visit_expr, default_visit_stmt, Visitor},
        Scope,
    },
    Config,
};
use std::cmp::min;

pub fn spread_types(scripto: &mut Scripto, root: &mut StmtBlock, scope: Scope, config: &Config) {
    let mut typist = Typist {
        types: Vec::with_capacity(64),
        cx: WriteCx { scope, config },
    };
    typist.block(scripto, root);
}

#[derive(Copy, Clone)]
enum Type {
    Enum(EnumId),
}

struct Typist<'a> {
    types: Vec<Option<Type>>,
    cx: WriteCx<'a>,
}

impl Typist<'_> {
    fn get_ty(&self, id: ExprId) -> Option<Type> {
        *self.types.get(id)?
    }

    fn set_ty(&mut self, id: ExprId, type_: Option<Type>) {
        let type_ = match type_ {
            Some(type_) => type_,
            None => return,
        };
        if self.types.len() < id {
            self.types.resize_with(id, || None);
        }
        self.types.insert(id, Some(type_));
    }
}

impl Visitor for Typist<'_> {
    fn stmt(&mut self, script: &mut Scripto, stmt: &mut Stmt) {
        default_visit_stmt(script, stmt, self);

        match *stmt {
            Stmt::Assign(var, expr) => {
                let ty = find_var_type(var, &self.cx);
                specify(script, expr, ty, self.cx.config);
            }
            Stmt::Case { value, ref cases } => {
                let mut ty = self.get_ty(value);
                for case in cases {
                    match case.cond {
                        CaseCond::Eq(e) => {
                            ty = unify(ty, self.get_ty(e));
                        }
                        CaseCond::In(_) | CaseCond::Else => {}
                    }
                }
                for case in cases {
                    match case.cond {
                        CaseCond::Eq(e) => {
                            specify(script, e, ty, self.cx.config);
                        }
                        CaseCond::In(e) => {
                            specify_list_items(script, e, ty, self.cx.config);
                        }
                        CaseCond::Else => {}
                    }
                }
            }
            Stmt::Generic {
                bytecode: _,
                ins,
                ref args,
            } => {
                type_generic(script, ins, args, &self.cx);
            }
            _ => {}
        }
    }

    fn expr(&mut self, script: &mut Scripto, id: ExprId) {
        default_visit_expr(script, id, self);

        match script.exprs[id] {
            Expr::Variable(var) => {
                self.set_ty(id, find_var_type(var, &self.cx));
            }
            Expr::StackDup(e) => {
                self.set_ty(id, self.get_ty(e));
            }
            Expr::Equal(lhs, rhs) | Expr::NotEqual(lhs, rhs) => {
                let ty = unify(self.get_ty(lhs), self.get_ty(rhs));
                specify(script, lhs, ty, self.cx.config);
                specify(script, rhs, ty, self.cx.config);
            }
            Expr::Call(_, ins, ref args) => {
                let args = args.clone(); // :(
                type_generic(script, ins, &args, &self.cx);
            }
            _ => {}
        }
    }
}

fn type_generic(script: &mut Scripto, ins: &GenericIns, ins_args: &[ExprId], cx: &WriteCx) {
    let name = match &ins.name {
        Some(name) => name,
        None => return,
    };
    // There has got to be a better way to do this
    if !name.ends_with("-script") {
        return;
    }
    let &script_target_expr = match ins_args.get(0) {
        Some(script) => script,
        None => return,
    };
    let &script_args_expr = match ins_args.last() {
        Some(args) => args,
        None => return,
    };
    let script_args = match &script.exprs[script_args_expr] {
        Expr::List(xs) => xs,
        _ => return,
    };
    let params = match resolve_script_params(script, script_target_expr, cx) {
        Some(params) => params,
        None => return,
    };
    let count = min(params.len(), script_args.len());

    // work around borrow checker
    macro_rules! args {
        () => {
            match &script.exprs[script_args_expr] {
                Expr::List(xs) => xs,
                _ => unreachable!(),
            }
        };
    }

    #[allow(clippy::needless_range_loop)]
    for i in 0..count {
        specify(script, args!()[i], params[i].ty.map(Type::Enum), cx.config);
    }
}

fn resolve_script_params<'a>(
    script: &Scripto,
    script_ref: ExprId,
    cx: &'a WriteCx,
) -> Option<&'a [Var]> {
    match script.exprs[script_ref] {
        Expr::Number(number) => {
            let script = resolve_script(number, cx)?;
            let end = min(script.locals.len(), script.params?.into());
            Some(&script.locals[..end])
        }
        _ => None,
    }
}

pub const LOCAL_SCRIPT_CUTOFF: i32 = 2048;

fn resolve_script<'a>(number: i32, cx: &WriteCx<'a>) -> Option<&'a Script> {
    let number: usize = number.try_into().ok()?;
    if number < 2048 {
        return cx.config.scripts.get(number);
    }
    let room: usize = cx.scope.room()?.try_into().ok()?;
    cx.config.rooms.get(room)?.scripts.get(number)
}

fn unify(lhs: Option<Type>, rhs: Option<Type>) -> Option<Type> {
    match (lhs, rhs) {
        (lhs, None) => lhs,
        (None, rhs) => rhs,
        (_, _) => None,
    }
}

fn find_var_type(var: Variable, cx: &WriteCx) -> Option<Type> {
    let (scope, number) = (var.0 & 0xf000, var.0 & 0x0fff);
    match scope {
        0x0000 => {
            // global
            cx.config
                .global_types
                .get(usize::from(number))
                .and_then(|&o| o)
                .map(Type::Enum)
        }
        0x4000 => {
            // local
            if let Some(script) = get_script_config(cx) {
                if let Some(var) = script.locals.get(usize::from(number)) {
                    return var.ty.map(Type::Enum);
                }
            }
            None
        }
        0x8000 => {
            // room
            if let Some(room) = cx.scope.room() {
                if let Ok(room) = usize::try_from(room) {
                    if let Some(room) = cx.config.rooms.get(room) {
                        if let Some(var) = room.vars.get(usize::from(number)) {
                            return var.ty.map(Type::Enum);
                        }
                    }
                }
            }
            None
        }
        _ => panic!("bad variable scope bits"),
    }
}

fn specify(script: &mut Scripto, id: ExprId, ty: Option<Type>, config: &Config) {
    let ty = match ty {
        Some(ty) => ty,
        None => return,
    };
    match (ty, &script.exprs[id]) {
        (Type::Enum(enum_id), &Expr::Number(number)) => {
            if !config.enums[enum_id].values.contains_key(&number) {
                return;
            }
            script.exprs[id] = Expr::EnumConst(enum_id, number);
        }
        _ => {}
    }
}

fn specify_list_items(script: &mut Scripto, id: ExprId, ty: Option<Type>, config: &Config) {
    let xs = match &script.exprs[id] {
        Expr::List(xs) => xs,
        _ => return,
    };
    let len = xs.len();
    for i in 0..len {
        let xs = match &script.exprs[id] {
            Expr::List(xs) => xs,
            _ => unreachable!(),
        };
        let e = xs[i];
        specify(script, e, ty, config);
    }
}
