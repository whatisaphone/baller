use crate::{
    config::{Type, Var},
    script::{
        ast::{
            resolve_script,
            resolve_variable,
            CaseCond,
            Expr,
            ExprId,
            Scripto,
            Stmt,
            StmtBlock,
            WriteCx,
        },
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

struct Typist<'a> {
    types: Vec<Option<&'a Type>>,
    cx: WriteCx<'a>,
}

impl<'a> Typist<'a> {
    fn get_ty(&self, id: ExprId) -> Option<&'a Type> {
        *self.types.get(id)?
    }

    fn set_ty(&mut self, id: ExprId, type_: Option<&'a Type>) {
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
                specify(script, expr, ty, &self.cx);
            }
            Stmt::SetArrayItem(var, x, value) => {
                specify_array_indices(script, var, None, x, &self.cx);
                let item_type = array_item_type(script, var, None, x, &self.cx);
                specify(script, value, item_type, &self.cx);
            }
            Stmt::SetArrayItem2D(var, y, x, value) => {
                specify_array_indices(script, var, Some(y), x, &self.cx);
                let item_type = array_item_type(script, var, None, x, &self.cx);
                specify(script, value, item_type, &self.cx);
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
                            specify(script, e, ty, &self.cx);
                        }
                        CaseCond::In(e) => {
                            specify_list_items(script, e, ty, &self.cx);
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
            Expr::ArrayIndex(var, index) => {
                specify_array_indices(script, var, None, index, &self.cx);
                let item_type = array_item_type(script, var, None, index, &self.cx);
                self.set_ty(id, item_type);
            }
            Expr::ArrayIndex2D(var, y_index, x_index) => {
                specify_array_indices(script, var, Some(y_index), x_index, &self.cx);
                let item_type = array_item_type(script, var, Some(y_index), x_index, &self.cx);
                self.set_ty(id, item_type);
            }
            Expr::Equal(lhs, rhs) | Expr::NotEqual(lhs, rhs) => {
                let ty = unify(self.get_ty(lhs), self.get_ty(rhs));
                specify(script, lhs, ty, &self.cx);
                specify(script, rhs, ty, &self.cx);
            }
            Expr::Call(_, ins, ref args) => {
                let args = args.clone(); // :(
                type_generic(script, ins, &args, &self.cx);
            }
            Expr::In(value, list) => {
                let ty = self.get_ty(value);
                specify_list_items(script, list, ty, &self.cx);
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
    if !(name.ends_with("-script") || name.ends_with("-script-xc3")) {
        return;
    }
    let &script_target_expr = match ins_args.first() {
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
        specify(script, args!()[i], params[i].ty.as_ref(), cx);
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

fn unify<'a>(lhs: Option<&'a Type>, rhs: Option<&'a Type>) -> Option<&'a Type> {
    match (lhs, rhs) {
        (lhs, None) => lhs,
        (None, rhs) => rhs,
        (_, _) => None,
    }
}

fn find_var_type<'a>(var: Variable, cx: &WriteCx<'a>) -> Option<&'a Type> {
    let (_, ty) = resolve_variable(var, cx);
    ty
}

fn specify(script: &mut Scripto, id: ExprId, ty: Option<&Type>, cx: &WriteCx) {
    let ty = match ty {
        Some(ty) => ty,
        None => return,
    };
    match (ty, &script.exprs[id]) {
        (&Type::Enum(enum_id), &Expr::Number(number)) => {
            if !cx.config.enums[enum_id].values.contains_key(&number) {
                return;
            }
            script.exprs[id] = Expr::EnumConst(enum_id, number);
        }
        (&Type::Char, &Expr::Number(number)) => {
            script.exprs[id] = Expr::Char(number.try_into().unwrap());
        }
        (&Type::Script, &Expr::Number(number)) => {
            script.exprs[id] = Expr::Script(number);
        }
        _ => {}
    }
}

fn specify_list_items(script: &mut Scripto, id: ExprId, ty: Option<&Type>, cx: &WriteCx) {
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
        specify(script, e, ty, cx);
    }
}

fn specify_array_indices(
    script: &mut Scripto,
    var: Variable,
    y_index: Option<usize>,
    x_index: usize,
    cx: &WriteCx,
) {
    let ty = match find_var_type(var, cx) {
        Some(ty) => ty,
        None => return,
    };
    let (y_ty, x_ty) = match ty {
        Type::Array { item: _, y, x } | Type::AssocArray { assoc: _, y, x } => (y, x),
        _ => return,
    };
    if let Some(y_index) = y_index {
        specify(script, y_index, Some(y_ty), cx);
    }
    specify(script, x_index, Some(x_ty), cx);
}

fn array_item_type<'a>(
    script: &mut Scripto,
    var: Variable,
    _y_index: Option<usize>,
    x_index: usize,
    cx: &WriteCx<'a>,
) -> Option<&'a Type> {
    match find_var_type(var, cx)? {
        Type::Array { item, .. } => Some(item),
        &Type::AssocArray { assoc, .. } => {
            let index = match script.exprs[x_index] {
                Expr::Number(n) | Expr::EnumConst(_, n) => n,
                _ => return None,
            };
            let index: usize = index.try_into().ok()?;
            cx.config.assocs[assoc].types.get(index)
        }
        _ => None,
    }
}
