use crate::{
    config::EnumId,
    script::{
        ast::{get_script_config, CaseCond, Expr, ExprId, Scripto, Stmt, StmtBlock, WriteCx},
        ins::Variable,
        visit::{default_visit_expr, default_visit_stmt, Visitor},
        Scope,
    },
    Config,
};

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
                        _ => {}
                    }
                }
                for case in cases {
                    match case.cond {
                        CaseCond::Eq(e) => {
                            specify(script, e, ty, self.cx.config);
                        }
                        _ => {}
                    }
                }
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
            _ => {}
        }
    }
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
