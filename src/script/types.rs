use crate::{
    config::EnumId,
    script::{
        ast::{CaseCond, Expr, ExprId, Scripto, Stmt, StmtBlock},
        ins::Variable,
        visit::{default_visit_expr, default_visit_stmt, Visitor},
    },
    Config,
};

pub fn spread_types(scripto: &mut Scripto, root: &mut StmtBlock, config: &Config) {
    let mut typist = Typist {
        types: Vec::with_capacity(64),
        config,
    };
    typist.block(scripto, root);
}

#[derive(Copy, Clone)]
enum Type {
    Enum(EnumId),
}

struct Typist<'a> {
    types: Vec<Option<Type>>,
    config: &'a Config,
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
                let ty = find_var_type(var, self.config);
                specify(script, expr, ty, self.config);
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
                            specify(script, e, ty, self.config);
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
                self.set_ty(id, find_var_type(var, self.config));
            }
            Expr::StackDup(e) => {
                self.set_ty(id, self.get_ty(e));
            }
            Expr::Equal(lhs, rhs) | Expr::NotEqual(lhs, rhs) => {
                let ty = unify(self.get_ty(lhs), self.get_ty(rhs));
                specify(script, lhs, ty, self.config);
                specify(script, rhs, ty, self.config);
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

fn find_var_type(var: Variable, config: &Config) -> Option<Type> {
    let (scope, number) = (var.0 & 0xf000, var.0 & 0x0fff);
    match scope {
        0x0000 => {
            // global
            let enum_id = (*config.global_types.get(usize::from(number))?)?;
            Some(Type::Enum(enum_id))
        }
        _ => None,
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
