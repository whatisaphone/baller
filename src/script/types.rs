use crate::{
    config::EnumId,
    script::{
        ast::{Expr, ExprId, Scripto, Stmt, StmtBlock},
        ins::Variable,
        visit::{default_visit_stmt, Visitor},
    },
    Config,
};

pub fn spread_types(scripto: &mut Scripto, root: &mut StmtBlock, config: &Config) {
    let mut typist = Typist { config };
    typist.block(scripto, root);
}

#[derive(Copy, Clone)]
enum Type {
    Enum(EnumId),
}

struct Typist<'a> {
    config: &'a Config,
}

impl Visitor for Typist<'_> {
    fn stmt(&mut self, script: &mut Scripto, stmt: &mut Stmt) {
        default_visit_stmt(script, stmt, self);
        match stmt {
            &mut Stmt::Assign(var, expr) => {
                let ty = match find_var_type(var, self.config) {
                    Some(ty) => ty,
                    None => return,
                };
                specify(script, expr, ty, self.config);
            }
            _ => {}
        }
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

fn specify(script: &mut Scripto, id: ExprId, ty: Type, config: &Config) {
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
