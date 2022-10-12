use crate::script::{
    ast::{CaseCond, Expr, ExprId, Scripto, Stmt, StmtBlock},
    ins::Variable,
};
use std::mem;

pub trait Visitor: AsVisitor {
    fn block(&mut self, script: &mut Scripto, block: &mut StmtBlock) {
        default_visit_block(script, block, self.as_visit_mut());
    }

    fn stmt(&mut self, script: &mut Scripto, stmt: &mut Stmt) {
        default_visit_stmt(script, stmt, self.as_visit_mut());
    }

    fn expr(&mut self, script: &mut Scripto, id: ExprId) {
        default_visit_expr(script, id, self.as_visit_mut());
    }

    #[allow(unused_variables)]
    fn var(&mut self, var: Variable) {}
}

pub trait AsVisitor {
    fn as_visit_mut(&mut self) -> &mut dyn Visitor;
}

impl<V: Visitor> AsVisitor for V {
    fn as_visit_mut(&mut self) -> &mut dyn Visitor {
        self
    }
}

fn default_visit_block(script: &mut Scripto, block: &mut StmtBlock, visit: &mut dyn Visitor) {
    for s in &mut block.stmts {
        visit.stmt(script, s);
    }
}

#[allow(clippy::too_many_lines)]
pub fn default_visit_stmt(script: &mut Scripto, stmt: &mut Stmt, visit: &mut dyn Visitor) {
    #[allow(clippy::match_same_arms)]
    match *stmt {
        Stmt::DimArray {
            var,
            item_size: _,
            min_y,
            max_y,
            min_x,
            max_x,
            swap,
        } => {
            visit.var(var);
            visit.expr(script, min_y);
            visit.expr(script, max_y);
            visit.expr(script, min_x);
            visit.expr(script, max_x);
            visit.expr(script, swap);
        }
        Stmt::RedimArray {
            var,
            item_size: _,
            min_y,
            max_y,
            min_x,
            max_x,
        } => {
            visit.var(var);
            visit.expr(script, min_y);
            visit.expr(script, max_y);
            visit.expr(script, min_x);
            visit.expr(script, max_x);
        }
        Stmt::Assign(var, expr) => {
            visit.var(var);
            visit.expr(script, expr);
        }
        Stmt::SetArrayItem(var, x, value) => {
            visit.var(var);
            visit.expr(script, x);
            visit.expr(script, value);
        }
        Stmt::SetArrayItem2D(var, y, x, value) => {
            visit.var(var);
            visit.expr(script, y);
            visit.expr(script, x);
            visit.expr(script, value);
        }
        Stmt::Inc(var) | Stmt::Dec(var) => {
            visit.var(var);
        }
        Stmt::Label | Stmt::Goto(_) => {}
        Stmt::If {
            condition,
            ref mut true_,
            ref mut false_,
        } => {
            visit.expr(script, condition);
            visit.block(script, true_);
            visit.block(script, false_);
        }
        Stmt::While {
            condition,
            ref mut body,
        } => {
            visit.expr(script, condition);
            visit.block(script, body);
        }
        Stmt::Do {
            ref mut body,
            condition,
        } => {
            visit.block(script, body);
            if let Some(condition) = condition {
                visit.expr(script, condition);
            }
        }
        Stmt::Case {
            value,
            ref mut cases,
        } => {
            visit.expr(script, value);
            for case in cases {
                match case.cond {
                    CaseCond::Eq(x) | CaseCond::In(x) => visit.expr(script, x),
                    CaseCond::Else => {}
                }
                visit.block(script, &mut case.body);
            }
        }
        Stmt::For {
            var,
            start,
            end,
            step: _,
            ref mut body,
        } => {
            visit.var(var);
            visit.expr(script, start);
            visit.expr(script, end);
            visit.block(script, body);
        }
        Stmt::Generic {
            bytecode: _,
            ins: _,
            ref args,
        } => {
            for &e in args {
                visit.expr(script, e);
            }
        }
        Stmt::DecompileError(_, _) => {}
    }
}

pub fn default_visit_expr(script: &mut Scripto, id: ExprId, visit: &mut dyn Visitor) {
    match script.exprs[id] {
        Expr::Variable(var) => {
            visit.var(var);
        }
        Expr::List(ref mut exprs) => {
            // Temporarily take exprs, then put it back
            let exprs = mem::take(exprs);
            for &expr in &exprs {
                visit.expr(script, expr);
            }
            match script.exprs[id] {
                Expr::List(ref mut exprs_mut) => *exprs_mut = exprs,
                _ => unreachable!(),
            }
        }
        Expr::ArrayIndex(var, x) => {
            visit.var(var);
            visit.expr(script, x);
        }
        Expr::ArrayIndex2D(var, y, x) => {
            visit.var(var);
            visit.expr(script, y);
            visit.expr(script, x);
        }
        Expr::StackDup(expr) | Expr::Not(expr) => {
            visit.expr(script, expr);
        }
        Expr::Equal(lhs, rhs)
        | Expr::NotEqual(lhs, rhs)
        | Expr::Greater(lhs, rhs)
        | Expr::Less(lhs, rhs)
        | Expr::LessOrEqual(lhs, rhs)
        | Expr::GreaterOrEqual(lhs, rhs)
        | Expr::Add(lhs, rhs)
        | Expr::Sub(lhs, rhs)
        | Expr::Mul(lhs, rhs)
        | Expr::Div(lhs, rhs)
        | Expr::LogicalAnd(lhs, rhs)
        | Expr::LogicalOr(lhs, rhs)
        | Expr::BitwiseAnd(lhs, rhs)
        | Expr::BitwiseOr(lhs, rhs)
        | Expr::In(lhs, rhs) => {
            visit.expr(script, lhs);
            visit.expr(script, rhs);
        }
        Expr::Call(_, _, ref mut args) => {
            // Temporarily take args, then put it back
            let args = mem::take(args);
            for &arg in &args {
                visit.expr(script, arg);
            }
            match script.exprs[id] {
                Expr::Call(_, _, ref mut args_mut) => *args_mut = args,
                _ => unreachable!(),
            }
        }
        Expr::Number(_)
        | Expr::String(_)
        | Expr::StackUnderflow
        | Expr::EnumConst(_, _)
        | Expr::DecompileError(_, _) => {}
    }
}
