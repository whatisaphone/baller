use crate::script::{
    ast::{CaseCond, Expr, ExprId, Scripto, Stmt, StmtBlock},
    ins::Variable,
};

pub trait Visitor: AsVisitor {
    fn block(&mut self, script: &Scripto, block: &StmtBlock) {
        default_visit_block(script, block, self.as_visit_mut());
    }

    fn stmt(&mut self, script: &Scripto, stmt: &Stmt) {
        default_visit_stmt(script, stmt, self.as_visit_mut());
    }

    fn expr(&mut self, script: &Scripto, id: ExprId) {
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

fn default_visit_block(script: &Scripto, block: &StmtBlock, visit: &mut dyn Visitor) {
    for s in &block.stmts {
        visit.stmt(script, s);
    }
}

pub fn default_visit_stmt(script: &Scripto, stmt: &Stmt, visit: &mut dyn Visitor) {
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
            ref true_,
            ref false_,
        } => {
            visit.expr(script, condition);
            visit.block(script, true_);
            visit.block(script, false_);
        }
        Stmt::While {
            condition,
            ref body,
        } => {
            visit.expr(script, condition);
            visit.block(script, body);
        }
        Stmt::Do {
            ref body,
            condition,
        } => {
            visit.block(script, body);
            if let Some(condition) = condition {
                visit.expr(script, condition);
            }
        }
        Stmt::Case { value, ref cases } => {
            visit.expr(script, value);
            for case in cases {
                match case.cond {
                    CaseCond::Eq(x) | CaseCond::In(x) => visit.expr(script, x),
                    CaseCond::Else => {}
                }
                visit.block(script, &case.body);
            }
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

fn default_visit_expr(script: &Scripto, id: ExprId, visit: &mut dyn Visitor) {
    match script.exprs[id] {
        Expr::Variable(var) => {
            visit.var(var);
        }
        Expr::List(ref exprs) => {
            for &expr in exprs {
                visit.expr(script, expr);
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
        Expr::Call(_, _, ref args) => {
            for &arg in args {
                visit.expr(script, arg);
            }
        }
        Expr::Number(_) | Expr::String(_) | Expr::StackUnderflow | Expr::DecompileError(_, _) => {}
    }
}
