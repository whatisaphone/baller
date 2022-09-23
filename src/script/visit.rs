use crate::script::{
    ast::{CaseCond, Expr, Stmt},
    ins::Variable,
};

pub trait Visitor: AsVisitor {
    fn stmts(&mut self, stmts: &[Stmt]) {
        visit_stmts(stmts, self.as_visit_mut());
    }

    fn stmt(&mut self, stmt: &Stmt) {
        visit_stmt(stmt, self.as_visit_mut());
    }

    fn expr(&mut self, expr: &Expr) {
        visit_expr(expr, self.as_visit_mut());
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

fn visit_stmts(stmts: &[Stmt], visit: &mut dyn Visitor) {
    for s in stmts {
        visit.stmt(s);
    }
}

fn visit_stmt(stmt: &Stmt, visit: &mut dyn Visitor) {
    match stmt {
        Stmt::DimArray {
            var,
            item_size: _,
            min_y,
            max_y,
            min_x,
            max_x,
            swap,
        } => {
            visit.var(*var);
            visit.expr(min_y);
            visit.expr(max_y);
            visit.expr(min_x);
            visit.expr(max_x);
            visit.expr(swap);
        }
        Stmt::RedimArray {
            var,
            item_size: _,
            min_y,
            max_y,
            min_x,
            max_x,
        } => {
            visit.var(*var);
            visit.expr(min_y);
            visit.expr(max_y);
            visit.expr(min_x);
            visit.expr(max_x);
        }
        Stmt::Assign(var, expr) => {
            visit.var(*var);
            visit.expr(expr);
        }
        Stmt::SetArrayItem(var, x, value) => {
            visit.var(*var);
            visit.expr(x);
            visit.expr(value);
        }
        Stmt::SetArrayItem2D(var, y, x, value) => {
            visit.var(*var);
            visit.expr(y);
            visit.expr(x);
            visit.expr(value);
        }
        Stmt::Inc(var) | Stmt::Dec(var) => {
            visit.var(*var);
        }
        Stmt::Goto(_target) => {}
        Stmt::If {
            condition,
            true_,
            false_,
        } => {
            visit.expr(condition);
            visit.stmts(true_);
            visit.stmts(false_);
        }
        Stmt::While { condition, body } => {
            visit.expr(condition);
            visit.stmts(body);
        }
        Stmt::Case { value, cases } => {
            visit.expr(value);
            for case in cases {
                match &case.cond {
                    CaseCond::Eq(x) | CaseCond::In(x) => visit.expr(x),
                    CaseCond::Else => {}
                }
                visit.stmts(&case.body);
            }
        }
        Stmt::Generic {
            bytecode: _,
            ins: _,
            args,
        } => {
            for e in args {
                visit.expr(e);
            }
        }
        Stmt::DecompileError(_, _) => {}
    }
}

fn visit_expr(expr: &Expr, visit: &mut dyn Visitor) {
    match expr {
        &Expr::Variable(var) => {
            visit.var(var);
        }
        Expr::List(exprs) => {
            for expr in exprs {
                visit.expr(expr);
            }
        }
        Expr::ArrayIndex(var, x) => {
            visit.var(*var);
            visit.expr(x);
        }
        Expr::ArrayIndex2D(var, xs) => {
            let (y, x) = &**xs;
            visit.var(*var);
            visit.expr(y);
            visit.expr(x);
        }
        Expr::StackDup(expr) | Expr::Not(expr) => {
            visit.expr(expr);
        }
        Expr::Equal(xs)
        | Expr::NotEqual(xs)
        | Expr::Greater(xs)
        | Expr::Less(xs)
        | Expr::LessOrEqual(xs)
        | Expr::GreaterOrEqual(xs)
        | Expr::Add(xs)
        | Expr::Sub(xs)
        | Expr::Mul(xs)
        | Expr::Div(xs)
        | Expr::LogicalAnd(xs)
        | Expr::LogicalOr(xs)
        | Expr::BitwiseAnd(xs)
        | Expr::BitwiseOr(xs)
        | Expr::In(xs) => {
            let (lhs, rhs) = &**xs;
            visit.expr(lhs);
            visit.expr(rhs);
        }
        Expr::Call(_bytecode, _ins, args) => {
            for arg in args {
                visit.expr(arg);
            }
        }
        Expr::Number(_) | Expr::String(_) | Expr::StackUnderflow | Expr::DecompileError(_, _) => {}
    }
}
