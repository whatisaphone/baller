use crate::script::{
    ast::{DecompileErrorKind, Expr, ExprId, Scripto, Stmt, StmtBlock},
    control::{ConditionKind, Control, ControlBlock},
    decode::Decoder,
    ins::{GenericArg, GenericIns, Ins, Operand, Variable},
};

pub fn build_ast<'a>(controls: &[ControlBlock], code: &'a [u8]) -> (Scripto<'a>, StmtBlock<'a>) {
    let mut script = Scripto::default();
    let mut block = StmtBlock::default();
    let entry = &controls[0];
    match decompile_block(
        entry,
        code,
        controls,
        &mut script,
        &mut block,
        BlockExit::Jump(entry.end),
    ) {
        Ok(()) => {}
        Err(DecompileError(offset, message)) => {
            // TODO: address
            block.push(usize::MAX, Stmt::DecompileError(offset, message));
        }
    }
    (script, block)
}

struct DecompileError(usize, DecompileErrorKind);

#[allow(clippy::too_many_lines)]
fn decompile_block<'a>(
    block: &ControlBlock,
    code: &'a [u8],
    controls: &[ControlBlock],
    script: &mut Scripto<'a>,
    out: &mut StmtBlock<'a>,
    expected_exit: BlockExit,
) -> Result<(), DecompileError> {
    debug_assert!(matches!(expected_exit, BlockExit::Jump(_))); // not passing expr to caller

    match &block.control {
        Control::CodeRange => {
            debug_assert!(matches!(expected_exit, BlockExit::Jump(_)));
            decompile_stmts(code, block, script, out, expected_exit)?;
            Ok(())
        }
        Control::Sequence(children) => {
            debug_assert!(matches!(expected_exit, BlockExit::Jump(_)));

            // Skip zero-length blocks since they mess up expected exits. The passed
            // expected exit must be sent to the last block that actually contains
            // instructions.
            let last_nonempty_child = children
                .iter()
                .rposition(|&b| controls[b].start != controls[b].end)
                .unwrap();

            for &i in &children[..last_nonempty_child] {
                decompile_block(
                    &controls[i],
                    code,
                    controls,
                    script,
                    out,
                    BlockExit::Jump(controls[i].end),
                )?;
            }
            decompile_block(
                &controls[children[last_nonempty_child]],
                code,
                controls,
                script,
                out,
                expected_exit,
            )?;
            Ok(())
        }
        Control::If(b) => {
            let (condition_addr, condition) = decompile_stmts(
                code,
                &controls[b.condition],
                script,
                out,
                BlockExit::JumpUnless(controls[b.true_].end),
            )?
            .unwrap();

            let mut true_block = StmtBlock::default();
            let expected = match b.false_ {
                Some(false_) => controls[false_].end,
                None => controls[b.true_].end,
            };
            decompile_block(
                &controls[b.true_],
                code,
                controls,
                script,
                &mut true_block,
                BlockExit::Jump(expected),
            )?;

            let mut false_block = StmtBlock::default();
            if let Some(false_) = b.false_ {
                decompile_block(
                    &controls[false_],
                    code,
                    controls,
                    script,
                    &mut false_block,
                    BlockExit::Jump(controls[false_].end),
                )?;
            }

            out.push(condition_addr, Stmt::If {
                condition,
                true_: true_block,
                false_: false_block,
            });
            out.end = block.end;
            Ok(())
        }
        Control::Do(b) => {
            let mut body_block = StmtBlock::default();
            decompile_block(
                &controls[b.body],
                code,
                controls,
                script,
                &mut body_block,
                BlockExit::Jump(controls[b.condition].start),
            )?;

            let expected_exit = match b.condition_kind {
                ConditionKind::Always => BlockExit::Jump(controls[b.body].start),
                ConditionKind::Until => BlockExit::JumpUnless(controls[b.body].start),
            };
            let cond = decompile_stmts(
                code,
                &controls[b.condition],
                script,
                &mut body_block,
                expected_exit,
            )?;

            out.push(block.start, Stmt::Do {
                body: body_block,
                condition: cond.map(|(_addr, expr)| expr),
                end: block.end,
            });
            out.end = block.end;
            Ok(())
        }
    }
}

#[allow(clippy::too_many_lines)]
fn decompile_stmts<'a>(
    code: &'a [u8],
    block: &ControlBlock,
    script: &mut Scripto<'a>,
    out: &mut StmtBlock<'a>,
    expected_exit: BlockExit,
) -> Result<Option<(usize, ExprId)>, DecompileError> {
    debug_assert!(matches!(block.control, Control::CodeRange));

    let mut stack = Vec::new();
    let mut string_stack = Vec::new();

    let decoder = Decoder::new(code);
    decoder.set_pos(block.start);

    let mut next_stmt_addr = block.start;

    macro_rules! pop {
        () => {
            stack
                .pop()
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
        (: string) => {
            pop_string(script, &mut stack, &mut string_stack)
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
        (: list) => {
            pop_list(script, &mut stack)
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
    }

    macro_rules! bump_stmt_addr {
        () => {{
            let addr = next_stmt_addr;
            #[allow(unused_assignments)]
            {
                next_stmt_addr = decoder.pos();
            }
            addr
        }};
    }

    macro_rules! handle_jump {
        ($target:expr, $expr:expr) => {
            let target = $target;
            let expr = $expr;

            if decoder.pos() == block.end {
                return Ok(finish_block(
                    expected_exit,
                    bump_stmt_addr!(),
                    decoder.pos(),
                    target,
                    expr,
                    &mut stack,
                    script,
                    out,
                ));
            }
            append_goto(bump_stmt_addr!(), decoder.pos(), target, expr, script, out);
        };
    }

    macro_rules! push_expr {
        ($expr:expr) => {{
            stack.push(add_expr(script, $expr));
        }};
    }

    while decoder.pos() < block.end {
        let (off, ins) = decoder.next().ok_or_else(|| {
            DecompileError(decoder.pos(), DecompileErrorKind::Other("opcode decode"))
        })?;
        match ins {
            Ins::Push(op) => {
                match op {
                    Operand::Byte(x) => push_expr!(Expr::Number(x.into())),
                    Operand::I16(x) => push_expr!(Expr::Number(x.into())),
                    Operand::I32(x) => push_expr!(Expr::Number(x)),
                    Operand::Var(var) => push_expr!(Expr::Variable(var)),
                    Operand::String(s) => string_stack.push(s),
                }
            }
            Ins::GetArrayItem(var) => {
                let x = pop!()?;
                push_expr!(Expr::ArrayIndex(var, x));
            }
            Ins::GetArrayItem2D(var) => {
                let x = pop!()?;
                let y = pop!()?;
                push_expr!(Expr::ArrayIndex2D(var, y, x));
            }
            Ins::StackDupN(count) => {
                for _ in 0..count {
                    let &expr = stack
                        .get(stack.len() - usize::from(count))
                        .ok_or(DecompileError(off, DecompileErrorKind::StackUnderflow))?;
                    push_expr!(Expr::StackDup(expr));
                }
            }
            Ins::StackDup => {
                // TODO: only constant expressions?
                let expr = stack
                    .last()
                    .copied()
                    .unwrap_or_else(|| add_expr(script, Expr::StackUnderflow));
                push_expr!(Expr::StackDup(expr));
            }
            Ins::Not => {
                let expr = pop!()?;
                push_expr!(Expr::Not(expr));
            }
            Ins::Equal => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::Equal(lhs, rhs));
            }
            Ins::NotEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::NotEqual(lhs, rhs));
            }
            Ins::Greater => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::Greater(lhs, rhs));
            }
            Ins::Less => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::Less(lhs, rhs));
            }
            Ins::LessOrEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::LessOrEqual(lhs, rhs));
            }
            Ins::GreaterOrEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::GreaterOrEqual(lhs, rhs));
            }
            Ins::Add => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::Add(lhs, rhs));
            }
            Ins::Sub => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::Sub(lhs, rhs));
            }
            Ins::Mul => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::Mul(lhs, rhs));
            }
            Ins::Div => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::Div(lhs, rhs));
            }
            Ins::LogicalAnd => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::LogicalAnd(lhs, rhs));
            }
            Ins::LogicalOr => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::LogicalOr(lhs, rhs));
            }
            Ins::PopDiscard => {
                if pop!().is_err() {
                    out.push(
                        bump_stmt_addr!(),
                        Stmt::DecompileError(off, DecompileErrorKind::StackUnderflow),
                    );
                }
            }
            Ins::In => {
                let list = pop!(:list)?;
                let value = pop!()?;
                push_expr!(Expr::In(value, list));
            }
            Ins::DimArray2D(item_size, var) => {
                let swap = pop!()?;
                let max_x = pop!()?;
                let min_x = pop!()?;
                let max_y = pop!()?;
                let min_y = pop!()?;
                out.push(bump_stmt_addr!(), Stmt::DimArray {
                    var,
                    item_size,
                    min_y,
                    max_y,
                    min_x,
                    max_x,
                    swap,
                });
            }
            Ins::RedimArray2D(item_size, var) => {
                let max_x = pop!()?;
                let min_x = pop!()?;
                let max_y = pop!()?;
                let min_y = pop!()?;
                out.push(bump_stmt_addr!(), Stmt::RedimArray {
                    var,
                    item_size,
                    min_y,
                    max_y,
                    min_x,
                    max_x,
                });
            }
            Ins::Set(var) => {
                let expr = pop!()?;
                out.push(bump_stmt_addr!(), Stmt::Assign(var, expr));
            }
            Ins::SetArrayItem(var) => {
                let value = pop!()?;
                let index = pop!()?;
                out.push(bump_stmt_addr!(), Stmt::SetArrayItem(var, index, value));
            }
            Ins::SetArrayItem2D(var) => {
                let value = pop!()?;
                let index_x = pop!()?;
                let index_y = pop!()?;
                out.push(
                    bump_stmt_addr!(),
                    Stmt::SetArrayItem2D(var, index_y, index_x, value),
                );
            }
            Ins::Inc(var) => {
                out.push(bump_stmt_addr!(), Stmt::Inc(var));
            }
            Ins::Dec(var) => {
                out.push(bump_stmt_addr!(), Stmt::Dec(var));
            }
            Ins::JumpIf(rel) => {
                #[allow(clippy::cast_sign_loss)]
                let target = decoder.pos().wrapping_add(rel as isize as usize);
                let expr = pop!()?;
                let expr = add_expr(script, Expr::Not(expr));
                handle_jump!(target, Some(expr));
            }
            Ins::JumpUnless(rel) => {
                #[allow(clippy::cast_sign_loss)]
                let target = decoder.pos().wrapping_add(rel as isize as usize);
                let expr = pop!()?;
                handle_jump!(target, Some(expr));
            }
            Ins::Jump(rel) => {
                #[allow(clippy::cast_sign_loss)]
                let target = decoder.pos().wrapping_add(rel as isize as usize);
                handle_jump!(target, None);
            }
            Ins::AssignString(var) => {
                let expr = pop!(:string)?;
                out.push(bump_stmt_addr!(), Stmt::Assign(var, expr));
            }
            Ins::Sprintf(var) => {
                let args = pop!(:list)?;
                let first_arg = pop!()?;
                match &mut script.exprs[args] {
                    Expr::List(xs) => xs.insert(0, first_arg),
                    _ => unreachable!(),
                }
                let format = pop!(:string)?;
                let dest = add_expr(script, Expr::Variable(var));
                out.push(bump_stmt_addr!(), Stmt::Generic {
                    bytecode: bytearray![0xa4, 0xc2],
                    ins: &GenericIns {
                        name: Some("sprintf"),
                        args: &[GenericArg::String, GenericArg::Int, GenericArg::List],
                        returns_value: false,
                    },
                    args: vec![dest, format, args],
                });
            }
            Ins::DimArray1DSimple(item_size, var) => {
                let max_x = pop!()?;
                out.push(bump_stmt_addr!(), Stmt::DimArray {
                    var,
                    item_size,
                    min_y: add_expr(script, Expr::Number(0)),
                    max_y: add_expr(script, Expr::Number(0)),
                    min_x: add_expr(script, Expr::Number(0)),
                    max_x,
                    swap: add_expr(script, Expr::Number(2)),
                });
            }
            Ins::DimArray2DSimple(item_size, var) => {
                let max_x = pop!()?;
                let max_y = pop!()?;
                out.push(bump_stmt_addr!(), Stmt::DimArray {
                    var,
                    item_size,
                    min_y: add_expr(script, Expr::Number(0)),
                    max_y,
                    min_x: add_expr(script, Expr::Number(0)),
                    max_x,
                    swap: add_expr(script, Expr::Number(2)),
                });
            }
            Ins::BitwiseAnd => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::BitwiseAnd(lhs, rhs));
            }
            Ins::BitwiseOr => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                push_expr!(Expr::BitwiseOr(lhs, rhs));
            }
            Ins::Generic(bytecode, operands, ins) => {
                let mut args = Vec::with_capacity(operands.len() + ins.args.len());
                for arg in ins.args.iter().rev() {
                    let expr = match arg {
                        GenericArg::Int | GenericArg::IntScript => pop!()?,
                        GenericArg::String => pop!(:string)?,
                        GenericArg::List => pop!(:list)?,
                    };
                    args.push(expr);
                }
                for op in operands.iter().rev() {
                    args.push(operand_to_expr(script, op));
                }
                args.reverse();
                if ins.returns_value {
                    push_expr!(Expr::Call(bytecode, ins, args));
                } else {
                    out.push(bump_stmt_addr!(), Stmt::Generic {
                        bytecode,
                        ins,
                        args,
                    });
                }
            }
        }
    }
    Ok(finish_block(
        expected_exit,
        bump_stmt_addr!(),
        decoder.pos(),
        decoder.pos(),
        None,
        &mut stack,
        script,
        out,
    ))
}

fn add_expr<'a>(script: &mut Scripto<'a>, expr: Expr<'a>) -> ExprId {
    let id = script.exprs.len();
    script.exprs.push(expr);
    id
}

fn operand_to_expr<'a>(script: &mut Scripto<'a>, operand: &Operand<'a>) -> ExprId {
    let expr = match *operand {
        Operand::Byte(x) => Expr::Number(x.into()),
        Operand::I16(x) => Expr::Number(x.into()),
        Operand::I32(x) => Expr::Number(x),
        Operand::Var(v) => Expr::Variable(v),
        Operand::String(s) => Expr::String(s),
    };
    add_expr(script, expr)
}

#[allow(clippy::too_many_arguments)]
fn finish_block<'a>(
    expected_exit: BlockExit,
    pos: usize,
    block_end: usize,
    target: usize,
    mut expr: Option<ExprId>,
    stack: &mut Vec<ExprId>,
    script: &mut Scripto<'a>,
    out: &mut StmtBlock<'a>,
) -> Option<(usize, ExprId)> {
    while let Some(expr) = stack.pop() {
        out.push(
            pos,
            Stmt::DecompileError(pos, DecompileErrorKind::StackOrphan(expr)),
        );
    }

    let actual_exit = match expr {
        None => BlockExit::Jump(target),
        Some(_) => BlockExit::JumpUnless(target),
    };

    if actual_exit != expected_exit {
        append_goto(pos, block_end, target, expr.take(), script, out);
    }

    out.end = pos;

    match expected_exit {
        BlockExit::Jump(_) => None,
        BlockExit::JumpUnless(_) => {
            let expr = expr.unwrap_or_else(|| {
                add_expr(
                    script,
                    Expr::DecompileError(pos, DecompileErrorKind::WrongBlockExit),
                )
            });
            Some((pos, expr))
        }
    }
}

fn append_goto<'a>(
    pos: usize,
    block_end: usize,
    target: usize,
    expr: Option<ExprId>,
    script: &mut Scripto<'a>,
    out: &mut StmtBlock<'a>,
) {
    match expr {
        None => {
            out.push(pos, Stmt::Goto(target));
        }
        Some(expr) => {
            let condition = add_expr(script, Expr::Not(expr));
            out.push(pos, Stmt::If {
                condition,
                true_: StmtBlock {
                    addrs: vec![pos],
                    stmts: vec![Stmt::Goto(target)],
                    end: block_end,
                },
                false_: StmtBlock::default(),
            });
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum BlockExit {
    Jump(usize),
    JumpUnless(usize),
}

fn pop_string<'a>(
    script: &mut Scripto<'a>,
    stack: &mut Vec<ExprId>,
    string_stack: &mut Vec<&'a [u8]>,
) -> Option<ExprId> {
    match script.exprs[stack.pop()?] {
        Expr::Number(-1) => Some(add_expr(script, Expr::String(string_stack.pop()?))),
        Expr::Number(var_id) => {
            Some(add_expr(
                script,
                Expr::Variable(Variable(var_id.try_into().ok()?)),
            ))
        }
        Expr::Variable(var) => Some(add_expr(script, Expr::Variable(var))),
        _ => None,
    }
}

fn pop_list<'a>(script: &mut Scripto<'a>, stack: &mut Vec<ExprId>) -> Option<ExprId> {
    let Expr::Number(len) = script.exprs[stack.pop()?] else { return None };
    let mut list = Vec::with_capacity(len.try_into().ok()?);
    for _ in 0..len {
        list.push(stack.pop()?);
    }
    list.reverse();
    Some(add_expr(script, Expr::List(list)))
}
