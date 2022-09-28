use crate::script::{
    ast::{DecompileErrorKind, Expr, Stmt, StmtBlock},
    control::{Control, ControlBlock},
    decode::Decoder,
    ins::{GenericArg, GenericIns, Ins, Operand, Variable},
};

pub fn build_ast<'a>(controls: &[ControlBlock], code: &'a [u8]) -> StmtBlock<'a> {
    let mut block = StmtBlock::default();
    let entry = &controls[0];
    match decompile_block(
        entry,
        code,
        controls,
        &mut block,
        BlockExit::Jump(entry.end),
    ) {
        Ok(()) => {}
        Err(DecompileError(offset, message)) => {
            block.stmts.push(Stmt::DecompileError(offset, message));
        }
    }
    block
}

struct DecompileError<'a>(usize, DecompileErrorKind<'a>);

#[allow(clippy::too_many_lines)]
fn decompile_block<'a>(
    block: &ControlBlock,
    code: &'a [u8],
    controls: &[ControlBlock],
    out: &mut StmtBlock<'a>,
    expected_exit: BlockExit,
) -> Result<(), DecompileError<'a>> {
    debug_assert!(matches!(expected_exit, BlockExit::Jump(_))); // not passing expr to caller

    match &block.control {
        Control::CodeRange => {
            decompile_stmts(code, block, out, expected_exit)?;
            Ok(())
        }
        Control::Sequence(children) => {
            for &i in &children[..children.len() - 1] {
                decompile_block(
                    &controls[i],
                    code,
                    controls,
                    out,
                    BlockExit::Jump(controls[i].end),
                )?;
            }
            decompile_block(
                &controls[*children.last().unwrap()],
                code,
                controls,
                out,
                expected_exit,
            )
        }
        Control::If(b) => {
            let condition = decompile_stmts(
                code,
                &controls[b.condition],
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
                &mut true_block,
                BlockExit::Jump(expected),
            )?;

            let mut false_block = StmtBlock::default();
            if let Some(false_) = b.false_ {
                decompile_block(
                    &controls[false_],
                    code,
                    controls,
                    &mut false_block,
                    BlockExit::Jump(controls[false_].end),
                )?;
            }

            out.stmts.push(Stmt::If {
                condition,
                true_: true_block,
                false_: false_block,
            });
            Ok(())
        }
        Control::While(b) => {
            let cond_expr = decompile_stmts(
                code,
                &controls[b.condition],
                out,
                BlockExit::JumpUnless(controls[b.body].end),
            )?
            .unwrap();

            let mut body = StmtBlock::default();
            decompile_block(
                &controls[b.body],
                code,
                controls,
                &mut body,
                BlockExit::Jump(controls[b.condition].start),
            )?;

            out.stmts.push(Stmt::While {
                condition: cond_expr,
                body,
            });
            Ok(())
        }
        Control::Do(b) => {
            let mut body_block = StmtBlock::default();
            let expected_exit = match b.condition {
                None => BlockExit::Jump(controls[b.body].start),
                Some(condition) => BlockExit::Jump(controls[condition].start),
            };
            decompile_block(
                &controls[b.body],
                code,
                controls,
                &mut body_block,
                expected_exit,
            )?;

            let cond_expr = match b.condition {
                None => None,
                Some(condition) => {
                    Some(
                        decompile_stmts(
                            code,
                            &controls[condition],
                            &mut body_block,
                            BlockExit::JumpUnless(controls[b.body].start),
                        )?
                        .unwrap(),
                    )
                }
            };

            out.stmts.push(Stmt::Do {
                body: body_block,
                condition: cond_expr,
            });
            Ok(())
        }
    }
}

#[allow(clippy::too_many_lines)]
fn decompile_stmts<'a>(
    code: &'a [u8],
    block: &ControlBlock,
    out: &mut StmtBlock<'a>,
    expected_exit: BlockExit,
) -> Result<Option<Expr<'a>>, DecompileError<'a>> {
    debug_assert!(matches!(block.control, Control::CodeRange));

    let mut stack = Vec::new();
    let mut string_stack = Vec::new();

    let decoder = Decoder::new(code);
    decoder.set_pos(block.start);

    macro_rules! pop {
        () => {
            stack
                .pop()
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
        (: string) => {
            pop_string(&mut stack, &mut string_stack)
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
        (: list) => {
            pop_list(&mut stack)
                .ok_or_else(|| DecompileError(decoder.pos(), DecompileErrorKind::StackUnderflow))
        };
    }

    macro_rules! handle_jump {
        ($target:expr, $expr:expr) => {
            let target = $target;
            let expr = $expr;

            if decoder.pos() == block.end {
                return Ok(finish_block(
                    expected_exit,
                    decoder.pos(),
                    target,
                    expr,
                    &mut stack,
                    out,
                ));
            }
            unfinished_block(decoder.pos(), expr, out);
        };
    }

    while decoder.pos() < block.end {
        let (off, ins) = decoder.next().ok_or_else(|| {
            DecompileError(decoder.pos(), DecompileErrorKind::Other("opcode decode"))
        })?;
        match ins {
            Ins::Push(op) => {
                match op {
                    Operand::Byte(x) => stack.push(Expr::Number(x.into())),
                    Operand::I16(x) => stack.push(Expr::Number(x.into())),
                    Operand::I32(x) => stack.push(Expr::Number(x)),
                    Operand::Var(var) => stack.push(Expr::Variable(var)),
                    Operand::String(s) => string_stack.push(s),
                }
            }
            Ins::GetArrayItem(var) => {
                let x = pop!()?;
                stack.push(Expr::ArrayIndex(var, Box::new(x)));
            }
            Ins::GetArrayItem2D(var) => {
                let x = pop!()?;
                let y = pop!()?;
                stack.push(Expr::ArrayIndex2D(var, Box::new((y, x))));
            }
            Ins::StackDupN(count) => {
                for _ in 0..count {
                    let expr = stack
                        .get(stack.len() - usize::from(count))
                        .ok_or(DecompileError(off, DecompileErrorKind::StackUnderflow))?;
                    let expr = Box::new(expr.clone());
                    stack.push(Expr::StackDup(expr));
                }
            }
            Ins::StackDup => {
                // TODO: only constant expressions?
                stack.push(Expr::StackDup(Box::new(
                    stack.last().cloned().unwrap_or(Expr::StackUnderflow),
                )));
            }
            Ins::Not => {
                let expr = pop!()?;
                stack.push(Expr::Not(Box::new(expr)));
            }
            Ins::Equal => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Equal(Box::new((lhs, rhs))));
            }
            Ins::NotEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::NotEqual(Box::new((lhs, rhs))));
            }
            Ins::Greater => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Greater(Box::new((lhs, rhs))));
            }
            Ins::Less => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Less(Box::new((lhs, rhs))));
            }
            Ins::LessOrEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::LessOrEqual(Box::new((lhs, rhs))));
            }
            Ins::GreaterOrEqual => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::GreaterOrEqual(Box::new((lhs, rhs))));
            }
            Ins::Add => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Add(Box::new((lhs, rhs))));
            }
            Ins::Sub => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Sub(Box::new((lhs, rhs))));
            }
            Ins::Mul => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Mul(Box::new((lhs, rhs))));
            }
            Ins::Div => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::Div(Box::new((lhs, rhs))));
            }
            Ins::LogicalAnd => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::LogicalAnd(Box::new((lhs, rhs))));
            }
            Ins::LogicalOr => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::LogicalOr(Box::new((lhs, rhs))));
            }
            Ins::PopDiscard => {
                if pop!().is_err() {
                    out.stmts.push(Stmt::DecompileError(
                        off,
                        DecompileErrorKind::StackUnderflow,
                    ));
                }
            }
            Ins::In => {
                let list = pop!(:list)?;
                let value = pop!()?;
                stack.push(Expr::In(Box::new((value, list))));
            }
            Ins::DimArray2D(item_size, var) => {
                let swap = pop!()?;
                let max_x = pop!()?;
                let min_x = pop!()?;
                let max_y = pop!()?;
                let min_y = pop!()?;
                out.stmts.push(Stmt::DimArray {
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
                out.stmts.push(Stmt::RedimArray {
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
                out.stmts.push(Stmt::Assign(var, expr));
            }
            Ins::SetArrayItem(var) => {
                let value = pop!()?;
                let index = pop!()?;
                out.stmts.push(Stmt::SetArrayItem(var, index, value));
            }
            Ins::SetArrayItem2D(var) => {
                let value = pop!()?;
                let index_x = pop!()?;
                let index_y = pop!()?;
                out.stmts
                    .push(Stmt::SetArrayItem2D(var, index_y, index_x, value));
            }
            Ins::Inc(var) => {
                out.stmts.push(Stmt::Inc(var));
            }
            Ins::Dec(var) => {
                out.stmts.push(Stmt::Dec(var));
            }
            Ins::JumpIf(rel) => {
                #[allow(clippy::cast_sign_loss)]
                let target = decoder.pos().wrapping_add(rel as isize as usize);
                let expr = pop!()?;
                let expr = Expr::Not(Box::new(expr));
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
                out.stmts.push(Stmt::Assign(var, expr));
            }
            Ins::Sprintf(var) => {
                let mut args = pop!(:list)?;
                let first_arg = pop!()?;
                match &mut args {
                    Expr::List(xs) => xs.insert(0, first_arg),
                    _ => unreachable!(),
                }
                let format = pop!(:string)?;
                out.stmts.push(Stmt::Generic {
                    bytecode: bytearray![0xa4, 0xc2],
                    ins: &GenericIns {
                        name: Some("sprintf"),
                        args: &[GenericArg::String, GenericArg::Int, GenericArg::List],
                        returns_value: false,
                    },
                    args: vec![Expr::Variable(var), format, args],
                });
            }
            Ins::DimArray1DSimple(item_size, var) => {
                let max_x = pop!()?;
                out.stmts.push(Stmt::DimArray {
                    var,
                    item_size,
                    min_y: Expr::Number(0),
                    max_y: Expr::Number(0),
                    min_x: Expr::Number(0),
                    max_x,
                    swap: Expr::Number(2),
                });
            }
            Ins::DimArray2DSimple(item_size, var) => {
                let max_x = pop!()?;
                let max_y = pop!()?;
                out.stmts.push(Stmt::DimArray {
                    var,
                    item_size,
                    min_y: Expr::Number(0),
                    max_y,
                    min_x: Expr::Number(0),
                    max_x,
                    swap: Expr::Number(2),
                });
            }
            Ins::BitwiseAnd => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::BitwiseAnd(Box::new((lhs, rhs))));
            }
            Ins::BitwiseOr => {
                let rhs = pop!()?;
                let lhs = pop!()?;
                stack.push(Expr::BitwiseOr(Box::new((lhs, rhs))));
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
                    args.push(operand_to_expr(op));
                }
                args.reverse();
                if ins.returns_value {
                    stack.push(Expr::Call(bytecode, ins, args));
                } else {
                    out.stmts.push(Stmt::Generic {
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
        decoder.pos(),
        decoder.pos(),
        None,
        &mut stack,
        out,
    ))
}

fn operand_to_expr<'a>(operand: &Operand<'a>) -> Expr<'a> {
    match *operand {
        Operand::Byte(x) => Expr::Number(x.into()),
        Operand::I16(x) => Expr::Number(x.into()),
        Operand::I32(x) => Expr::Number(x),
        Operand::Var(v) => Expr::Variable(v),
        Operand::String(s) => Expr::String(s),
    }
}

fn finish_block<'a>(
    expected_exit: BlockExit,
    pos: usize,
    target: usize,
    expr: Option<Expr<'a>>,
    stack: &mut Vec<Expr<'a>>,
    out: &mut StmtBlock<'a>,
) -> Option<Expr<'a>> {
    while let Some(expr) = stack.pop() {
        out.stmts.push(Stmt::DecompileError(
            pos,
            DecompileErrorKind::StackOrphan(Box::new(expr)),
        ));
    }

    match expected_exit {
        BlockExit::Jump(expected_target) => {
            if target != expected_target || expr.is_some() {
                out.stmts.push(Stmt::DecompileError(
                    pos,
                    DecompileErrorKind::WrongBlockExit,
                ));
            }
            None
        }
        BlockExit::JumpUnless(expected_target) => {
            if target != expected_target {
                out.stmts.push(Stmt::DecompileError(
                    pos,
                    DecompileErrorKind::WrongBlockExit,
                ));
            }
            Some(expr.unwrap_or(Expr::DecompileError(
                pos,
                DecompileErrorKind::WrongBlockExit,
            )))
        }
    }
}

fn unfinished_block<'a>(pos: usize, expr: Option<Expr<'a>>, out: &mut StmtBlock<'a>) {
    if let Some(expr) = expr {
        out.stmts.push(Stmt::DecompileError(
            pos,
            DecompileErrorKind::StackOrphan(Box::new(expr)),
        ));
    }
    out.stmts.push(Stmt::DecompileError(
        pos,
        DecompileErrorKind::WrongBlockExit,
    ));
}

#[derive(Copy, Clone)]
enum BlockExit {
    Jump(usize),
    JumpUnless(usize),
}

fn pop_string<'a>(stack: &mut Vec<Expr>, string_stack: &mut Vec<&'a [u8]>) -> Option<Expr<'a>> {
    match stack.pop()? {
        Expr::Number(-1) => Some(Expr::String(string_stack.pop()?)),
        Expr::Number(var_id) => Some(Expr::Variable(Variable(var_id.try_into().ok()?))),
        Expr::Variable(var) => Some(Expr::Variable(var)),
        _ => None,
    }
}

fn pop_list<'a>(stack: &mut Vec<Expr<'a>>) -> Option<Expr<'a>> {
    let len = match stack.pop()? {
        Expr::Number(n) => n,
        _ => return None,
    };
    let mut list = Vec::with_capacity(len.try_into().ok()?);
    for _ in 0..len {
        list.push(stack.pop()?);
    }
    list.reverse();
    Some(Expr::List(list))
}
