use crate::{
    blocks::BlockId,
    compiler::{
        ast::{Ast, AstNode, AstNodeId, RawBlockContainer, RawBlockFile},
        errors::{CompileError, CompileErrorPayload},
        lexer::{string_contents, substr, Lexer},
        loc::FileId,
        token::{TokenKind, TokenPayload},
    },
};

pub fn parse_room(file: FileId, src: &str) -> Result<Ast, CompileError> {
    let mut ast = Ast {
        nodes: Vec::with_capacity(256),
        lists: Vec::with_capacity(256),
        string_table: String::with_capacity(256),
        root_start: 0,
        root_len: 0,
    };
    let mut scratch = Vec::with_capacity(256);
    let scratch_start = 0;
    let mut lexer = Lexer::new(file, src);
    loop {
        let token = lexer.next()?;
        match token.payload {
            TokenPayload::Eof => break,
            TokenPayload::Ident { len } if substr(src, token.offset, len) == "raw-block" => {
                let node_id = parse_raw_block(&mut lexer, &mut ast, &mut scratch)?;
                scratch.push(node_id);
            }
            _ => {
                return Err(CompileError::new(
                    file.at(token.offset),
                    CompileErrorPayload::UnexpectedToken {
                        expected: "raw-block or eof",
                    },
                ));
            }
        }
    }
    let (root_start, root_len) = commit_scratch(&mut ast, &mut scratch, scratch_start);
    ast.root_start = root_start;
    ast.root_len = root_len;
    Ok(ast)
}

fn parse_raw_block(
    lexer: &mut Lexer,
    ast: &mut Ast,
    scratch: &mut Vec<AstNodeId>,
) -> Result<AstNodeId, CompileError> {
    let (block_id_offset, block_id_len) = lexer.expect_string()?;
    let block_id = parse_block_id(lexer, block_id_offset, block_id_len)?;

    let token = lexer.next()?;
    match token.payload {
        TokenPayload::String { len } => {
            parse_raw_block_file(lexer, block_id, token.offset, len, ast)
        }
        TokenPayload::BraceL => parse_raw_block_container(lexer, block_id, ast, scratch),
        _ => {
            Err(CompileError::new(
                lexer.file.at(token.offset),
                CompileErrorPayload::UnexpectedToken {
                    expected: "string or '{'",
                },
            ))
        }
    }
}

fn parse_raw_block_file(
    lexer: &mut Lexer,
    block_id: BlockId,
    path_token_offset: u32,
    path_token_len: u32,
    ast: &mut Ast,
) -> Result<AstNodeId, CompileError> {
    lexer.expect_token(TokenKind::Newline)?;
    let (path_offset, path_len) = parse_string(lexer, path_token_offset, path_token_len, ast);
    let result: AstNodeId = ast.nodes.len().try_into().unwrap();
    ast.nodes.push(AstNode::RawBlockFile(RawBlockFile {
        block_id,
        path_loc: lexer.file.at(path_token_offset),
        path_offset,
        path_len,
    }));
    Ok(result)
}

fn parse_raw_block_container(
    lexer: &mut Lexer,
    block_id: BlockId,
    ast: &mut Ast,
    scratch: &mut Vec<AstNodeId>,
) -> Result<AstNodeId, CompileError> {
    lexer.expect_token(TokenKind::Newline)?;
    let scratch_start = scratch.len();
    loop {
        let token = lexer.next()?;
        match token.payload {
            TokenPayload::BraceR => {
                lexer.expect_token(TokenKind::Newline)?;
                break;
            }
            TokenPayload::Ident { len }
                if substr(lexer.source, token.offset, len) == "raw-block" =>
            {
                let node_id = parse_raw_block(lexer, ast, scratch)?;
                scratch.push(node_id);
            }
            _ => {
                return Err(CompileError::new(
                    lexer.file.at(token.offset),
                    CompileErrorPayload::UnexpectedToken {
                        expected: "'}' or raw-block",
                    },
                ));
            }
        }
    }

    let (children_start, children_len) = commit_scratch(ast, scratch, scratch_start);
    let node_id: AstNodeId = ast.nodes.len().try_into().unwrap();
    ast.nodes
        .push(AstNode::RawBlockContainer(RawBlockContainer {
            block_id,
            children_start,
            children_len,
        }));
    Ok(node_id)
}

fn parse_string(lexer: &Lexer, offset: u32, len: u32, ast: &mut Ast) -> (u32, u32) {
    let string = string_contents(lexer.source, offset, len);
    let offset: u32 = ast.string_table.len().try_into().unwrap();
    let len: u32 = string.len().try_into().unwrap();
    ast.string_table.push_str(string);
    (offset, len)
}

fn parse_block_id(lexer: &Lexer, offset: u32, len: u32) -> Result<BlockId, CompileError> {
    let s = string_contents(lexer.source, offset, len);
    if !(s.len() == 4 && s.bytes().all(|b| (32..=126).contains(&b))) {
        return Err(CompileError::new(
            lexer.file.at(offset),
            CompileErrorPayload::InvalidBlockId,
        ));
    }
    Ok(s.as_bytes().try_into().unwrap())
}

fn commit_scratch(ast: &mut Ast, scratch: &mut Vec<AstNodeId>, scratch_start: usize) -> (u32, u32) {
    let list_start: u32 = ast.lists.len().try_into().unwrap();
    let list_len: u32 = (scratch.len() - scratch_start).try_into().unwrap();
    ast.lists.extend(scratch.drain(scratch_start..));
    (list_start, list_len)
}
