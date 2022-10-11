use crate::{
    config::Config,
    script::{
        ast::{
            get_script_config,
            write_block,
            write_preamble,
            DecompileErrorKind,
            Scope,
            Scripto,
            Stmt,
            StmtBlock,
            WriteCx,
        },
        basic::find_basic_blocks,
        cases::build_cases,
        control::build_control_structures,
        goto::add_labels_for_gotos,
        ins::Variable,
        peep::peep,
        statements::build_ast,
        types::spread_types,
        visit::Visitor,
    },
};

pub fn decompile(code: &[u8], scope: Scope, config: &Config) -> String {
    if code.is_empty() {
        return String::new();
    }

    let cx = WriteCx { scope, config };

    let (blocks, decode_extent) = find_basic_blocks(code);
    let controls = build_control_structures(&blocks, &cx);
    let (mut script, mut root) = build_ast(&controls, code);
    peep(&script, &mut root);
    build_cases(&script, &mut root);
    add_labels_for_gotos(&mut script, &mut root);
    spread_types(&mut script, &mut root, scope, config);

    if decode_extent != code.len() {
        root.push(
            decode_extent,
            Stmt::DecompileError(
                decode_extent,
                DecompileErrorKind::Other("incomplete decode"),
            ),
        );
    }

    let locals = collect_locals(&mut script, &mut root, scope, config);

    let mut output = String::with_capacity(1024);
    if !config.suppress_preamble {
        write_preamble(&mut output, &locals, &cx).unwrap();
    }
    write_block(&mut output, &script, &root, 0, &cx).unwrap();
    output
}

fn collect_locals(
    script: &mut Scripto,
    block: &mut StmtBlock,
    scope: Scope,
    config: &Config,
) -> Vec<Variable> {
    struct CollectLocals {
        out: Vec<Variable>,
    }

    impl Visitor for CollectLocals {
        fn var(&mut self, var: Variable) {
            if applies(var) {
                self.out.push(var);
            }
        }
    }

    fn applies(var: Variable) -> bool {
        let scope = var.0 & 0xf000;
        scope == 0x4000
    }

    let mut out = Vec::with_capacity(16);

    // If there are params, treat them as always used, so they're always emitted
    if let Some(script) = get_script_config(&WriteCx { scope, config }) {
        if let Some(params) = script.params {
            out.extend((0..params).map(|n| Variable(0x4000 | n)));
        }
    }

    let mut locals = CollectLocals { out };
    locals.block(script, block);

    locals.out.sort_unstable_by_key(|v| v.0);
    locals.out.dedup_by_key(|v| v.0);
    locals.out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::read_scrp;
    use std::error::Error;

    #[test]
    fn basic_if() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(&bytecode[0x23c..0x266], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_starts_with(
            &out,
            r#"if (read-ini-int "NoPrinting") {
    global449 = 1
}
"#,
        );
        Ok(())
    }

    #[test]
    fn basic_if_else() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(&bytecode[0x5ba..0x5d4], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_starts_with(
            &out,
            r#"if (!global414) {
    global414 = 1
} else {
    global414 = 0
}
"#,
        );
        Ok(())
    }

    #[test]
    fn if_with_empty_body() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(19)?;
        let out = decompile(&bytecode[0x6d..0x76], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_eq!(
            out,
            "if (global229) {
}
",
        );
        Ok(())
    }

    #[test]
    fn if_else_if() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(406)?;
        let out = decompile(&bytecode[0x7c..0xbd], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_eq!(
            out,
            "if (local1 == 26) {
    local4 = call-script 236 [local3]
} else if (local1 in [23, 24]) {
    local4 = call-script 235 [local3, local1]
} else {
    local4 = global315[local3][local1]
}
",
        );
        Ok(())
    }

    #[test]
    fn basic_while() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(&bytecode[0x1b2..0x1d1], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_starts_with(
            &out,
            r#"while (local1 <= global105) {
    global220[local1] = local1
    ++local1
}
"#,
        );
        Ok(())
    }

    #[test]
    fn case_eq() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let out = decompile(&bytecode[0x501..0x542], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_starts_with(
            &out,
            r#"case global215 {
    of 2 {
        global90[0] = 70
    }
    of 1 {
        global90[0] = 77
    }
    of 0 {
        global90[0] = 83
    }
}
"#,
        );
        Ok(())
    }

    #[test]
    fn case_range() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(415)?;
        let out = decompile(&bytecode[0x1e..0xa6], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_starts_with(
            &out,
            r#"case local1 {
    in [1, 3, 5] {
        local4 = 350
    }
    in [2, 4, 6] {
        local4 = 351
    }
    in [7, 9, 11] {
        local4 = 356
    }
    in [8, 10, 12] {
        local4 = 357
    }
    else {
        xb6-xfe
        xb6-x4b "Invalid kid code"
        local4 = 350
        pop-discard 0
    }
}
"#,
        );
        Ok(())
    }

    #[test]
    fn do_until() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(46)?;
        let out = decompile(&bytecode[0xc9..0xde], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_eq!(
            out,
            "do {
    local13 = local0[local7]
    --local7
} until (local13 == 32)
",
        );
        Ok(())
    }

    #[test]
    fn do_until_containing_if() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(337)?;
        let out = decompile(&bytecode[0x128..0x145], Scope::Global(1), &Config {
            suppress_preamble: true,
            ..<_>::default()
        });
        assert_eq!(
            out,
            "do {
    if (!global577) {
        global577 = global677
    } else {
        global677 = global577
    }
    stop-script
} until (global577)
",
        );
        Ok(())
    }

    #[test]
    fn call_scripts_by_name() -> Result<(), Box<dyn Error>> {
        let bytecode = read_scrp(1)?;
        let mut config = Config::from_ini("script.80 = test")?;
        config.suppress_preamble = true;
        let out = decompile(&bytecode[0x5ce..0x5d4], Scope::Global(1), &config);
        assert_eq!(out, "run-script test{80} []\n");
        Ok(())
    }

    fn assert_starts_with(string: &str, prefix: &str) {
        assert!(
            string.starts_with(prefix),
            "assertion failed: {:?} starts with {:?}",
            string,
            prefix
        );
    }
}
