use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;

fn main() {
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .grammar_in_src_dir("tako.y")
                .expect("tako.y failed to build")
        })
        .lexer_in_src_dir("tako.l")
        .expect("tako.l failed to build")
        .build()
        .expect("tako's parser failed to build");
}
