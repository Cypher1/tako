use qbice::{Executor, TrackedEngine};
use tokio::task::JoinSet;

use crate::parser::parse;

use super::*;

#[derive(Clone, Copy, Debug)]
pub struct LoadExecutor;

impl<C: qbice::Config> Executor<Load, C> for LoadExecutor {
    async fn execute(&self, query: &Load, _engine: &TrackedEngine<C>) -> Result<String, TError> {
        // TODO(correctness): Look at  notify::recommended_watcher
        // See `tako/src/main.rs` `WatchFileTask`
        match &query.file {
            FileRef::File(path) => Ok(std::fs::read_to_string(path)?),
            FileRef::InMemory(_path, contents) => Ok(contents.clone()),
            FileRef::Dependency {
                name,
                version,
                internal_path,
            } => {
                // Download the dependency if it's not already down
                // Load the file from there.
                todo!("{name}@{version} / {internal_path:?}");
            }
        }
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::ExternalInput
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LexExecutor;

impl<C: qbice::Config> Executor<Lex, C> for LexExecutor {
    async fn execute(&self, query: &Lex, engine: &TrackedEngine<C>) -> Result<Vec<Token>, TError> {
        let contents = engine
            .query(&Load {
                file: query.entry.clone(),
            })
            .await?;
        use crate::parser::tokens::lex;
        lex(&contents)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ParseFrontMatterExecutor;

impl<C: qbice::Config> Executor<ParseFrontMatter, C> for ParseFrontMatterExecutor {
    async fn execute(
        &self,
        query: &ParseFrontMatter,
        _engine: &TrackedEngine<C>,
    ) -> Result<Ast, TError> {
        /*
        let contents = engine.query(&Lex {
            entry: query.entry.clone(),
        }).await?;
        */
        let ast = Ast::new(query.entry.clone());
        Ok(ast)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ParseExecutor;

impl<C: qbice::Config> Executor<Parse, C> for ParseExecutor {
    async fn execute(&self, query: &Parse, engine: &TrackedEngine<C>) -> Result<Ast, TError> {
        // TODO(cleanup): Avoid depending on contents in parse.
        let contents = engine
            .query(&Load {
                file: query.entry.clone(),
            })
            .await?;
        let tokens = engine
            .query(&Lex {
                entry: query.entry.clone(),
            })
            .await?;
        let ast = engine
            .query(&ParseFrontMatter {
                entry: query.entry.clone(),
            })
            .await?;
        parse(&ast, &contents, &tokens)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct HandleImportExecutor;

impl<C: qbice::Config> Executor<HandleImport, C> for HandleImportExecutor {
    async fn execute(
        &self,
        query: &HandleImport,
        engine: &TrackedEngine<C>,
    ) -> Result<Ast, TError> {
        let ast = engine
            .query(&Parse {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Scope the WHOLE AST inside the name.
        Ok(ast) // Return it for merging with others.
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ResolveAstExecutor;

impl<C: qbice::Config> Executor<ResolveAst, C> for ResolveAstExecutor {
    async fn execute(&self, query: &ResolveAst, engine: &TrackedEngine<C>) -> Result<Ast, TError> {
        let ast = engine
            .query(&Parse {
                entry: query.entry.clone(),
            })
            .await?;
        let mut set = JoinSet::new();
        for (_, import) in ast.imports.iter() {
            let entry = query.entry.clone();
            let import = import.entry.clone();
            let engine = engine.clone();
            set.spawn(async move {
                let import_query = HandleImport { entry, import };
                engine.query(&import_query).await
            });
        }

        while let Some(res) = set.join_next().await {
            let _imported_ast = res.expect("JoinError while merging imports?");
            // TODO(correctness): Merge asts.
            // ast.merge(imported_ast);
        }
        Ok(ast) // Return it for merging with others.
    }
}
