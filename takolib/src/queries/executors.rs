use qbice::{Executor, TrackedEngine};
use tokio::task::JoinSet;

use crate::ast::location::UserFacingLocation;
use crate::ast::NodeId;
use crate::parser::parse;
use crate::primitives::Prim;

use super::*;

#[derive(Clone, Copy, Debug)]
pub struct LoadExecutor;

impl<C: qbice::Config> Executor<Load, C> for LoadExecutor {
    async fn execute(&self, query: &Load, _engine: &TrackedEngine<C>) -> Result<String, TError> {
        // TODO(correctness): Look at  notify::recommended_watcher
        // See `tako/src/main.rs` `WatchFileTask`
        // TODO(perf): Use tokio's async read_to_string.
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
        let mut ast = engine
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
            let res = res.expect("JoinError while merging imports?");
            let imported_ast = res?;
            ast.merge(&imported_ast);
        }
        Ok(ast) // Return it for merging with others.
    }
}

#[derive(Clone, Copy, Debug)]
pub struct MacroExpandExecutor;

impl<C: qbice::Config> Executor<MacroExpand, C> for MacroExpandExecutor {
    async fn execute(&self, query: &MacroExpand, engine: &TrackedEngine<C>) -> Result<Ast, TError> {
        let ast = engine
            .query(&ResolveAst {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement macro expansion.
        Ok(ast)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FindNodeExecutor;

impl<C: qbice::Config> Executor<FindNode, C> for FindNodeExecutor {
    async fn execute(
        &self,
        query: &FindNode,
        engine: &TrackedEngine<C>,
    ) -> Result<(Ast, NodeId), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Node finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FindDefinitionExecutor;

impl<C: qbice::Config> Executor<FindDefinition, C> for FindDefinitionExecutor {
    async fn execute(
        &self,
        query: &FindDefinition,
        engine: &TrackedEngine<C>,
    ) -> Result<(Ast, NodeId), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct GetLocationExecutor;

impl<C: qbice::Config> Executor<GetLocation, C> for GetLocationExecutor {
    async fn execute(
        &self,
        query: &GetLocation,
        engine: &TrackedEngine<C>,
    ) -> Result<(FileRef, Location), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TypeAtExecutor;

impl<C: qbice::Config> Executor<TypeAt, C> for TypeAtExecutor {
    async fn execute(
        &self,
        query: &TypeAt,
        engine: &TrackedEngine<C>,
    ) -> Result<(Ast, NodeId), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TypeCheckExecutor;

impl<C: qbice::Config> Executor<TypeCheck, C> for TypeCheckExecutor {
    async fn execute(
        &self,
        query: &TypeCheck,
        engine: &TrackedEngine<C>,
    ) -> Result<(Ast, NodeId), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct GetTypeExecutor;

impl<C: qbice::Config> Executor<GetType, C> for GetTypeExecutor {
    async fn execute(
        &self,
        query: &GetType,
        engine: &TrackedEngine<C>,
    ) -> Result<(Ast, NodeId), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct CheckProofsExecutor;

impl<C: qbice::Config> Executor<CheckProofs, C> for CheckProofsExecutor {
    async fn execute(
        &self,
        query: &CheckProofs,
        engine: &TrackedEngine<C>,
    ) -> Result<(Ast, NodeId, Vec<TError>), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ErrorsExecutor;

impl<C: qbice::Config> Executor<Errors, C> for ErrorsExecutor {
    async fn execute(
        &self,
        query: &Errors,
        engine: &TrackedEngine<C>,
    ) -> BTreeMap<UserFacingLocation, Vec<Error>> {
        let contents = engine
            .query(&Load {
                file: query.file.clone(),
            })
            .await
            .ok();

        let maybe_proofs = engine
            .query(&CheckProofs {
                entry: query.file.clone(),
            })
            .await;

        let errors = match maybe_proofs {
            Err(err) => vec![err],
            Ok((_proofs_ast, _new_root, errors)) => errors,
        };

        let mut error_map: BTreeMap<UserFacingLocation, Vec<Error>> = BTreeMap::new();
        for source in errors {
            let location = UserFacingLocation::from(
                query.file.clone(),
                contents.as_deref(),
                source.location(),
            );
            let err = Error {
                source,
                location: Some(location.clone()),
            };
            error_map.entry(location).or_default().push(err);
        }
        error_map
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ErrorsAtExecutor;

impl<C: qbice::Config> Executor<ErrorsAt, C> for ErrorsAtExecutor {
    async fn execute(&self, query: &ErrorsAt, engine: &TrackedEngine<C>) -> Vec<Error> {
        let all_errors = engine
            .query(&Errors {
                file: query.file.clone(),
            })
            .await;
        all_errors.get(&query.location).cloned().unwrap_or(vec![])
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PrettyPrintExecutor;

impl<C: qbice::Config> Executor<PrettyPrint, C> for PrettyPrintExecutor {
    async fn execute(&self, query: &PrettyPrint, _engine: &TrackedEngine<C>) -> String {
        format!("{}", query.ast.pretty_node(query.root))
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct InterpretExecutor;

impl<C: qbice::Config> Executor<Interpret, C> for InterpretExecutor {
    async fn execute(&self, query: &Interpret, engine: &TrackedEngine<C>) -> Result<Prim, TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EvalExecutor;

impl<C: qbice::Config> Executor<Eval, C> for EvalExecutor {
    async fn execute(&self, query: &Eval, engine: &TrackedEngine<C>) -> Result<Prim, TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EvalNodeExecutor;

impl<C: qbice::Config> Executor<EvalNode, C> for EvalNodeExecutor {
    async fn execute(&self, query: &EvalNode, engine: &TrackedEngine<C>) -> Result<Prim, TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.eval(query.???)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct OptimizeExecutor;

impl<C: qbice::Config> Executor<Optimize, C> for OptimizeExecutor {
    async fn execute(
        &self,
        query: &Optimize,
        engine: &TrackedEngine<C>,
    ) -> Result<(Ast, NodeId), TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement Definition finding.
        todo!("Something like ast.get_at_location(query.location)?;")
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[derive(Clone, Copy, Debug)]
pub struct LowerExecutor;

impl<C: qbice::Config> Executor<Lower, C> for LowerExecutor {
    async fn execute(&self, query: &Lower, engine: &TrackedEngine<C>) -> Result<Ast, TError> {
        let _ast = engine
            .query(&MacroExpand {
                entry: query.entry.clone(),
            })
            .await?;
        // TODO(correctness): Implement  Lowering.
        Ok(_ast)
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::Projection
    }
}

#[cfg(feature = "codegen")]
mod codegen {

    #[derive(Clone, Copy, Debug)]
    pub struct CodeGenAllExecutor;

    impl<C: qbice::Config> Executor<CodeGenAll, C> for CodeGenAllExecutor {
        async fn execute(
            &self,
            query: &CodeGenAll,
            engine: &TrackedEngine<C>,
        ) -> Result<(Ast, NodeId), TError> {
            let _ast = engine
                .query(&MacroExpand {
                    entry: query.entry.clone(),
                })
                .await?;
            // TODO(correctness): Implement Definition finding.
            todo!("Something like ast.get_at_location(query.location)?;")
        }

        fn execution_style() -> qbice::ExecutionStyle {
            qbice::ExecutionStyle::Projection
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct WriteCodeGenAllExecutor;

    impl<C: qbice::Config> Executor<WriteCodeGenAll, C> for WriteCodeGenAllExecutor {
        async fn execute(
            &self,
            query: &WriteCodeGenAll,
            engine: &TrackedEngine<C>,
        ) -> Result<(Ast, NodeId), TError> {
            let _ast = engine
                .query(&MacroExpand {
                    entry: query.entry.clone(),
                })
                .await?;
            // TODO(correctness): Implement Definition finding.
            todo!("Something like ast.get_at_location(query.location)?;")
        }

        fn execution_style() -> qbice::ExecutionStyle {
            qbice::ExecutionStyle::Projection
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct EnumerateBinariesExecutor;

    impl<C: qbice::Config> Executor<EnumerateBinaries, C> for EnumerateBinariesExecutor {
        async fn execute(
            &self,
            query: &EnumerateBinaries,
            engine: &TrackedEngine<C>,
        ) -> Result<(Ast, NodeId), TError> {
            let _ast = engine
                .query(&MacroExpand {
                    entry: query.entry.clone(),
                })
                .await?;
            // TODO(correctness): Implement Definition finding.
            todo!("Something like ast.get_at_location(query.location)?;")
        }

        fn execution_style() -> qbice::ExecutionStyle {
            qbice::ExecutionStyle::Projection
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct WriteCodeGenAllExecutor;

    impl<C: qbice::Config> Executor<WriteCodeGenAll, C> for WriteCodeGenAllExecutor {
        async fn execute(
            &self,
            query: &WriteCodeGenAll,
            engine: &TrackedEngine<C>,
        ) -> Result<(Ast, NodeId), TError> {
            let _ast = engine
                .query(&MacroExpand {
                    entry: query.entry.clone(),
                })
                .await?;
            // TODO(correctness): Implement Definition finding.
            todo!("Something like ast.get_at_location(query.location)?;")
        }

        fn execution_style() -> qbice::ExecutionStyle {
            qbice::ExecutionStyle::Projection
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct CodeGenExecutor;

    impl<C: qbice::Config> Executor<CodeGen, C> for CodeGenExecutor {
        async fn execute(
            &self,
            query: &CodeGen,
            engine: &TrackedEngine<C>,
        ) -> Result<(Ast, NodeId), TError> {
            let _ast = engine
                .query(&MacroExpand {
                    entry: query.entry.clone(),
                })
                .await?;
            // TODO(correctness): Implement Definition finding.
            todo!("Something like ast.get_at_location(query.location)?;")
        }

        fn execution_style() -> qbice::ExecutionStyle {
            qbice::ExecutionStyle::Projection
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct SourceMapGenExecutor;

    impl<C: qbice::Config> Executor<SourceMapGen, C> for SourceMapGenExecutor {
        async fn execute(
            &self,
            query: &SourceMapGen,
            engine: &TrackedEngine<C>,
        ) -> Result<(Ast, NodeId), TError> {
            let _ast = engine
                .query(&MacroExpand {
                    entry: query.entry.clone(),
                })
                .await?;
            // TODO(correctness): Implement Definition finding.
            todo!("Something like ast.get_at_location(query.location)?;")
        }

        fn execution_style() -> qbice::ExecutionStyle {
            qbice::ExecutionStyle::Projection
        }
    }
}
