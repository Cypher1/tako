#![deny(clippy::all)]

pub mod ast;
#[cfg(feature = "backend")]
pub mod codegen;
pub mod compiler;
pub mod desugarer;
pub mod error;
pub mod interpreter;
pub mod lowerer;
pub mod parser;
pub mod primitives;
pub mod tasks;
#[cfg(test)]
pub mod test;
pub mod ui;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(test)]
use qbice::{Config, Decode, Encode, Executor, Identifiable, Query, StableHash, TrackedEngine};

use crate::compiler::Compiler;

static mut LOGS_UNINITIALISED: bool = true;

#[cfg(not(target_arch = "wasm32"))]
fn build_logger(finish: impl FnOnce(&mut env_logger::Builder)) {
    if unsafe { LOGS_UNINITIALISED } {
        unsafe {
            LOGS_UNINITIALISED = false;
        }
        finish(
            env_logger::Builder::from_env(
                env_logger::Env::default()
                    .filter_or("RUST_LOG", "debug")
                    .write_style_or("RUST_LOG_STYLE", "AUTO"),
            )
            .format_timestamp(None),
        );
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(test)]
pub fn ensure_initialized() {
    build_logger(|env| {
        let _ = env.is_test(true).try_init();
    });
}

#[cfg(target_arch = "wasm32")]
pub fn ensure_initialized() {
    if unsafe { LOGS_UNINITIALISED } {
        unsafe {
            LOGS_UNINITIALISED = false;
        }
        wasm_logger::init(wasm_logger::Config::new(log::Level::Trace));
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[cfg(not(test))]
pub fn ensure_initialized() {
    use std::fs::OpenOptions;
    build_logger(|env| {
        let log_file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(".tako.log")
            .expect("Failed to setup log file.");
        env_logger::Builder::init(env.target(env_logger::fmt::Target::Pipe(Box::new(log_file))));
    });
    build_logger(env_logger::Builder::init);
}

pub async fn start() -> Compiler {
    Compiler::default()
}

// Define query types
#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub enum Variable {
    A,
    B,
}

#[cfg(test)]
impl Query for Variable {
    type Value = i32;
}

#[cfg(test)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct SafeDivide {
    pub numerator: Variable,
    pub denominator: Variable,
}

#[cfg(test)]
impl Query for SafeDivide {
    type Value = Option<i32>;
}

// Define executor
#[cfg(test)]
struct SafeDivideExecutor;

#[cfg(test)]
impl<C: Config> Executor<SafeDivide, C> for SafeDivideExecutor {
    async fn execute(&self, query: &SafeDivide, engine: &TrackedEngine<C>) -> Option<i32> {
        let num = engine.query(&query.numerator).await;
        let denom = engine.query(&query.denominator).await;

        if denom == 0 {
            return None;
        }

        Some(num / denom)
    }
}

#[tokio::test]
async fn run_qbice() -> Result<(), Box<dyn std::error::Error>> {
    use std::sync::Arc;
    use std::path::Path;

    use qbice::{
        serialize::Plugin,
        stable_hash::{SeededStableHasherBuilder, Sip128Hasher},
        storage::{
            kv_database::rocksdb::RocksDB,
            storage_engine::db_backed::{Configuration, DbBackedFactory},
        },
        DefaultConfig, Engine, InputSession,
    };

    let dir = tempfile::tempdir()?;

    // Create and configure the engine
    let mut engine = Engine::<DefaultConfig>::new_with(
        Plugin::default(),
        DbBackedFactory::builder()
            .configuration(Configuration::builder().build())
            .db_factory(RocksDB::factory(dir.path()))
            .build(),
        SeededStableHasherBuilder::<Sip128Hasher>::new(0),
    )
    .await?;

    // Register executor
    engine.register_executor(Arc::new(SafeDivideExecutor));

    let engine = Arc::new(engine);

    // Set initial inputs
    {
        let mut input_session: InputSession<DefaultConfig> = engine.input_session().await;
        input_session.set_input(Variable::A, 42).await;
        input_session.set_input(Variable::B, 2).await;
        input_session.commit().await;
    }

    // Execute query
    let tracked_engine = engine.tracked().await;
    let result = tracked_engine
        .query(&SafeDivide {
            numerator: Variable::A,
            denominator: Variable::B,
        })
        .await;

    assert_eq!(result, Some(21));
    Ok(())
}
