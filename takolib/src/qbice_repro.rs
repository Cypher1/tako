use qbice::Config;
use qbice::Executor;
use qbice::TrackedEngine;
use qbice::{Decode, Encode, Identifiable, Query, StableHash};

// Define query types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub enum Variable {
    A,
    B,
}

impl Query for Variable {
    type Value = i32;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct SafeDivide {
    pub numerator: Variable,
    pub denominator: Variable,
}

impl Query for SafeDivide {
    type Value = Option<i32>;
}

// Define executor
pub struct SafeDivideExecutor;

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
    use qbice::InputSession;
    use qbice::{
        serialize::Plugin,
        stable_hash::{SeededStableHasherBuilder, Sip128Hasher},
        storage::{
            kv_database::rocksdb::RocksDB,
            storage_engine::db_backed::{Configuration, DbBackedFactory},
        },
        DefaultConfig, Engine,
    };
    use std::sync::Arc;

    let dir = tempfile::tempdir().expect("Creating temp dir shouldn't fail");

    // Create and configure the engine
    let mut engine = Engine::<DefaultConfig>::new_with(
        Plugin::default(),
        DbBackedFactory::builder()
            .configuration(Configuration::builder().build())
            .db_factory(RocksDB::factory(dir.path()))
            .build(),
        SeededStableHasherBuilder::<Sip128Hasher>::new(0),
    )
    .await
    .expect("Creating engine shouldn't fail");

    // Register executor
    engine.register_executor(Arc::new(SafeDivideExecutor));

    let engine = Arc::new(engine);

    // Set initial inputs
    let mut input_session: InputSession<DefaultConfig> = engine.clone().input_session().await;
    input_session.set_input(Variable::A, 42).await;
    input_session.set_input(Variable::B, 2).await;
    input_session.commit().await;

    // Execute query
    let tracked_engine1 = engine.clone().tracked().await;
    let result = tracked_engine1
        .query(&SafeDivide {
            numerator: Variable::A,
            denominator: Variable::B,
        })
        .await;

    assert_eq!(result, Some(21));

    // Execute query
    let tracked_engine2 = engine.clone().tracked().await;
    let result = tracked_engine2
        .query(&SafeDivide {
            numerator: Variable::A,
            denominator: Variable::B,
        })
        .await;

    assert_eq!(result, Some(21));

    Ok(())
}
