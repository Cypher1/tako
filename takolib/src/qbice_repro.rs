use qbice::{Config, Decode, Encode, Executor, Identifiable, Query, StableHash, TrackedEngine};
#[cfg(test)]
use qbice::{DefaultConfig, Engine};
#[cfg(test)]
use std::sync::Arc;

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
struct SafeDivideExecutor;

impl<C: Config> Executor<SafeDivide, C> for SafeDivideExecutor {
    async fn execute(&self, query: &SafeDivide, engine: &TrackedEngine<C>) -> Option<i32> {
        let num = engine.query(&query.numerator).await;
        let denom = engine.query(&query.denominator).await;

        if denom == 0 {
            return None;
        }

        Some(num / denom)
    }

    // TODO: For others implement `execution_style`
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
    engine.register_executor(Arc::new(SafeDivideExecutor));

    //Compiler {
    // let compiler = crate::start().await;
    // Register executor

    let engine = Arc::new(engine);

    // Set initial inputs
    {
        let mut input_session: InputSession<DefaultConfig> = engine.input_session().await;
        input_session.set_input(Variable::A, 42).await;
        input_session.set_input(Variable::B, 2).await;
        input_session.commit().await;
    }

    // {
        // Execute query
        // NOTE: the tracked engine [currently] MUST be dropped before another can be started.
        let tracked_engine = engine.clone().tracked().await;
        let result = tracked_engine
            .query(&SafeDivide {
                numerator: Variable::A,
                denominator: Variable::B,
            })
            .await;

        assert_eq!(result, Some(21));
    // }

    // Set initial inputs
    {
        let mut input_session: InputSession<DefaultConfig> = engine.clone().input_session().await;
        input_session.set_input(Variable::A, 44).await;
        input_session.set_input(Variable::B, 2).await;
        input_session.commit().await;
    }

    {
        // Execute query
        let tracked_engine = engine.clone().tracked().await;
        let result = tracked_engine
            .query(&SafeDivide {
                numerator: Variable::A,
                denominator: Variable::B,
            })
            .await;

        assert_eq!(result, Some(22));
    }
    Ok(())
}
