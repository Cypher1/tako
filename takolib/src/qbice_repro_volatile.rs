#[cfg(test)]
use qbice::Config;
#[cfg(test)]
use qbice::Executor;
#[cfg(test)]
use qbice::TrackedEngine;
use qbice::{Decode, Encode, Identifiable, Query, StableHash};
#[cfg(test)]
use std::sync::Arc;

// Define query types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub enum Variable {
    A,
    B,
    C,
    D,
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
#[cfg(test)]
struct SafeDivideExecutor;

#[cfg(test)]
impl<C: Config> Executor<SafeDivide, C> for SafeDivideExecutor {
    async fn execute(&self, query: &SafeDivide, engine: &TrackedEngine<C>) -> Option<i32> {
        println!("{num:?}/{denom:?} => ?", num=query.numerator, denom=query.denominator);
        let numerator = engine.query(&query.numerator).await;
        let denominator = engine.query(&query.denominator).await;
        return engine.query(&SafeDivideNumeric{
            numerator,
            denominator,
        }).await;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, StableHash, Identifiable, Encode, Decode)]
pub struct SafeDivideNumeric {
    pub numerator: i32,
    pub denominator: i32,
}

impl Query for SafeDivideNumeric {
    type Value = Option<i32>;
}

#[cfg(test)]
struct SafeDivideNumericExecutor;

#[cfg(test)]
impl<C: Config> Executor<SafeDivideNumeric, C> for SafeDivideNumericExecutor {
    async fn execute(&self, query: &SafeDivideNumeric, _engine: &TrackedEngine<C>) -> Option<i32> {
        println!("  {num:?}/{denom:?} => ?", num=query.numerator, denom=query.denominator);
        let num = query.numerator;
        let denom = query.denominator;

        if denom == 0 {
            println!("    {num:?}/{denom:?} => INF");
            return None;
        }

        println!("    {num:?}/{denom:?} => {r}", r=num/denom);
        Some(num / denom)
    }

    // TODO: For others implement `execution_style`
}

#[cfg(test)]
pub struct GetVariableExecutor;
#[cfg(test)]
impl<C: Config> Executor<Variable, C> for GetVariableExecutor {
    async fn execute(&self, query: &Variable, _engine: &TrackedEngine<C>) -> i32 {
        // Get value from static memory (as if it were the file system)
        let r = match query {
            Variable::A => 42,
            Variable::B => 2,
            Variable::C => 44,
            Variable::D => 2,
        };
        println!("  {query:?} => {r}");
        r
    }

    fn execution_style() -> qbice::ExecutionStyle {
        qbice::ExecutionStyle::ExternalInput
    }
}

#[tokio::test]
async fn run_qbice() -> Result<(), Box<dyn std::error::Error>> {
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
    engine.register_executor(Arc::new(SafeDivideNumericExecutor));
    engine.register_executor(Arc::new(GetVariableExecutor));

    //Compiler {
    // let compiler = crate::start().await;
    // Register executor

    let engine = Arc::new(engine);

    // Set initial inputs
    // set_input(Variable::A, 42);
    // set_input(Variable::B, 2);

    let query1 = async move |engine: Arc<Engine<DefaultConfig>>| {
        // Execute query
        // NOTE: the tracked engine [currently] MUST be dropped before another can be started.
        let tracked_engine = engine.tracked().await;
        let result = tracked_engine
            .query(&SafeDivide {
                numerator: Variable::A,
                denominator: Variable::B,
            })
            .await;

        assert_eq!(result, Some(21));
    };

    // Set initial inputs
    // set_input(Variable::C, 44);
    // set_input(Variable::D, 2);

    let query2 = async move |engine: Arc<Engine<DefaultConfig>>| {
        // Execute query
        // NOTE: the tracked engine [currently] MUST be dropped before another can be started.
        let tracked_engine = engine.tracked().await;
        let result = tracked_engine
            .query(&SafeDivide {
                numerator: Variable::A,
                denominator: Variable::D,
            })
            .await;

        assert_eq!(result, Some(21));
    };

    // Queries run in definition order.
    let q1 = tokio::spawn(query1(engine.clone()));
    let q2 = tokio::spawn(query2(engine.clone()));

    let (q1, q2) = tokio::join!(q1, q2);
    q1?;
    q2?;
    Ok(())
}
