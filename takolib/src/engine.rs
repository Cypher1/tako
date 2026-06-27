pub use crate::tasks::manager::{StatusReport, TaskStats};
pub use crate::tasks::status::*;
pub use crate::tasks::task_trait::TaskId;
#[cfg(feature = "fjall")]
#[cfg(not(feature = "rocksdb"))]
use qbice::storage::kv_database::fjall::Fjall;
#[cfg(feature = "fjall")]
#[cfg(not(feature = "rocksdb"))]
use qbice::storage::storage_engine::db_backed::{Configuration, DbBacked};
use qbice::storage::storage_engine::in_memory::InMemoryStorageEngine;
use qbice::Identifiable;
use std::fmt::Debug;

#[cfg(feature = "rocksdb")]
pub(crate) async fn get_qbice_engine() -> qbice::Engine<qbice::DefaultConfig> {
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

    Engine::<DefaultConfig>::new_with(
        Plugin::default(),
        DbBackedFactory::builder()
            .configuration(Configuration::builder().build())
            .db_factory(RocksDB::factory(dir.path()))
            .build(),
        SeededStableHasherBuilder::<Sip128Hasher>::new(0),
    )
    .await
    .expect("database initialization should never fail (rocks)")
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Identifiable)]
pub struct InMemoryDBConfig;

use qbice::stable_hash::{SeededStableHasherBuilder, Sip128Hasher};

impl qbice::config::Config for InMemoryDBConfig {
    type StorageEngine = InMemoryStorageEngine;

    type BuildStableHasher = SeededStableHasherBuilder<Sip128Hasher>;

    type BuildHasher = std::hash::BuildHasherDefault<fxhash::FxHasher>;
}

#[cfg(feature = "fjall")]
#[cfg(not(feature = "rocksdb"))]
#[derive(Debug, Default, Clone, Copy, Identifiable, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub(crate) struct FjallConfig;

#[cfg(feature = "fjall")]
#[cfg(not(feature = "rocksdb"))]
impl qbice::config::Config for FjallConfig {
    type StorageEngine = DbBacked<Fjall>;
    type BuildStableHasher =
        qbice::stable_hash::SeededStableHasherBuilder<qbice::stable_hash::Sip128Hasher>;
    type BuildHasher = fxhash::FxBuildHasher;
}

#[cfg(feature = "fjall")]
#[cfg(not(feature = "rocksdb"))]
pub(crate) async fn get_qbice_engine() -> qbice::Engine<FjallConfig> {
    use qbice::{
        serialize::Plugin,
        stable_hash::{SeededStableHasherBuilder, Sip128Hasher},
        storage::{kv_database::fjall::Fjall, storage_engine::db_backed::DbBackedFactory},
        Engine,
    };

    let dir = tempfile::tempdir().expect("Creating temp dir shouldn't fail");

    Engine::new_with(
        Plugin::default(),
        DbBackedFactory::builder()
            .configuration(Configuration::builder().build())
            .db_factory(Fjall::factory(dir.path()))
            .build(),
        SeededStableHasherBuilder::<Sip128Hasher>::new(0),
    )
    .await
    .expect("database initialization should never fail (fjall)")
}

#[cfg(not(feature = "fjall"))]
#[cfg(not(feature = "rocksdb"))]
pub(crate) async fn get_qbice_engine() -> qbice::Engine<InMemoryDBConfig> {
    use qbice::serialize::Plugin;
    qbice::Engine::<InMemoryDBConfig>::new_with(
        Plugin::default(),
        InMemoryStorageEngine,
        SeededStableHasherBuilder::<Sip128Hasher>::new(0),
    )
    .await
    .expect("database initialization should never fail (in memory)")
}
