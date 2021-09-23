use specs::prelude::*;
use specs::{Entities, Entity, System};

pub struct MapSystem<'a, T, E> {
    f: &'a dyn Fn(Entity) -> Result<T, E>,
    pub results: Vec<T>,
    pub errors: Vec<E>,
}

impl<'a, T, E> MapSystem<'a, T, E> {
    pub fn new(f: &'a dyn Fn(Entity) -> Result<T, E>) -> Self {
        Self {
            f,
            results: Vec::new(),
            errors: Vec::new(),
        }
    }
}

impl<'a, T, E> System<'a> for MapSystem<'a, T, E> {
    type SystemData = Entities<'a>;

    fn run(&mut self, entities: Self::SystemData) {
        for ent in (&*entities).join() {
            match (self.f)(ent) {
                Ok(val) => self.results.push(val),
                Err(err) => self.errors.push(err),
            }
        }
    }
}
