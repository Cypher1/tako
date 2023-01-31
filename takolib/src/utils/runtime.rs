use std::future::Future;

#[cfg(target_arch = "wasm32")]
pub fn spawn<T: Send + Future<Output=()> + 'static>(task: T) where <T as Future>::Output: Send {
    wasm_bindgen_futures::spawn_local(task);
}

#[cfg(not(target_arch = "wasm32"))]
pub fn spawn<T: Send + Future<Output=()> + 'static>(task: T) where <T as Future>::Output: Send {
    tokio::spawn(task);
}
