
trait Runtime {
    fn spawn<T>(task: T);
}


struct TokioRuntime {
}

impl Runtime for TokioRuntime {
    fn spawn<T>(task: T) {

    }
}
