//! A thin wrapper around `ThreadPool` to make sure that we join all things
//! properly.

// From https://github.com/rust-analyzer/rust-analyzer/blob/7435b9e98c9280043605748c11a1f450669e04d6/crates/rust-analyzer/src/thread_pool.rs
use crossbeam_channel::Sender;

pub struct TaskPool<T> {
    sender: Sender<T>,
    inner: threadpool::ThreadPool,
}

impl<T> TaskPool<T> {
    pub fn new(sender: Sender<T>) -> TaskPool<T> {
        TaskPool { sender, inner: threadpool::ThreadPool::default() }
    }

    pub fn spawn<F>(&mut self, task: F)
    where
        F: FnOnce() -> T + Send + 'static,
        T: Send + 'static,
    {
        self.inner.execute({
            let sender = self.sender.clone();
            move || sender.send(task()).unwrap()
        })
    }

    pub fn spawn_with_sender<F>(&mut self, task: F)
    where
        F: FnOnce(Sender<T>) + Send + 'static,
        T: Send + 'static,
    {
        self.inner.execute({
            let sender = self.sender.clone();
            move || task(sender)
        })
    }

    pub fn len(&self) -> usize {
        self.inner.queued_count()
    }
}

impl<T> Drop for TaskPool<T> {
    fn drop(&mut self) {
        self.inner.join()
    }
}
