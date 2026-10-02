//! A reentrant lock that serializes every entry into a state.
//!
//! The state owns one and locks it in the methods that mutate the state or
//! run a script, so two threads that share a state (through the C API, for
//! example) take turns instead of racing on the backend's `RefCell`s.
//!
//! The lock has to be reentrant because a host function called by a script
//! can call back into the same state, on the same thread, before the outer
//! call has returned.

use std::marker::PhantomData;
use std::sync::{Condvar, Mutex, PoisonError};
use std::thread::ThreadId;

/// A mutex that the owning thread can lock more than once.
pub(crate) struct ReentrantLock {
    /// The thread that holds the lock and how many times it locked it, or
    /// `None` when the lock is free.
    owner: Mutex<Option<(ThreadId, usize)>>,
    waiters: Condvar,
}

/// Unlocks its [`ReentrantLock`] when dropped.
///
/// This is intentionally neither `Send` nor `Sync`: moving it to another
/// thread would leave the lock owned by a thread that cannot release it.
pub(crate) struct ReentrantGuard<'a> {
    lock: &'a ReentrantLock,
    _not_send: PhantomData<*const ()>,
}

impl ReentrantLock {
    pub(crate) const fn new() -> Self {
        Self {
            owner: Mutex::new(None),
            waiters: Condvar::new(),
        }
    }

    /// Acquires the lock, blocking until no other thread holds it.
    ///
    /// The thread that already holds the lock acquires it again immediately,
    /// so a call that re-enters the state from a host function does not
    /// deadlock.
    pub(crate) fn lock(&self) -> ReentrantGuard<'_> {
        let me = std::thread::current().id();
        let mut owner = self.owner.lock().unwrap_or_else(PoisonError::into_inner);
        loop {
            match owner.as_mut() {
                None => {
                    *owner = Some((me, 1));
                    break;
                }
                Some((owner, count)) if *owner == me => {
                    *count += 1;
                    break;
                }
                Some(_) => {
                    owner = self
                        .waiters
                        .wait(owner)
                        .unwrap_or_else(PoisonError::into_inner);
                }
            }
        }
        ReentrantGuard {
            lock: self,
            _not_send: PhantomData,
        }
    }
}

impl Drop for ReentrantGuard<'_> {
    fn drop(&mut self) {
        let me = std::thread::current().id();
        let mut owner = self
            .lock
            .owner
            .lock()
            .unwrap_or_else(PoisonError::into_inner);
        let Some((holder, count)) = owner.as_mut() else {
            return;
        };
        debug_assert_eq!(
            *holder, me,
            "a guard was dropped by a thread that does not own it"
        );
        if *count > 1 {
            *count -= 1;
        } else {
            *owner = None;
        }
        drop(owner);
        self.lock.waiters.notify_one();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::mpsc::channel;
    use std::time::Duration;

    #[test]
    fn one_thread_can_lock_twice() {
        let lock = ReentrantLock::new();
        let first = lock.lock();
        let second = lock.lock();
        drop(first);
        drop(second);
        let third = lock.lock();
        drop(third);
    }

    #[test]
    fn another_thread_waits_until_the_lock_is_released() {
        let lock = Arc::new(ReentrantLock::new());
        let guard = lock.lock();
        let (started_tx, started_rx) = channel();
        let (acquired_tx, acquired_rx) = channel();
        let handle = {
            let lock = Arc::clone(&lock);
            std::thread::spawn(move || {
                started_tx.send(()).unwrap();
                let _guard = lock.lock();
                acquired_tx.send(()).unwrap();
            })
        };

        started_rx.recv().unwrap();
        assert!(
            acquired_rx
                .recv_timeout(Duration::from_millis(100))
                .is_err(),
            "the other thread acquired a lock that was still held"
        );

        drop(guard);
        acquired_rx.recv_timeout(Duration::from_secs(1)).unwrap();
        handle.join().unwrap();
    }

    #[test]
    fn waiting_threads_take_turns() {
        let lock = Arc::new(ReentrantLock::new());
        let concurrent = Arc::new(AtomicUsize::new(0));
        let max_concurrent = Arc::new(AtomicUsize::new(0));
        let mut handles = Vec::new();
        for _ in 0..4 {
            let lock = Arc::clone(&lock);
            let concurrent = Arc::clone(&concurrent);
            let max_concurrent = Arc::clone(&max_concurrent);
            handles.push(std::thread::spawn(move || {
                for _ in 0..20 {
                    let _guard = lock.lock();
                    let now = concurrent.fetch_add(1, Ordering::SeqCst) + 1;
                    max_concurrent.fetch_max(now, Ordering::SeqCst);
                    std::thread::yield_now();
                    concurrent.fetch_sub(1, Ordering::SeqCst);
                }
            }));
        }
        for handle in handles {
            handle.join().unwrap();
        }
        assert_eq!(max_concurrent.load(Ordering::SeqCst), 1);
    }
}
