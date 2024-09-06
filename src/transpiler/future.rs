use std::{mem, sync::{atomic::{AtomicUsize, Ordering}, Arc, Mutex}};

use manual_future::{ManualFuture, ManualFutureCompleter};


pub struct SharedManualFuture<T: Send> {
    value: Mutex<(Option<Arc<T>>, Vec<ManualFutureCompleter<Arc<T>>>)>
}

impl<T: Send> SharedManualFuture<T> {
    
    pub fn new() -> SharedManualFuture<T> {
        Self { value: Mutex::new((None, Vec::new())) }
    }

    pub fn new_completed(value: T) -> Self {
        Self { value: Mutex::new((Some(Arc::new(value)), Vec::new())) }
    }

    pub fn get(&self) -> ManualFuture<Arc<T>> {
        let mut value = self.value.lock().unwrap();

        match &value.0 {
            Some(value) => {
                ManualFuture::new_completed(value.clone())
            },
            _ => {
                let (future, completer) = ManualFuture::new();
                value.1.push(completer);
                future
            }
        }
    }

    pub async fn complete(&self, complete_value: Arc<T>) {
        let (arc_complete_value, completers) = {
            let mut value = self.value.lock().unwrap();

            if value.0.is_some() {
                panic!("double complete!");
            }

            value.0 = Some(complete_value.clone());

            let mut completers = Vec::new();
            mem::swap(&mut completers, &mut value.1);

            (complete_value, completers)
        };
        
        for completer in completers {
            completer.complete(arc_complete_value.clone()).await;
        }
    }

}


pub struct MultiTaskFuture {
    task_count: AtomicUsize,
    future: SharedManualFuture<()>
}

impl MultiTaskFuture {
    
    pub fn new() -> Self {
        Self {
            task_count: AtomicUsize::new(0),
            future: SharedManualFuture::new()
        }
    }

    pub fn mark_as_running(&self) {
        self.task_count.fetch_add(1, Ordering::Relaxed);
    }

    pub async fn mark_as_finished(&self) {
        if self.task_count.fetch_sub(1, Ordering::Relaxed) == 1 {
            self.future.complete(Arc::new(())).await;
        }
    }

    pub fn get_future(&self) -> ManualFuture<Arc<()>> {
        self.future.get()
    }

}

