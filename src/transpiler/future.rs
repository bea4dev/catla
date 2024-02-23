use std::{mem, sync::{Arc, Mutex}};

use manual_future::{ManualFuture, ManualFutureCompleter};


pub(crate) struct SharedManualFuture<T> {
    value: Mutex<(Option<Arc<T>>, Vec<ManualFutureCompleter<Arc<T>>>)>
}

impl<T> SharedManualFuture<T> {
    
    pub fn new() -> SharedManualFuture<T> {
        return Self {
            value: Mutex::new((None, Vec::new()))
        };
    }

    pub fn get(&self) -> ManualFuture<Arc<T>> {
        let mut value = self.value.lock().unwrap();

        return match &value.0 {
            Some(value) => {
                ManualFuture::new_completed(value.clone())
            },
            _ => {
                let (future, completer) = ManualFuture::new();
                value.1.push(completer);
                future
            }
        };
    }

    pub async fn complete(&self, complete_value: T) {
        let mut value = self.value.lock().unwrap();

        if value.0.is_some() {
            panic!("double complete!");
        }

        let complete_value = Arc::new(complete_value);
        value.0 = Some(complete_value.clone());

        let mut completers = Vec::new();;
        mem::swap(&mut completers, &mut value.1);

        // unlock
        drop(value);
        
        for completer in completers {
            completer.complete(complete_value.clone()).await;
        }
    }

}