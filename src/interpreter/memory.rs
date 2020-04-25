use std::collections::HashMap;

use crate::interpreter::value::Value;

#[derive(Debug)]
pub struct Memory {
    memory_frame: Vec<HashMap<String, Value>>
}

impl Memory {
    pub fn new() -> Self {
        Self {
            memory_frame: vec![HashMap::new()]
        }
    }

    pub fn create_frame(&mut self) {
        self.memory_frame.push(HashMap::new());
    }

    pub fn remove_frame(&mut self) {
        self.memory_frame.pop();
    }


    pub fn insert_value(&mut self, key: String, value: Value) {
        let mut current_frame = self.memory_frame.pop().unwrap();
        current_frame.insert(key.clone(), value);
        self.memory_frame.push(current_frame);
    }


    pub fn retrieve_value(&self, key: String) -> Option<Value> {
        for frame in self.memory_frame.iter().rev() {
            match frame.get(&key) {
                Some(value) => return Some(value.clone()),
                _ => ()
            }
        }
        None
    }
}