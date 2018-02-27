use std::sync::mpsc::Sender;
use std::sync::mpsc::Receiver;

#[derive(Clone, Debug, PartialEq)]
pub enum Direction {
    Left,
    Right,
}

pub trait Source<T> {
    fn take(&mut self) -> Option<T>;
}

impl<T> Source<T> for Vec<T> {
    fn take(&mut self) -> Option<T> {
        self.pop()
    }
}

impl<T> Source<T> for Receiver<T> {
    fn take(&mut self) -> Option<T> {
        self.recv().ok()
    }
}

pub trait Sink<T> {
    fn put(&mut self, thing: T);
}

impl<T> Sink<T> for Vec<T> {
    fn put(&mut self, thing: T) {
        self.push(thing);
    }
}

impl<T> Sink<T> for Sender<T> {
    fn put(&mut self, thing: T) {
        self.send(thing).unwrap();
    }
}
