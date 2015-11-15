use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;
use std::convert::From;
use std::ops::Deref;

use super::{StringData, TableData, Container, IndirectChild, FormattedValue, Value};

pub enum ValueRef<'a> {
    String(StringRef<'a>),
    Table(TableRef<'a>),
}

pub enum ValueRefMut<'a> {
    String(StringRefMut<'a>),
}

pub struct StringRef<'a>(#[doc(hidden)] pub Ref<'a, FormattedValue>);

impl<'a> StringRef<'a> {
    pub fn get(&self) -> &str {
        match self.0.value {
            Value::String(ref data) => &data.get(),
            _ => unreachable!()
        }
    }

    pub fn markup(&self) -> ValueMarkup {
        ValueMarkup(&self.0)
    }
}

pub struct StringRefMut<'a>(#[doc(hidden)] pub &'a RefCell<FormattedValue>);

impl<'a> StringRefMut<'a> {
    pub fn as_ref(&self) -> StringRef {
        StringRef(self.0.borrow())
    }

    pub fn set(&mut self, s: String) {
        match &mut self.0.borrow_mut().value {
            &mut Value::String(ref mut data) => data.set_checked(s),
            _ => unreachable!()
        }
    }
}

impl<'a, 'b> From<&'a StringRefMut<'b>> for StringRef<'a> {
    fn from(t: &'a StringRefMut<'b>) -> StringRef<'a> {
        t.as_ref()
    }
}


pub struct TableRef<'a> {
    data: Table<'a>
}

enum Table<'a> {
    Inline(&'a TableData),
    Implicit(&'a HashMap<String, IndirectChild>),
    Explicit(&'a Container)
}

pub struct ValueMarkup<'a>(#[doc(hidden)] &'a FormattedValue);
impl<'a> ValueMarkup<'a> {
    pub fn get_leading_trivia(&self) -> &str {
        &self.0.lead
    }

    pub fn get_trailing_trivia(&self) -> &str {
        &self.0.trail
    }

    pub fn get_unescaped_value(&self) -> Option<&str> {
        match self.0.value {
            Value::String(ref data) => Some(&data.raw),
            Value::Integer { ref raw, .. } => Some(&raw),
            Value::Float { ref raw, .. } => Some(&raw),
            Value::Datetime(ref val) => None,
            Value::Boolean(_) => None,
            Value::Array { .. } => None,
            Value::InlineTable(_) => None,
        }
    }
}