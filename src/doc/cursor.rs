use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;
use std::convert::From;
use std::ops::Deref;
use std::mem::transmute;
use std::str::Chars;
use std::iter::Peekable;

use super::{check_ws, eat_before_newline, eat_newline, transmute_lifetime_mut};
use super::{MALFORMED_LEAD_MSG, MALFORMED_TRAIL_MSG};

use super::{StringData, TableData, Container, IndirectChild, FormattedValue};
use super::{Value, ValueNode, ValueMarkup, FormattedKey};

pub enum ValueRef<'a> {
    String(&'a StringNode),
    Table(TableRef<'a>),
}

impl<'a> ValueRef<'a> {
    #[doc(hidden)] pub
    fn _new_string<'x>(v: &'x ValueNode) -> ValueRef<'a> {
        ValueRef::String(unsafe  { transmute(v) })
    }
}

pub enum ValueRefMut<'a> {
    String(&'a mut StringNode),
    Table(TableRefMut<'a>),
}

impl<'a> ValueRefMut<'a> {
    #[doc(hidden)] pub
    fn _new_string<'x>(v: &'x mut ValueNode) -> ValueRefMut<'a> {
        ValueRefMut::String(unsafe  { transmute(v) })
    }
}

pub struct StringNode(ValueNode);

impl StringNode {
    pub fn get(&self) -> &str {
        match self.0.value.value {
            Value::String(ref data) => &data.get(),
            _ => unreachable!()
        }
    }

    pub fn set(&mut self, s: String) {
        match &mut self.0.value.value {
            &mut Value::String(ref mut data) => data.set_checked(s),
            _ => unreachable!()
        }
    }
    pub fn markup(&self) -> &ValueMarkup {
        &self.0.value.markup
    }

    pub fn markup_mut(&mut self) -> &mut ValueMarkup {
        &mut self.0.value.markup
    }
    pub fn key(&self) -> &KeyMarkup {
        unsafe{ transmute(&self.0.key) }
    }

    pub fn key_mut(&mut self) -> &mut KeyMarkup {
        unsafe{ transmute(&mut self.0.key) }
    }

    pub fn get_raw(&self) -> &str {
        match self.0.value.value {
            Value::String(ref data) => &data.raw,
            _ => unreachable!()
        }
    }
}

pub struct KeyMarkup(FormattedKey);

impl KeyMarkup {
    pub fn get_leading_trivia(&self) -> &str {
        &self.0.markup.lead
    }

    pub fn set_leading_trivia(&mut self, s: String) {
        KeyMarkup::check_leading_trivia(&*s);
        self.0.markup.lead = s;
    }

    fn check_leading_trivia(s: &str) {
        let mut chars = s.chars().peekable();
        loop {
            let c  = chars.next();
            match c {
                Some(c) => { match c {
                    ' ' | '\t' => { },
                    '#' => { 
                        chars = eat_before_newline(chars);
                        chars = eat_newline(chars, MALFORMED_LEAD_MSG);
                    },
                    _ => panic!(super::MALFORMED_LEAD_MSG)
                }}
                None => break,
            }
        }
    }

    pub fn get_trailing_trivia(&self) -> &str {
        &self.0.markup.trail
    }

    pub fn set_trailing_trivia(&mut self, s: String) {
        check_ws(&*s, super::MALFORMED_TRAIL_MSG);
        self.0.markup.trail = s;
    }

    pub fn get_raw(&self) -> &str {
        match self.0.raw {
            Some(ref raw) => &*raw,
            None => &*self.0.escaped
        }
    }
}

pub struct TableRef<'a> {
    #[doc(hidden)] pub
    data: Table<'a>
}

pub enum Table<'a> {
    Inline(&'a TableData),
    Implicit(&'a HashMap<String, IndirectChild>),
    Explicit(Ref<'a, Container>)
}

impl<'a> TableRef<'a> {
    pub fn get(&self, key: &str) -> Option<ValueRef> {
        match self.data {
            Table::Inline(ref data) => data.values.get(key),
            Table::Implicit(ref map) => map.get(key).map(|c| c.as_cursor()),
            Table::Explicit(ref data) => data.data.get(key),
        }
    }

    pub fn len(&self) -> usize {
        match self.data {
            Table::Inline(ref data) => data.values.len(),
            Table::Implicit(ref map) => map.len(),
            Table::Explicit(ref data) => data.data.len(),
        }
    }
}

pub struct TableRefMut<'a> {
    #[doc(hidden)] pub
    data: TableMut<'a>
}

pub enum TableMut<'a> {
    Inline(&'a mut TableData),
    Implicit(&'a mut HashMap<String, IndirectChild>),
    Explicit(&'a RefCell<Container>)
}

impl<'a> TableRefMut<'a> {
    pub fn as_ref<'b>(&'b self) -> TableRef<'b> {
        let inner = match self.data {
            TableMut::Inline(ref data) => Table::Inline(data),
            TableMut::Implicit(ref map) => Table::Implicit(&*map),
            TableMut::Explicit(ref data) => Table::Explicit(data.borrow()),
        };
        TableRef { data: inner }
    }

    pub fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
        match self.data {
            TableMut::Inline(ref mut data) => data.values.get_mut(key),
            TableMut::Implicit(ref mut map) => {
                 map.get_mut(key).map(|c| c.as_cursor_mut())
            }
            TableMut::Explicit(ref mut data) => {
                let mut container = unsafe { 
                    transmute_lifetime_mut(&mut data.borrow_mut().data)
                };
                container.get_mut(key)
            }
        }
    }
}