use std::cell::{RefCell, Ref, RefMut};
use std::collections::HashMap;
use std::convert::From;
use std::ops::Deref;
use std::mem::transmute;
use std::str::Chars;
use std::iter::Peekable;

static MALFORMED_LEAD_MSG: &'static str = "Malformed leading trivia";
static MALFORMED_TRAIL_MSG: &'static str = "Malformed trailing trivia";

use super::{StringData, TableData, Container, IndirectChild, FormattedValue, Value};

pub enum ValueRef<'a> {
    String(&'a StringNode),
    Table(TableRef<'a>),
}

impl<'a> ValueRef<'a> {
    #[doc(hidden)] pub
    fn _new_string<'x>(v: &'x FormattedValue) -> ValueRef<'a> {
        ValueRef::String(unsafe  { transmute(v) })
    }
}

pub enum ValueRefMut<'a> {
    String(&'a mut StringNode),
}

impl<'a> ValueRefMut<'a> {
    #[doc(hidden)] pub
    fn _new_string<'x>(v: &'x mut FormattedValue) -> ValueRefMut<'a> {
        ValueRefMut::String(unsafe  { transmute(v) })
    }
}

pub struct StringNode(FormattedValue);

impl StringNode {
    pub fn get(&self) -> &str {
        match self.0.value {
            Value::String(ref data) => &data.get(),
            _ => unreachable!()
        }
    }

    pub fn set(&mut self, s: String) {
        match &mut self.0.value {
            &mut Value::String(ref mut data) => data.set_checked(s),
            _ => unreachable!()
        }
    }

    pub fn markup(&self) -> &ValueMarkup {
        unsafe { transmute(self) }
    }

    pub fn markup_mut(&mut self) -> &mut ValueMarkup {
        unsafe { transmute(self) }
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

pub struct ValueMarkup(FormattedValue);

impl ValueMarkup {
    pub fn get_leading_trivia(&self) -> &str {
        &self.0.lead
    }

    pub fn set_leading_trivia(&mut self, s: String) {
        for c in s.chars() {
            match c {
                ' ' | '\t' => { },
                _ => panic!(MALFORMED_LEAD_MSG)
            }
        }
        self.0.lead = s;
    }

    pub fn get_trailing_trivia(&self) -> &str {
        &self.0.trail
    }

    pub fn set_trailing_trivia(&mut self, s: String) {
        ValueMarkup::check_trailing_trivia(&*s);
        self.0.trail = s;
    }

    fn check_trailing_trivia(s: &str) {
        let chars = s.chars().peekable();
        let chars = ValueMarkup::eat_ws(chars);
        ValueMarkup::check_comment_or_newline(chars);
    }

    fn eat_ws(mut chars: Peekable<Chars>) -> Peekable<Chars> {
        while chars.peek().is_some() {
            match *chars.peek().unwrap() {
                ' ' | '\t' => { },
                _ => break
            }
            chars.next();
        }
        chars
    }

    fn eat_comment(mut chars: Peekable<Chars>) -> Peekable<Chars> {
        if chars.peek() != Some(&'#') { return chars; }
        while chars.peek().is_some() {
            match *chars.peek().unwrap() {
                '\n' => break,
                '\r' => {
                    let before_cr = chars.clone();
                    chars.next();
                    if chars.peek() == Some(&'\n') {
                        return before_cr;
                    } else {
                        continue;
                    }
                },
                _ => { }
            }
            chars.next();
        }
        chars
    }

    fn check_comment_or_newline(mut chars: Peekable<Chars>) {
        chars = ValueMarkup::eat_comment(chars);
        // we are either at newline or end of string
        match chars.next().unwrap_or_else(|| panic!(MALFORMED_TRAIL_MSG)) {
            '\n' => assert!(chars.next().is_none(), MALFORMED_TRAIL_MSG),
            '\r' => {
                assert!(chars.next() == Some('\n'), MALFORMED_TRAIL_MSG);
                assert!(chars.next().is_none(), MALFORMED_TRAIL_MSG);
            }
            _ => panic!(MALFORMED_TRAIL_MSG)
        }
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