use std::fmt::{Display, Error, Formatter, Write};

use super::{Document, KeyMarkup, StringValue, TableKeyMarkup, BoolValue};
use super::{ValueRef, Container, DirectChild, InlineArray, FloatValue};
use super::{ContainerKind, InlineTable, IntegerValue, DatetimeValue};

fn fmt_join<'a, T, I>(f: &mut Formatter, values: I, sep: &str)
                      -> Result<(), Error> where T: Display, I:Iterator<Item=T>{
    let mut values = values.peekable();
    loop {
        let value = values.next();
        match value {
            Some(value) => {
                try!(write!(f, "{}", value));
                if values.peek().is_some() {
                    try!(write!(f, "{}", sep));
                }
            }
            None => break,
        }
    }
    Ok(())
}

impl Display for Document { 
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        fmt_join(f, self.iter_children(), "")
        .and_then(|_| fmt_join(f, self.iter_containers(), ""))
        .and_then(|_| write!(f, "{}", self.get_trailing_trivia()))
    }
}

impl Display for DirectChild {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}={}", self.key(), self.value())
    }
}

impl<'a> Display for ValueRef<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            ValueRef::String(node) => node.fmt(f),
            ValueRef::Integer(node) => node.fmt(f),
            ValueRef::Float(node) => node.fmt(f),
            ValueRef::Boolean(node) => node.fmt(f),
            ValueRef::Datetime(node) => node.fmt(f),
            ValueRef::Array(arr) => arr.fmt(f),
            ValueRef::Table(table) => table.fmt(f),
        }
    }
}

impl Display for StringValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f,
               "{}{}{}",
               self.markup().get_leading_trivia(),
               self.raw(),
               self.markup().get_trailing_trivia())
    }
}

impl Display for IntegerValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        unimplemented!()
    }
}

impl Display for BoolValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.get() {
            write!(f, "true")
        } else {
            write!(f, "false")
        }
    }
}

impl Display for DatetimeValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        unimplemented!()
    }
}

impl Display for FloatValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        unimplemented!()
    }
}

impl Display for InlineArray {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        unimplemented!()
    }
}

impl Display for InlineTable {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        unimplemented!()
    }
}

impl<'a> Display for KeyMarkup {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f,
               "{}{}{}",
               self.get_leading_trivia(),
               self.raw(),
               self.get_trailing_trivia())
    }
}

impl<'a> Display for TableKeyMarkup {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f,
               "{}{}{}",
               self.get_leading_trivia(),
               self.raw(),
               self.get_trailing_trivia())
    }
}

impl Display for Container {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let is_array = match self.kind {
            ContainerKind::ArrayMember => true,
            ContainerKind::Table => false
        };
        try!(write!(f, "{}", self.get_leading_trivia()));
        if is_array {
            try!(write!(f, "[["));
        } else {
            try!(write!(f, "["));
        }
        try!(fmt_join(f, self.keys().iter(), "."));
        if is_array {
            try!(write!(f, "]]"));
        } else {
            try!(write!(f, "]"));
        }
        fmt_join(f, self.iter_children(), "")
    }
}