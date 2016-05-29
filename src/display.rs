use std::fmt::{Display, Error, Formatter, Write};

use super::{Document, KeyMarkup, StringValue, TableKeyMarkup, BoolValue};
use super::{ValueRef, Container, DirectChild, InlineArray, FloatValue};
use super::{ContainerKind, InlineTable, IntegerValue, DatetimeValue, ValueMarkup};

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

fn fmt_with_markup<T>(f: &mut Formatter, value: T, markup: &ValueMarkup)
                       -> Result<(), Error> where T: Display {

    write!(f,
           "{}{}{}",
           markup.get_leading_trivia(),
           value,
           markup.get_trailing_trivia())
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
        fmt_with_markup(f, self.raw(), self.markup())
    }
}

impl Display for IntegerValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        fmt_with_markup(f, self.raw(), self.markup())
    }
}

impl Display for BoolValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        fmt_with_markup(f, self.get(), self.markup())
    }
}

impl Display for DatetimeValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        fmt_with_markup(f, self.get(), self.markup())
    }
}

impl Display for FloatValue {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        fmt_with_markup(f, self.raw(), self.markup())
    }
}

impl Display for InlineArray {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        try!(write!(f, "{}[", self.markup().get_leading_trivia()));
        try!(fmt_join(f, self.iter(), ","));
        try!(write!(f, "{}", self.markup().get_comma_trivia()));
        try!(write!(f, "]{}", self.markup().get_trailing_trivia()));
        Ok(())
    }
}

impl Display for InlineTable {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        try!(write!(f, "{}{{", self.markup().get_leading_trivia()));
        try!(fmt_join(f, self.iter(), ","));
        try!(write!(f, "}}{}", self.markup().get_trailing_trivia()));
        Ok(())
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
        try!(write!(f, "{}", self.keys().get_leading_trivia()));
        if is_array {
            try!(write!(f, "[["));
        } else {
            try!(write!(f, "["));
        }
        try!(fmt_join(f, self.keys().markup().iter(), "."));
        if is_array {
            try!(write!(f, "]]"));
        } else {
            try!(write!(f, "]"));
        }
        try!(write!(f, "{}", self.keys().get_trailing_trivia()));
        fmt_join(f, self.iter_children(), "")
    }
}