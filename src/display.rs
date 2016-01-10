use std::fmt::{Display, Error, Formatter, Write};

use super::{Document, KeyMarkup, StringNode, TableKeyMarkup, TableMarkup};
use super::{TableRef, ValueRef};

fn join_values<'a, T, I>(values: I, sep: &str) -> String 
                      where T: Display, I:Iterator<Item=T> {
    let mut buff = String::new();
    let mut values = values.peekable();
    loop {
        let value = values.next();
        match value {
            Some(value) => {
                let _ = write!(buff, "{}", value);
                if values.peek().is_some() {
                    let _ = write!(buff, "{}", sep);
                }
            }
            None => break,
        }
    }
    buff
}

impl Display for Document {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.iter_syntactic()
            .map(|v| v.fmt(f))
            .find(Result::is_err)
            .unwrap_or(Ok(()))
            .and_then(|_| { f.write_str(&self.get_trailing_trivia()) })
    }
}

impl<'a> Display for ValueRef<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match *self {
            ValueRef::String(node) => node.fmt(f),
            ValueRef::Table(ref table) => table.fmt(f)
        }
    }
}

impl<'a> Display for StringNode {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f,
               "{}={}{}{}",
               self.key(),
               self.markup().get_leading_trivia(),
               self.raw(),
               self.markup().get_trailing_trivia())
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

impl<'a> Display for TableRef<'a> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self.markup() {
            TableMarkup::Inline{ key, value, comma_trivia } => {
                write!(f,
                       "{}={}[{}{}]{}",
                       key,
                       value.get_leading_trivia(),
                       join_values(self.iter_children(), ","),
                       comma_trivia,
                       value.get_trailing_trivia())
            }
            TableMarkup::Implicit => Ok(()),
            TableMarkup::Explicit{ leading_trivia, keys } => {
                write!(f,
                       "{}[{}]{}",
                       leading_trivia,
                       join_values(keys.iter(), "."),
                       join_values(self.iter_children(), ""))
            }
        }
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
