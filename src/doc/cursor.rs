use std::cell::Ref;
use std::collections::HashMap;

use super::{StringData, TableData, Container, IndirectChild, FormattedValue};

pub enum ValueCursor<'a> {
    String(StringCursor<'a>),
    Table(TableCursor<'a>),
}

pub struct StringCursor<'a>(#[doc(hidden)] pub Ref<'a, FormattedValue>);

pub struct TableCursor<'a> {
    data: Table<'a>
}

enum Table<'a> {
    Inline(&'a TableData),
    Implicit(&'a HashMap<String, IndirectChild>),
    Explicit(&'a Container)
}