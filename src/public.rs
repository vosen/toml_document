use std::cell::{RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::slice;
use std::str::Chars;
use std::iter::{Peekable};

use super::{check_ws, eat_before_newline, eat_newline};
use super::{MALFORMED_LEAD_MSG, MALFORMED_TRAIL_MSG};
use Parser;

use super::{TableData, Container, IndirectChild, PrivKeyMarkup, StringData};
use super::{Value, ValueNode, ValueMarkup, FormattedKey, FormattedValue};
use super::{ContainerKind, Document, ValuesMap, InlineArrayData};

macro_rules! define_view {
    ($name: ident, $inner: ty) => (
        pub struct $name($inner);

        impl $name {
            #[allow(dead_code)]
            fn new<'a>(src: &'a $inner) -> &'a $name {
                unsafe { ::std::mem::transmute(src) }
            }

            #[allow(dead_code)]
            fn new_mut<'a>(src: &'a mut $inner) -> &'a mut $name {
                unsafe { ::std::mem::transmute(src) }
            }

            #[allow(dead_code)]
            fn new_slice<'a>(src: &'a [$inner]) -> &'a [$name] {
                unsafe { ::std::mem::transmute(src) }
            }

            #[allow(dead_code)]
            fn new_slice_mut<'a>(src: &'a mut [$inner]) -> &'a mut [$name] {
                unsafe { ::std::mem::transmute(src) }
            }
        }
    )
}

macro_rules! impl_value_markup {
    ($name: ident) => (
        impl $name {
            pub fn markup(&self) -> &ValueMarkup {
                &self.0.markup
            }

            pub fn markup_mut(&mut self) -> &mut ValueMarkup {
                &mut self.0.markup
            }
        }
    )
}

unsafe fn transmute_lifetime<'a, 'b, T>(x: &'a T) -> &'b T {
    ::std::mem::transmute(x)
}

unsafe fn transmute_lifetime_mut<'a, 'b, T>(x: &'a mut T) -> &'b mut T {
    ::std::mem::transmute(x)
}

fn direct_cursor<'a>((k, v): (&'a String, &'a Rc<RefCell<ValueNode>>)) -> (&'a str, EntryRef<'a>) {
    (k, ValueNode::as_cursor(v))
}

fn indirect_cursor<'a>((k, v): (&'a String, &'a IndirectChild)) -> (&'a str, EntryRef<'a>) {
    (k, v.as_cursor())
}

fn iter_logical<'a>(direct: &'a ValuesMap,
                    indirect: &'a HashMap<String, IndirectChild>)
                    -> Box<Iterator<Item = (&'a str, EntryRef<'a>)> + 'a> {
    let val_iter = direct.kvp_index
                         .iter()
                         .map(direct_cursor);
    let cont_iter = indirect.iter()
                            .map(indirect_cursor);
    Box::new(val_iter.chain(cont_iter))
}

impl Document {
    pub fn new() -> Document {
        Document {
            values: ValuesMap::new(),
            container_list: Vec::new(),
            container_index: HashMap::new(),
            trail: String::new(),
        }
    }

    pub fn get(&self, key: &str) -> Option<EntryRef> {
        self.values
            .get_entry(key)
            .or_else(|| {
                self.container_index
                    .get(key)
                    .map_or(None, |child| Some(child.as_cursor()))
            })
    }

    pub fn len(&self) -> usize {
        self.values.len() + self.container_index.len()
    }

    pub fn iter<'a>(&'a self)
                            -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
        iter_logical(&self.values, &self.container_index)
    }

    pub fn get_child(&self, idx: usize) -> &DirectChild {
        unsafe { DirectChild::new_rc(self.values.get_at(idx)) }
    }

    pub fn len_children(&self) -> usize {
        self.values.len()
    }

    pub fn iter_children(&self) -> DirectChildren {
        self.values.iter_children()
    }

    pub fn get_container(&self, idx: usize) -> &Container {
        unsafe { Container::new(&self.container_list[idx]) }
    }

    pub fn len_containers(&self) -> usize {
        self.container_list.len()
    }

    pub fn iter_containers(&self) -> Containers {
        Containers {
            iter: self.container_list.iter()
        }
    }

    pub fn get_trailing_trivia(&self) -> &str {
        &self.trail
    }

    fn insert_child(&mut self, idx: usize, key: String, value: Value)
                    -> &mut ValueNode {
        let node = ValueNode::new(key, value);
        self.values.insert_public(idx, node);
        unsafe {
            transmute_lifetime_mut(&mut self.values.get_at_mut(idx))
        }
    }

    pub fn insert_string(&mut self, idx: usize, key: String, value: String)
                         -> &mut StringValue {
        let value = Value::String(
            StringData {
                raw: StringValue::escape(&value),
                escaped: value
            }
        );
        let node = self.insert_child(idx, key, value);
        StringValue::new_mut(&mut node.value)
    }

    pub fn insert_integer(&mut self, idx: usize, key: String, value: i64)
                          -> &mut IntegerValue {
        let value = Value::Integer{
            raw: value.to_string(),
            parsed: value
        };
        let node = self.insert_child(idx, key, value);
        IntegerValue::new_mut(&mut node.value)
    }

    pub fn insert_float(&mut self, idx: usize, key: String, value: f64)
                        -> &mut FloatValue {
        let value = Value::Float{
            raw: value.to_string(),
            parsed: value
        };
        let node = self.insert_child(idx, key, value);
        FloatValue::new_mut(&mut node.value)
    }

    pub fn insert_boolean(&mut self, idx: usize, key: String, value: bool)
                          -> &mut BoolValue {
        let value = Value::Boolean(value);
        let node = self.insert_child(idx, key, value);
        BoolValue::new_mut(&mut node.value)
    }

    pub fn insert_datetime(&mut self, idx: usize, key: String, value: String)
                           -> &mut DatetimeValue {
        if !Parser::_is_valid_datetime(&value) {
            panic!("Malformed date literal `{}` for key `{}`", value, key)
        }
        let value = Value::Datetime(value);
        let node = self.insert_child(idx, key, value);
        DatetimeValue::new_mut(&mut node.value)
    }

    pub fn insert_array(&mut self, idx: usize, key: String)
                               -> &mut InlineArray {
        let value = Value::InlineArray(
            InlineArrayData {
                values: Vec::new(),
                comma_trail: "".to_owned()
            }
        );
        let node = self.insert_child(idx, key, value);
        InlineArray::new_mut(&mut node.value)
    }

    pub fn insert_inline_table(&mut self, idx: usize, key: String)
                               -> &mut InlineTable {
        let value = Value::new_table(ValuesMap::new(), "".to_owned());
        let node = self.insert_child(idx, key, value);
        InlineTable::new_mut(&mut node.value)
    }

    pub fn find_index<T:InternalNode>(&self, node: &T) -> Option<usize> {
        let address = node.ptr();
        for (idx, rc) in self.values.kvp_list.iter().enumerate() {
            let inner : &ValueNode = &rc.borrow();
            if inner as *const ValueNode as usize == address {
                return Some(idx)
            }
            if &inner.value as *const FormattedValue as usize == address {
                return Some(idx)
            }
        }
        for (idx, rc) in self.container_list.iter().enumerate() {
            let inner : &Container = &rc.borrow();
            if inner as *const Container as usize == address {
                return Some(idx + self.values.len())
            }
        }
        None
    }
}

pub struct DirectChildren<'a> {
    iter: Option<slice::Iter<'a, Rc<RefCell<ValueNode>>>>
}

impl<'a> Iterator for DirectChildren<'a> {
    type Item = &'a DirectChild;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter {
            Some(ref mut iter) => {
                iter.next().map(|v| unsafe { DirectChild::new_rc(v) })
            }
            None => None
        }
    }
}

pub struct Containers<'a> {
    iter: slice::Iter<'a, Rc<RefCell<Container>>>
}

impl<'a> Iterator for Containers<'a> {
    type Item = &'a Container;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|c| unsafe { Container::new(c) })
    }
}

define_view!(DirectChild, ValueNode);

impl DirectChild {
    unsafe fn new_rc(src: &Rc<RefCell<ValueNode>>) -> &DirectChild {
        DirectChild::new(transmute_lifetime(&*src.borrow()))
    }

    pub fn key(&self) -> &KeyMarkup {
        &KeyMarkup::new(&self.0.key)
    }

    pub fn value(&self) -> ValueRef {
        ValueRef::new(&self.0.value)
    }
}

#[derive(Clone,Copy)]
pub enum ValueRef<'a> {
    String(&'a StringValue),
    Integer(&'a IntegerValue),
    Float(&'a FloatValue),
    Boolean(&'a BoolValue),
    Datetime(&'a DatetimeValue),
    Array(&'a InlineArray),
    Table(&'a InlineTable)
}

impl<'a> ValueRef<'a> {
    fn new(src: &FormattedValue) -> ValueRef {
        match src.value {
            Value::String(..) => ValueRef::String(
                StringValue::new(src)
            ),
            Value::Integer{..} => ValueRef::Integer(
                IntegerValue::new(src)
            ),
            Value::Float{..} => ValueRef::Float(
                FloatValue::new(src)
            ),
            Value::Boolean(..) => ValueRef::Boolean(
                BoolValue::new(src)
            ),
            Value::Datetime(..) => ValueRef::Datetime(
                DatetimeValue::new(src)
            ),
            Value::InlineArray(..) => ValueRef::Array(
                InlineArray::new(src)
            ),
            Value::InlineTable(..) => ValueRef::Table(
                InlineTable::new(src)
            ),
        }
    }

    pub fn to_entry(self) -> EntryRef<'a> {
        match self {
            ValueRef::String(val) => EntryRef::String(val),
            ValueRef::Integer(val) => EntryRef::Integer(val),
            ValueRef::Float(val) => EntryRef::Float(val),
            ValueRef::Boolean(val) => EntryRef::Boolean(val),
            ValueRef::Datetime(val) => EntryRef::Datetime(val),
            ValueRef::Array(arr) => {
                EntryRef::Array(
                      ArrayEntry {
                        data: Array::Inline(arr)
                    }
                )
            },
            ValueRef::Table(arr) => {
                EntryRef::Table(
                      TableEntry {
                        data: Table::Inline(arr)
                    }
                )
            },
        }
    }
}

impl ValuesMap {
    fn get(&self, key: &str) -> Option<&DirectChild> {
        self.kvp_index
            .get(key)
            .map(|val| unsafe { DirectChild::new_rc(val) })
    }

    fn get_entry(&self, key: &str) -> Option<EntryRef> {
        self.kvp_index
            .get(key)
            .map(|val| ValueNode::as_cursor(val))
    }

    fn get_at(&self, idx: usize) -> &Rc<RefCell<ValueNode>> {
        &self.kvp_list[idx]
    }

    fn len(&self) -> usize {
        self.kvp_list.len()
    }

    fn iter_entries<'a>(&'a self)
                        -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
        let iter = self.kvp_index
                       .iter()
                       .map(direct_cursor);
        Box::new(iter)
    }

    fn iter_children(&self) -> DirectChildren {
        DirectChildren {
            iter: Some(self.kvp_list.iter())
        }
    }
}

impl ValueNode {
    fn new(key: String, value: Value) -> ValueNode {
        ValueNode {
            key: FormattedKey {
                escaped: key,
                raw: None,
                markup: PrivKeyMarkup {
                    lead: String::new(),
                    trail: " ".to_owned()
                }
            },
            value: FormattedValue {
                value: value,
                markup: ValueMarkup {
                    lead: " ".to_owned(),
                    trail: "\n".to_owned()
                }
            }
        }
    }

    fn as_cursor<'a>(r: &'a RefCell<Self>) -> EntryRef<'a> {
        match r.borrow().value.value  {
            Value::String(..) => {
                EntryRef::String(
                    StringValue::new(
                        &unsafe { transmute_lifetime(&*r.borrow()) }.value
                    )
                )
            }
            Value::Integer{..} => {
                EntryRef::Integer(
                    IntegerValue::new(
                        &unsafe { transmute_lifetime(&*r.borrow()) }.value
                    )
                )
            }
            Value::Float{..} => {
                EntryRef::Float(
                    FloatValue::new(
                        &unsafe { transmute_lifetime(&*r.borrow()) }.value
                    )
                )
            }
            Value::Boolean(..) => {
                EntryRef::Boolean(
                    BoolValue::new(
                        &unsafe { transmute_lifetime(&*r.borrow()) }.value
                    )
                )
            }
            Value::Datetime(..) => {
                EntryRef::Datetime(
                    DatetimeValue::new(
                        &unsafe { transmute_lifetime(&*r.borrow()) }.value
                    )
                )
            }
            Value::InlineArray(..) => {
                let value_wrapper = unsafe { transmute_lifetime(&*r.borrow()) };                
                EntryRef::Array(
                    ArrayEntry{
                        data: Array::Inline(
                            InlineArray::new(&value_wrapper.value)
                        )
                    }
                )
            }
            Value::InlineTable(..) => {
                let value_wrapper = unsafe { transmute_lifetime(&*r.borrow()) };                
                EntryRef::Table(
                    TableEntry {
                        data: Table::Inline(
                            InlineTable::new(&value_wrapper.value)
                        )
                    }
                )
            }
        }
    }
}

impl Container {
    unsafe fn new(src: &Rc<RefCell<Container>>) -> &Container {
        transmute_lifetime(&*src.borrow())
    }

    fn len_logical(&self) -> usize {
        self.data.direct.len() + self.data.indirect.len()
    }

    fn iter_logical<'a>(&'a self)
                        -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
        self.data.iter_logical()
    }

    pub fn get(&self, key: &str) -> Option<EntryRef> {
        self.data.get(key)
    }

    pub fn kind(&self) -> ContainerKind {
        self.kind
    }

    pub fn len_children(&self) -> usize {
        self.data.direct.len()
    }

    pub fn iter_children(&self) -> DirectChildren {
        self.data.direct.iter_children()
    }

    pub fn get_leading_trivia(&self) -> &str {
        &self.lead
    }

    pub fn keys(&self) -> &[TableKeyMarkup] {
        TableKeyMarkup::new_slice(&self.keys)
    }

    pub fn to_entry(&self) -> EntryRef {
        EntryRef::Table(
            TableEntry {
                data: Table::Explicit(self)
            }
        )
    }
}

impl super::ContainerData {
    fn get(&self, key: &str) -> Option<EntryRef> {
        self.direct
            .get_entry(key)
            .or_else(|| {
                self.indirect
                    .get(key)
                    .map_or(None, |c| Some(IndirectChild::as_cursor(c)))
            })
    }

    fn iter_logical<'a>(&'a self)
                        -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
        iter_logical(&self.direct, &self.indirect)
    }
}

impl IndirectChild {
    fn as_cursor(&self) -> EntryRef {
        match self {
            &IndirectChild::ImplicitTable(ref map) => {
                EntryRef::Table(
                    TableEntry {
                        data: Table::Implicit(map)
                    }
                )
            }
            &IndirectChild::ExplicitTable(ref container) => {
                EntryRef::Table(
                    TableEntry {
                        data: Table::Explicit(
                            unsafe { transmute_lifetime(&container.borrow()) }
                        )
                    }
                )
            }
            &IndirectChild::Array(ref arr) => {
                EntryRef::Array(
                    ArrayEntry {
                        data: Array::Explicit(&arr)
                    }
                )
            }
        }
    }

    fn get(&self, key: &str) -> Option<EntryRef> {
        match self {
            &IndirectChild::ImplicitTable(ref map) => {
                map.get(key).map(IndirectChild::as_cursor)
            }
            &IndirectChild::ExplicitTable(ref container) => {
                unsafe { transmute_lifetime(&container.borrow().data) }.get(key)
            }
            &IndirectChild::Array(..) => panic!()
        }
    }
}

#[derive(Clone, Copy)]
pub enum EntryRef<'a> {
    String(&'a StringValue),
    Integer(&'a IntegerValue),
    Float(&'a FloatValue),
    Boolean(&'a BoolValue),
    Datetime(&'a DatetimeValue),
    Array(ArrayEntry<'a>),
    Table(TableEntry<'a>),
}

impl<'a> EntryRef<'a> {
    pub fn is_child(self) -> bool {
        match self {
            EntryRef::String(..)
            | EntryRef::Integer(..)
            | EntryRef::Float(..)
            | EntryRef::Boolean(..)
            | EntryRef::Datetime(..)=> true,
            EntryRef::Array(arr) => {
                match arr.to_value() {
                    ArrayValue::Inline(..) => true,
                    ArrayValue::OfTables => false
                }
            }
            EntryRef::Table(table) => {
                match table.to_value() {
                    TableValue::Inline(..) =>true,
                    TableValue::Implicit
                    | TableValue::Explicit(..) => false,
                }
            }
        }
    }
}

define_view!(BoolValue, FormattedValue);
impl_value_markup!(BoolValue);

impl BoolValue {
    pub fn get(&self) -> bool {
        match self.0.value {
            Value::Boolean(data) => data,
            _ => unreachable!()
        }
    }

    pub fn set(&mut self, val: bool) {
        self.0.value = Value::Boolean(val);
    }
}

define_view!(StringValue, FormattedValue);
impl_value_markup!(StringValue);

impl StringValue {
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

    pub fn raw(&self) -> &str {
        match self.0.value {
            Value::String(ref data) => &data.raw,
            _ => unreachable!()
        }
    }

    fn escape(value: &str) -> String {
        let mut buff = String::with_capacity(value.len() + 2);
        buff.push('"');
        for c in value.chars() {
            match c {
                '\u{0008}' => buff.push_str("\\b"),
                '\t' => buff.push_str("\\t"),
                '\n' => buff.push_str("\\n"),
                '\u{000C}' => buff.push_str("\\f"),
                '\r' => buff.push_str("\\r"),
                '\"' => buff.push_str("\\\""),
                '\\' => buff.push_str("\\\\"),
                c if c.is_control() => {
                    buff.push_str(&format!("\\u{:04x}", c as u32));
                }
                c => buff.push(c)
            }
        }
        buff.push('"');
        buff
    }
}

define_view!(FloatValue, FormattedValue);
impl_value_markup!(FloatValue);

impl FloatValue {
    pub fn get(&self) -> f64 {
        match self.0.value {
            Value::Float{ parsed, .. }  => parsed,
            _ => unreachable!()
        }
    }

    pub fn set(&mut self, f: f64) {
        self.0.value = Value::Float { 
            parsed: f,
            raw: format!("{}", f)

        };
    }

    pub fn raw(&self) -> &str {
        match self.0.value {
            Value::Float{ ref raw, .. } => &raw,
            _ => unreachable!()
        }
    }
}

define_view!(IntegerValue, FormattedValue);
impl_value_markup!(IntegerValue);

impl IntegerValue {
    pub fn get(&self) -> i64 {
        match self.0.value {
            Value::Integer{ parsed, .. }  => parsed,
            _ => unreachable!()
        }
    }

    pub fn set(&mut self, i: i64) {
        self.0.value = Value::Integer { 
            parsed: i,
            raw: format!("{}", i)

        };
    }

    pub fn raw(&self) -> &str {
        match self.0.value {
            Value::Integer{ ref raw, .. } => &raw,
            _ => unreachable!()
        }
    }
}

define_view!(DatetimeValue, FormattedValue);
impl_value_markup!(DatetimeValue);

impl DatetimeValue {
    pub fn get(&self) -> &str {
        match self.0.value {
            Value::Datetime(ref value) => &value,
            _ => unreachable!()
        }
    }

    pub fn set(&mut self, s: String) {
        unimplemented!()
    }
}

define_view!(InlineArray, FormattedValue);

impl InlineArray {
    pub fn markup(&self) -> &InlineArrayMarkup {
        InlineArrayMarkup::new(&self.0)
    }

    pub fn markup_mut(&mut self) -> &mut InlineArrayMarkup {
        InlineArrayMarkup::new_mut(&mut self.0)
    }
}

impl InlineArray {
    fn data(&self) -> &InlineArrayData {
        match self.0.value {
            super::Value::InlineArray(ref arr_data) => arr_data,
            _ => unreachable!()
        }
    }

    pub fn len(&self) -> usize {
        self.data().values.len()
    }

    pub fn get(&self, idx: usize) -> ValueRef {
        ValueRef::new(&self.data().values[idx])
    }

    pub fn iter(&self) -> Values {
        Values {
            iter: self.data().values.iter()
        }
    }
}

define_view!(InlineArrayMarkup, FormattedValue);

impl InlineArrayMarkup {
    pub fn get_leading_trivia(&self) -> &str {
        self.0.markup.get_leading_trivia()
    }

    pub fn set_leading_trivia(&mut self, s: String) {
        self.0.markup.set_leading_trivia(s)
    }

    pub fn get_trailing_trivia(&self) -> &str {
        self.0.markup.get_trailing_trivia()
    }

    pub fn set_trailing_trivia(&mut self, s: String) {
        self.0.markup.set_trailing_trivia(s)
    }

    pub fn get_comma_trivia(&self) -> &str {
        match self.0.value {
            Value::InlineArray(ref data) => &data.comma_trail,
            _ => unreachable!()
        }
    }
}

pub struct Values<'a> {
    iter: slice::Iter<'a, FormattedValue>
}

impl<'a> Iterator for Values<'a> {
    type Item = ValueRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(ValueRef::new)
    }
}

#[derive(Clone, Copy)]
pub struct TableEntry<'a> {
    data: Table<'a>
}

#[derive(Copy, Clone)]
enum Table<'a> {
    Inline(&'a InlineTable),
    Implicit(&'a HashMap<String, IndirectChild>),
    Explicit(&'a Container)
}

impl<'a> TableEntry<'a> {
    pub fn get(self, key: &'a str) -> Option<EntryRef> {
        match self.data {
            Table::Inline(data) => data.get_entry(key),
            Table::Implicit(map) => implicit_table_get(map, key),
            Table::Explicit(data) => data.get(key),
        }
    }

    pub fn len_children(self) -> usize {
        match self.data {
            Table::Inline(ref data) => data.len(),
            Table::Implicit(..) => 0,
            Table::Explicit(ref data) => data.len_children(),
        }
    }

    pub fn iter_children(self) -> DirectChildren<'a> {
        match self.data {
            Table::Inline(ref tbl) => tbl.data().values.direct.iter_children(),
            Table::Implicit(..) => DirectChildren { iter: None },
            Table::Explicit(ref data) => data.iter_children(),
        }
    }

    pub fn len(self) -> usize {
        match self.data {
            Table::Inline(ref data) => data.len(),
            Table::Implicit(ref map) => implicit_table_len_logical(map),
            Table::Explicit(ref data) => data.len_logical(),
        }
    }

    pub fn iter(self) -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
        match self.data {
            Table::Inline(ref data) => data.iter_entries(),
            Table::Implicit(ref map) => implicit_table_iter_logical(map),
            Table::Explicit(ref data) => data.iter_logical(),
        }
    }

    pub fn to_value(self) -> TableValue<'a> {
        match self.data {
            Table::Inline(data) => TableValue::Inline(data),
            Table::Implicit(..) => TableValue::Implicit,
            Table::Explicit(data) => TableValue::Explicit(data)
        }
    }
}

#[derive(Clone, Copy)]
pub enum TableValue<'a> {
    Inline(&'a InlineTable),
    Implicit,
    Explicit(&'a Container),
}

#[derive(Clone, Copy)]
pub struct ArrayEntry<'a> {
    data: Array<'a>
}

#[derive(Copy, Clone)]
enum Array<'a> {
    Inline(&'a InlineArray),
    Explicit(&'a [Rc<RefCell<Container>>])
}

#[derive(Copy, Clone)]
pub enum ArrayValue<'a> {
    Inline(&'a InlineArray),
    OfTables
}

impl<'a> ArrayEntry<'a> {
    pub fn to_value(self) -> ArrayValue<'a> {
        match self.data {
            Array::Inline(arr) => ArrayValue::Inline(arr),
            Array::Explicit(..) => ArrayValue::OfTables
        }
    }

    pub fn len(self) -> usize {
        match self.data {
            Array::Inline(data) => data.len(),
            Array::Explicit(vec) => vec.len()
        }
    }

    pub fn get(self, idx: usize) -> EntryRef<'a> {
        match self.data {
            Array::Inline(data) => data.get(idx).to_entry(),
            Array::Explicit(vec) => unsafe {
                Container::new(&vec[idx]).to_entry()
            }
        }
    }

    pub fn iter(self) -> Box<Iterator<Item=EntryRef<'a>> + 'a> {
        match self.data {
            Array::Inline(data) => {
                Box::new(data.iter().map(ValueRef::to_entry))
            }
            Array::Explicit(vec) => {
                let iter = vec.iter()
                               .map(|x| unsafe { Container::new(x) })
                               .map(Container::to_entry);
                Box::new(iter)
            }
        }
    }
}

define_view!(InlineTable, FormattedValue);

impl InlineTable {
    fn data(&self) -> &TableData {
        match self.0.value {
            super::Value::InlineTable(ref table_data) => table_data,
            _ => unreachable!()
        }
    }

    pub fn get(&self, key: &str) -> Option<&DirectChild> {
        self.data().values.direct.get(key)
    }

    fn get_entry(&self, key: &str) -> Option<EntryRef> {
        self.data().values.direct.get_entry(key)
    }

    pub fn len(&self) -> usize {
        self.data().values.direct.len()
    }

    pub fn iter(&self) -> DirectChildren {
        self.data().values.direct.iter_children()
    }

    fn iter_entries<'a>(&'a self)
                    -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
        self.data().values.direct.iter_entries()
    }

    pub fn markup(&self) -> &ValueMarkup {
        &self.0.markup
    }

    pub fn markup_mut(&mut self) -> &mut ValueMarkup {
        &mut self.0.markup
    }

    pub fn get_comma_trivia(&self) -> &str {
        &self.data().comma_trail
    }

    pub fn as_ref(&self) -> TableEntry {
        TableEntry {
            data: Table::Inline(self)
        }
    }
}

fn implicit_table_get<'a>(t: &'a HashMap<String, IndirectChild>,
                          key: &str) -> Option<EntryRef<'a>> {
    t.get(key).map(IndirectChild::as_cursor)
}

fn implicit_table_len_logical<'a>(t: &'a HashMap<String, IndirectChild>)
                                  -> usize {
    t.len()
}

fn implicit_table_iter_logical<'a>(t: &'a HashMap<String, IndirectChild>)
                                   -> Box<Iterator<Item=(&'a str, EntryRef<'a>)> + 'a> {
    let iter = t.iter().map(indirect_cursor);
    Box::new(iter)
}


define_view!(TableKeyMarkup, FormattedKey);

impl TableKeyMarkup {
    pub fn get(&self) -> &str {
        &self.0.escaped
    }

    pub fn get_leading_trivia(&self) -> &str {
        &self.0.markup.lead
    }

    pub fn set_leading_trivia(&mut self, s: String) {
        check_ws(&*s, super::MALFORMED_LEAD_MSG);
        self.0.markup.lead = s;
    }

    pub fn get_trailing_trivia(&self) -> &str {
        &self.0.markup.trail
    }

    pub fn set_trailing_trivia(&mut self, s: String) {
        check_ws(&*s, super::MALFORMED_TRAIL_MSG);
        self.0.markup.trail = s;
    }

    pub fn raw(&self) -> &str {
        match self.0.raw {
            Some(ref raw) => &*raw,
            None => &*self.0.escaped
        }
    }
}

define_view!(KeyMarkup, FormattedKey);

impl KeyMarkup {
    pub fn get(&self) -> &str {
        &self.0.escaped
    }

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

    pub fn raw(&self) -> &str {
        match self.0.raw {
            Some(ref raw) => &*raw,
            None => &*self.0.escaped
        }
    }
}

fn eat_eof(mut chars: Peekable<Chars>, error: &'static str) {
    assert!(chars.next() == None, error);
}

impl ValueMarkup {
    pub fn get_leading_trivia(&self) -> &str {
        &self.lead
    }

    pub fn set_leading_trivia(&mut self, s: String) {
        check_ws(&*s, MALFORMED_LEAD_MSG);
        self.lead = s;
    }

    pub fn get_trailing_trivia(&self) -> &str {
        &self.trail
    }

    pub fn set_trailing_trivia(&mut self, s: String) {
        ValueMarkup::check_trailing_trivia(&*s);
        self.trail = s;
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

    fn check_comment_or_newline(mut chars: Peekable<Chars>) {
        if chars.peek() == Some(&'#') {
            chars = eat_before_newline(chars);
        }
        chars = eat_newline(chars, MALFORMED_TRAIL_MSG);
        eat_eof(chars, MALFORMED_TRAIL_MSG);
    }
}

pub trait InternalNode: Sized {
    fn ptr(&self) -> usize {
        self as *const Self as usize
    }
}

impl InternalNode for DirectChild {}

impl InternalNode for StringValue {}
impl InternalNode for IntegerValue {}
impl InternalNode for FloatValue {}
impl InternalNode for BoolValue {}
impl InternalNode for DatetimeValue {}
impl InternalNode for InlineArray {}
impl InternalNode for InlineTable {}
impl InternalNode for Container {}

impl<'a> InternalNode for ValueRef<'a> {
    fn ptr(&self) -> usize {
        match *self {
            ValueRef::String(x) => x.ptr(),
            ValueRef::Integer(x) => x.ptr(),
            ValueRef::Float(x) => x.ptr(),
            ValueRef::Boolean(x) => x.ptr(),
            ValueRef::Datetime(x) => x.ptr(),
            ValueRef::Array(x) => x.ptr(),
            ValueRef::Table(x) => x.ptr(),
        }
    }
}

#[cfg(test)]
mod tests {
    mod document {
        use Document;

        #[test]
        fn insert_string_simple() {
            let mut doc = Document::new();
            {
                let val = doc.insert_string(0,
                                            "foo".to_owned(),
                                            "bar".to_owned());
                assert_eq!(r#""bar""#, val.raw());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_string_escaped() {
            let mut doc = Document::new();
            {
                let val = doc.insert_string(0,
                                            "foo".to_owned(),
                                            "b\nr".to_owned());
                assert_eq!(r#""b\nr""#, val.raw());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_string_escape_control() {
            let mut doc = Document::new();
            {
                let val = doc.insert_string(0,
                                            "foo".to_owned(),
                                            "\u{0016}".to_owned());
                assert_eq!("\"\\u0016\"", val.raw());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_integer_zero() {
            let mut doc = Document::new();
            {
                let val = doc.insert_integer(0, "foo".to_owned(), 0);
                assert_eq!("0", val.raw());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_integer_min() {
            let mut doc = Document::new();
            {
                let val = doc.insert_integer(0,
                                             "foo".to_owned(),
                                             -9223372036854775808);
                assert_eq!("-9223372036854775808", val.raw());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_integer_max() {
            let mut doc = Document::new();            
            {
                let val = doc.insert_integer(0,
                                             "foo".to_owned(),
                                             9223372036854775807);
                assert_eq!("9223372036854775807", val.raw());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_float_neg() {
            let mut doc = Document::new();
            {
                let val = doc.insert_float(0, "foo".to_owned(), -0.3);
                assert_eq!("-0.3", val.raw());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_bool() {
            let mut doc = Document::new();
            {
                let val = doc.insert_boolean(0, "foo".to_owned(), false);
                assert_eq!(false, val.get());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_array() {
            let mut doc = Document::new();
            {
                let val = doc.insert_array(0, "foo".to_owned());
                assert_eq!(0, val.len());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_inline_table() {
            let mut doc = Document::new();
            {
                let val = doc.insert_inline_table(0, "foo".to_owned());
                assert_eq!(0, val.len());
            }
            assert_eq!(1, doc.len());
        }

        #[test]
        fn insert_datetime() {
            let mut doc = Document::new();
            {
                let val = doc.insert_datetime(0,
                                              "foo".to_owned(),
                                              "1979-05-27T07:32:00Z".to_owned());
                assert_eq!("1979-05-27T07:32:00Z", val.get());
            }
            assert_eq!(1, doc.len());
        }
    }
}