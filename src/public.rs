use std::cell::{RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::slice;

use super::{check_ws, eat_before_newline, eat_newline};
use super::{MALFORMED_LEAD_MSG, MALFORMED_TRAIL_MSG};

use super::{TableData, Container, IndirectChild};
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
            .get(key)
            .or_else(|| {
                self.container_index
                    .get(key)
                    .map_or(None, |child| Some(child.as_cursor()))
            })
    }

    pub fn get_mut(&mut self, key: &str) -> Option<EntryRefMut> {
        if let some @ Some(_)  = self.values.get_mut(key) {
            return some;
        };
        self.container_index
            .get_mut(key)
            .map_or(None, |child| child.get_mut(key))
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
    Array(&'a InlineArray),
    Table(&'a InlineTable)
}

impl<'a> ValueRef<'a> {
    fn new(src: &FormattedValue) -> ValueRef {
        match src.value {
            Value::String(..) => ValueRef::String(
                StringValue::new(src)
            ),
            Value::InlineArray(..) => ValueRef::Array(
                InlineArray::new(src)
            ),
            _ => unimplemented!()
        }
    }

    pub fn to_entry(self) -> EntryRef<'a> {
        match self {
            ValueRef::String(val) => EntryRef::String(val),
            ValueRef::Array(arr) => {
                EntryRef::Array(
                      ArrayEntry {
                        data: Array::Inline(arr)
                    }
                )
            },
            _ => unimplemented!()
        }
    }
}

impl ValuesMap {
    fn get(&self, key: &str) -> Option<EntryRef> {
        self.kvp_index
            .get(key)
            .map(|val| { ValueNode::as_cursor(val) })
    }

    fn get_mut(&mut self, key: &str) -> Option<EntryRefMut> {
        self.kvp_index
            .get_mut(key)
            .map(|val| { ValueNode::as_cursor_mut(val) })
    }

    fn get_at(&self, idx: usize) -> &Rc<RefCell<ValueNode>> {
        &self.kvp_list[idx]
    }

    fn get_at_mut(&mut self, idx: usize) -> &mut Rc<RefCell<ValueNode>> {
        &mut self.kvp_list[idx]
    }

    fn len(&self) -> usize {
        self.kvp_list.len()
    }

    fn iter<'a>(&'a self) -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
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
    fn as_cursor<'a>(r: &'a RefCell<Self>) -> EntryRef<'a> {
        match r.borrow().value.value  {
            Value::String(..) => {
                EntryRef::String(
                    StringValue::new(
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
                            InlineTable::new(&value_wrapper)
                        )
                    }
                )
            }
            _ => unimplemented!()
        }
    }

    fn as_cursor_mut<'a>(r: &'a RefCell<Self>) -> EntryRefMut<'a> {
        fn borrow_mut<'b>(r: &'b RefCell<ValueNode>) -> &'b mut ValueNode {
            unsafe { transmute_lifetime_mut(&mut r.borrow_mut()) }
        }
        let value = { &r.borrow().value.value };
        match *value {
            Value::String(..) => {
                drop(value);
                EntryRefMut::String(
                    StringValue::new_mut(&mut borrow_mut(r).value)
                )
            }
            Value::InlineArray(..) => {
                drop(value);
                EntryRefMut::InlineArray(
                    InlineArray::new_mut(&mut borrow_mut(r).value)
                )
            }
            _ => unimplemented!()
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
        match self.kind {
            ContainerKind::ArrayMember => panic!(),
            ContainerKind::Table => unimplemented!()
        }
    }
}

impl super::ContainerData {
    fn get(&self, key: &str) -> Option<EntryRef> {
        self.direct
            .get(key)
            .or_else(|| {
                self.indirect
                    .get(key)
                    .map_or(None, |child| child.get(key))
            })
    }

    fn get_mut(&mut self, key: &str) -> Option<EntryRefMut> {
        if let some @ Some(_) =  self.direct.get_mut(key) {
            return some;
        };
        self.indirect
            .get_mut(key)
            .map_or(None, |child| child.get_mut(key))
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

    fn as_cursor_mut(&mut self) -> EntryRefMut {
        match self {
            &mut IndirectChild::ImplicitTable(ref mut map) => {
                EntryRefMut::ImplicitTable(
                    ImplicitTableEntry::new(map)
                )
            }
            &mut IndirectChild::ExplicitTable(ref container) => {
                EntryRefMut::ExplicitTable(
                    ExplicitTableEntry::new(container)
                )
            }
            &mut IndirectChild::Array(..) => unimplemented!()
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

    fn get_mut(&mut self, key: &str) -> Option<EntryRefMut> {
        match *self {
            IndirectChild::ImplicitTable(ref mut map) => {
                map.get_mut(key).map(IndirectChild::as_cursor_mut)
            }
            IndirectChild::ExplicitTable(ref mut container) => {
                let container_data = &mut container.borrow_mut().data;
                unsafe { transmute_lifetime_mut(container_data) }.get_mut(key)
            }
            IndirectChild::Array(..) => panic!()
        }
    }
}

#[derive(Clone, Copy)]
pub enum EntryRef<'a> {
    String(&'a StringValue),
    Array(ArrayEntry<'a>),
    Table(TableEntry<'a>),
}

impl<'a> EntryRef<'a> {
    pub fn is_child(self) -> bool {
        match self {
            EntryRef::String(..) => true,
            EntryRef::Array(arr) => {
                match arr.markup() {
                    ArrayMarkup::Inline => true,
                    ArrayMarkup::Explicit => false
                }
            }
            EntryRef::Table(table) => {
                match table.markup() {
                    TableMarkup::Inline{ .. } =>true,
                    TableMarkup::Implicit
                    | TableMarkup::Explicit{ .. } => false,
                }
            }
        }
    }
}

define_view!(StringValue, FormattedValue);


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
    pub fn markup(&self) -> &ValueMarkup {
        &self.0.markup
    }

    pub fn markup_mut(&mut self) -> &mut ValueMarkup {
        &mut self.0.markup
    }

    pub fn raw(&self) -> &str {
        match self.0.value {
            Value::String(ref data) => &data.raw,
            _ => unreachable!()
        }
    }
}

define_view!(InlineArray, FormattedValue);

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
            Table::Inline(data) => data.get(key),
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
            Table::Inline(ref data) => data.iter(),
            Table::Implicit(ref map) => implicit_table_iter_logical(map),
            Table::Explicit(ref data) => data.iter_logical(),
        }
    }

    pub fn markup(self) -> TableMarkup<'a> {
        match self.data {
            Table::Inline(ref data) => TableMarkup::Inline {
                key: data.key(),
                value: data.markup(),
                comma_trivia: data.get_comma_trivia()
            },
            Table::Implicit(..) => TableMarkup::Implicit,
            Table::Explicit(ref data) => TableMarkup::Explicit {
                leading_trivia: data.get_leading_trivia(),
                keys: data.keys()
            },
        }
    }
}

#[derive(Clone, Copy)]
pub enum TableMarkup<'a> {
    Inline{ key: &'a KeyMarkup, value: &'a ValueMarkup, comma_trivia: &'a str },
    Implicit,
    Explicit{ leading_trivia: &'a str, keys: &'a [TableKeyMarkup] },
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

pub enum ArrayMarkup {
    Inline,
    Explicit
}

impl<'a> ArrayEntry<'a> {
    pub fn markup(&self) -> ArrayMarkup {
        match self.data {
            Array::Inline(..) => ArrayMarkup::Inline,
            Array::Explicit(..) => ArrayMarkup::Explicit
        }
    }

    pub fn len(&self) -> usize {
        match self.data {
            Array::Inline(data) => data.len(),
            Array::Explicit(vec) => vec.len()
        }
    }

    pub fn get(&self, idx: usize) -> EntryRef {
        match self.data {
            Array::Inline(data) => data.get(idx).to_entry(),
            Array::Explicit(vec) => unimplemented!()
        }
    }

    pub fn iter(&self) -> Box<Iterator<Item=(&'a str, EntryRef<'a>)> + 'a> {
        unimplemented!()
    }
}

define_view!(ExplicitArray, Vec<Rc<RefCell<Container>>>);

pub enum EntryRefMut<'a> {
    String(&'a mut StringValue),
    InlineArray(&'a mut InlineArray),
    ArrayOfTables(&'a mut ExplicitArray),
    InlineTable(&'a mut InlineTable),
    ImplicitTable(ImplicitTableEntry<'a>),
    ExplicitTable(ExplicitTableEntry<'a>),
}

impl<'a> EntryRefMut<'a> {
    pub fn is_child(self) -> bool {
        match self {
            EntryRefMut::String(..)
            | EntryRefMut::InlineArray(..)
            | EntryRefMut::InlineTable(..) => true,
            EntryRefMut::ImplicitTable(..)
            | EntryRefMut::ArrayOfTables(..)
            | EntryRefMut::ExplicitTable(.. ) => false
        }
    }
}

define_view!(InlineTable, ValueNode);

impl InlineTable {
    fn data(&self) -> &TableData {
        match self.0.value.value {
            super::Value::InlineTable(ref table_data) => table_data,
            _ => unreachable!()
        }
    }

    fn data_mut(&mut self) -> &mut TableData {
        match self.0.value.value {
            super::Value::InlineTable(ref mut table_data) => table_data,
            _ => unreachable!()
        }
    }

    pub fn get(&self, key: &str) -> Option<EntryRef> {
        self.data().values.direct.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<EntryRefMut> {
        self.data_mut().values.direct.get_mut(key)
    }

    pub fn get_at(&self, idx: usize) -> EntryRef {
        ValueNode::as_cursor(self.data().values.direct.get_at(idx))
    }

    pub fn get_at_mut(&mut self, idx: usize) -> EntryRefMut {
        ValueNode::as_cursor_mut(self.data_mut().values.direct.get_at_mut(idx))
    }

    pub fn len(&self) -> usize {
        self.data().values.direct.len()
    }

    pub fn iter<'a>(&'a self)
                    -> Box<Iterator<Item=(&'a str, EntryRef<'a>)>+'a> {
        self.data().values.direct.iter()
    }

    pub fn markup(&self) -> &ValueMarkup {
        &self.0.value.markup
    }

    pub fn markup_mut(&mut self) -> &mut ValueMarkup {
        &mut self.0.value.markup
    }
    pub fn key(&self) -> &KeyMarkup {
        KeyMarkup::new(&self.0.key)
    }

    pub fn key_mut(&mut self) -> &mut KeyMarkup {
        KeyMarkup::new_mut(&mut self.0.key)
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

pub struct ImplicitTableEntry<'a>(&'a mut HashMap<String, IndirectChild>);

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

impl<'a> ImplicitTableEntry<'a> {
    fn new(src: &'a mut HashMap<String, IndirectChild>) -> ImplicitTableEntry {
        ImplicitTableEntry(src)
    }

    pub fn get(&self, key: &str) -> Option<EntryRef> {
        implicit_table_get(self.0, key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<EntryRefMut> {
        self.0.get_mut(key).map(IndirectChild::as_cursor_mut)
    }

    pub fn len(&self) -> usize {
        implicit_table_len_logical(self.0)
    }

    pub fn iter<'b>(&'b self)
                    -> Box<Iterator<Item=(&'b str, EntryRef<'b>)>+'b> {
        implicit_table_iter_logical(self.0)
    }

    pub fn as_ref(&self) -> TableEntry {
        TableEntry {
            data: Table::Implicit(self.0)
        }
    }
}

#[derive(Clone, Copy)]
pub struct ExplicitTableEntry<'a>(&'a RefCell<Container>);

impl<'a> ExplicitTableEntry<'a> {
    fn new(src: &'a RefCell<Container>) -> ExplicitTableEntry {
        ExplicitTableEntry(src)
    }

    fn borrow<'b>(&'b self) -> &'b Container {
        unsafe { transmute_lifetime(&*self.0.borrow()) }
    }

    fn borrow_mut<'b>(&'b mut self) -> &'b mut Container {
        unsafe { transmute_lifetime_mut(&mut *self.0.borrow_mut()) }
    }

    pub fn get(&self, key: &str) -> Option<EntryRef> {
        self.borrow().get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<EntryRefMut> {
        self.borrow_mut().data.get_mut(key)
    }

    pub fn len_children(&self) -> usize {
        self.borrow().len_children()
    }

    pub fn iter_children(&self) -> DirectChildren {
        self.borrow().data.direct.iter_children()
    }

    pub fn len(&self) -> usize {
        self.borrow().len_logical()
    }

    pub fn iter<'b>(&'b self)
                    -> Box<Iterator<Item=(&'b str, EntryRef<'b>)>+'b> {
        self.borrow().iter_logical()
    }

    pub fn get_leading_trivia(&self) -> &str {
        &self.borrow().get_leading_trivia()
    }

    pub fn set_leading_trivia(&mut self, s: String) {
        KeyMarkup::check_leading_trivia(&*s);
        self.borrow_mut().lead = s;
    }

    pub fn keys(&self) -> &[TableKeyMarkup] {
        self.borrow().keys()
    }

    pub fn keys_mut(&mut self) -> &mut [TableKeyMarkup] {
        TableKeyMarkup::new_slice_mut(&mut*self.borrow_mut().keys)
    }

    pub fn as_ref(&self) -> TableEntry {
        TableEntry {
            data: Table::Explicit(
                unsafe { transmute_lifetime(&self.0.borrow()) }
            )
        }
    }
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