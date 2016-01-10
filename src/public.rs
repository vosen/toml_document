use std::cell::{RefCell, Ref};
use std::collections::HashMap;
use std::rc::Rc;
use std::slice;

use super::{check_ws, eat_before_newline, eat_newline};
use super::{MALFORMED_LEAD_MSG, MALFORMED_TRAIL_MSG};

use super::{TableData, Container, IndirectChild};
use super::{Value, ValueNode, ValueMarkup, FormattedKey, Document, ValuesMap};
use super::{ContainerKind};

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

fn direct_cursor<'a>((k, v): (&'a String, &'a Rc<RefCell<ValueNode>>)) -> (&'a str, ValueRef<'a>) {
    (k, ValueNode::as_cursor(v))
}

fn indirect_cursor<'a>((k, v): (&'a String, &'a IndirectChild)) -> (&'a str, ValueRef<'a>) {
    (k, v.as_cursor())
}

fn iter_logical<'a>(direct: &'a ValuesMap,
                    indirect: &'a HashMap<String, IndirectChild>)
                    -> Box<Iterator<Item = (&'a str, ValueRef<'a>)> + 'a> {
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

    pub fn get(&self, key: &str) -> Option<ValueRef> {
        self.values
            .get(key)
            .or_else(|| {
                self.container_index
                    .get(key)
                    .map_or(None, |child| Some(child.as_cursor()))
            })
    }

    pub fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
        if let some @ Some(_)  = self.values.get_mut(key) {
            return some;
        };
        self.container_index
            .get_mut(key)
            .map_or(None, |child| child.get_mut(key))
    }

    pub fn len_children(&self) -> usize {
        self.values.len()
    }

    pub fn iter_children(&self) -> ChildrenValues {
        self.values.iter_children()
    }

    pub fn len_logical(&self) -> usize {
        self.values.len() + self.container_index.len()
    }

    pub fn iter_logical<'a>(&'a self)
                            -> Box<Iterator<Item=(&'a str, ValueRef<'a>)>+'a> {
        iter_logical(&self.values, &self.container_index)
    }

    pub fn get_at(&self, idx: usize) -> ValueRef {
        if idx < self.values.len() {
            ValueNode::as_cursor(self.values.get_at(idx))
        } else {
            Container::as_cursor(&*self.container_list[idx - self.values.len()])
        }
    }

    pub fn get_at_mut(&mut self, idx: usize) -> ValueRefMut {
        if idx < self.values.len() {
            ValueNode::as_cursor_mut(self.values.get_at_mut(idx))
        } else {
            Container::as_cursor_mut(&*self.container_list[idx - self.values.len()])
        }
    }

    pub fn len_syntactic(&self) -> usize {
        self.values.len() + self.container_list.len()
    }

    pub fn iter_syntactic(&self) -> SyntacticTableValues {
        SyntacticTableValues {
            table: self,
            idx: 0
        }
    }

    pub fn get_trailing_trivia(&self) -> &str {
        &self.trail
    }
}

pub struct ChildrenValues<'a> {
    iter: Option<slice::Iter<'a, Rc<RefCell<ValueNode>>>>
}

impl<'a> Iterator for ChildrenValues<'a> {
    type Item = ValueRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter {
            Some(ref mut iter) => {
                iter.next().map(|v| ValueNode::as_cursor(v))
            }
            None => None
        }
    }
}

pub struct SyntacticTableValues<'a> {
    table: &'a Document,
    idx: usize,
}

impl<'a> Iterator for SyntacticTableValues<'a> {
    type Item = ValueRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.table.len_syntactic() {
            return None
        }
        let ret_val = Some(self.table.get_at(self.idx));
        self.idx += 1;
        ret_val
    }
}

impl ValuesMap {
    fn get(&self, key: &str) -> Option<ValueRef> {
        self.kvp_index
            .get(key)
            .map(|val| { ValueNode::as_cursor(val) })
    }

    fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
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

    fn iter<'a>(&'a self) -> Box<Iterator<Item=(&'a str, ValueRef<'a>)>+'a> {
        let iter = self.kvp_index
                       .iter()
                       .map(direct_cursor);
        Box::new(iter)
    }

    fn iter_children(&self) -> ChildrenValues {
        ChildrenValues {
            iter: Some(self.kvp_list.iter())
        }
    }
}

impl ValueNode {
    fn as_cursor<'a>(r: &'a RefCell<Self>) -> ValueRef<'a> {
        match r.borrow().value.value  {
            Value::String(..) => {
                ValueRef::String(
                    StringNode::new(
                        unsafe { transmute_lifetime(&*r.borrow()) }
                    )
                )
            }
            _ => unimplemented!()
        }
    }

    fn as_cursor_mut<'a>(r: &'a RefCell<Self>) -> ValueRefMut<'a> {
        let value = { &r.borrow().value.value };
        match *value {
            Value::String(..) => {
                ValueRefMut::String(
                    StringNode::new_mut(
                        unsafe { transmute_lifetime_mut(&mut*r.borrow_mut()) }
                    )
                )
            }
            _ => unimplemented!()
        }
    }
}

impl Container {
    fn as_cursor<'a>(r: &'a RefCell<Self>) -> ValueRef<'a> {
        let kind = { r.borrow().kind };
        match kind {
            ContainerKind::Table => {
                ValueRef::Table(
                    TableRef {
                        data: Table::Explicit(r.borrow())
                    }
                )
            }
            ContainerKind::Array => unimplemented!()
        }
    }

    fn as_cursor_mut<'a>(r: &'a RefCell<Self>) -> ValueRefMut<'a> {
        let kind = { r.borrow().kind };
        match kind {
            ContainerKind::Table => {
                ValueRefMut::ExplicitTable(
                    ExplicitTableRef::new(r)
                )
            }
            ContainerKind::Array => unimplemented!()
        }
    }

    fn get(&self, key: &str) -> Option<ValueRef> {
        self.data.get(key)
    }

    fn len_logical(&self) -> usize {
        self.data.direct.len() + self.data.indirect.len()
    }

    fn len_children(&self) -> usize {
        self.data.direct.len()
    }

    fn iter_children(&self) -> ChildrenValues {
        self.data.direct.iter_children()
    }

    fn iter_logical<'a>(&'a self)
                        -> Box<Iterator<Item=(&'a str, ValueRef<'a>)>+'a> {
        self.data.iter_logical()
    }

    fn get_leading_trivia(&self) -> &str {
        &self.lead
    }

    fn keys(&self) -> &[TableKeyMarkup] {
        TableKeyMarkup::new_slice(&self.keys)
    }
}

impl super::ContainerData {
    fn get(&self, key: &str) -> Option<ValueRef> {
        self.direct
            .get(key)
            .or_else(|| {
                self.indirect
                    .get(key)
                    .map_or(None, |child| child.get(key))
            })
    }

    fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
        if let some @ Some(_) =  self.direct.get_mut(key) {
            return some;
        };
        self.indirect
            .get_mut(key)
            .map_or(None, |child| child.get_mut(key))
    }

    fn iter_logical<'a>(&'a self)
                        -> Box<Iterator<Item=(&'a str, ValueRef<'a>)>+'a> {
        iter_logical(&self.direct, &self.indirect)
    }
}

impl IndirectChild {
    fn as_cursor(&self) -> ValueRef {
        match self {
            &IndirectChild::ImplicitTable(ref map) => {
                ValueRef::Table(
                    TableRef {
                        data: Table::Implicit(map)
                    }
                )
            }
            &IndirectChild::ExplicitTable(ref container) => {
                ValueRef::Table(
                    TableRef {
                        data: Table::Explicit(container.borrow())
                    }
                )
            }
            &IndirectChild::Array(..) => unimplemented!()
        }
    }

    fn as_cursor_mut(&mut self) -> ValueRefMut {
        match self {
            &mut IndirectChild::ImplicitTable(ref mut map) => {
                ValueRefMut::ImplicitTable(
                    ImplicitTableRef::new(map)
                )
            }
            &mut IndirectChild::ExplicitTable(ref container) => {
                ValueRefMut::ExplicitTable(
                    ExplicitTableRef::new(container)
                )
            }
            &mut IndirectChild::Array(..) => unimplemented!()
        }
    }

    fn get(&self, key: &str) -> Option<ValueRef> {
        match self {
            &IndirectChild::ImplicitTable(ref map) => {
                map.get(key).map(IndirectChild::as_cursor)
            }
            &IndirectChild::ExplicitTable(ref container) => {
                unsafe { transmute_lifetime(&container.borrow().data) }.get(key)
            }
            &IndirectChild::Array(..) => unimplemented!()
        }
    }

    fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
        match *self {
            IndirectChild::ImplicitTable(ref mut map) => {
                map.get_mut(key).map(IndirectChild::as_cursor_mut)
            }
            IndirectChild::ExplicitTable(ref mut container) => {
                let container_data = &mut container.borrow_mut().data;
                unsafe { transmute_lifetime_mut(container_data) }.get_mut(key)
            }
            IndirectChild::Array(..) => unimplemented!()
        }
    }
}

pub enum ValueRef<'a> {
    String(&'a StringNode),
    Table(TableRef<'a>),
}

impl<'a> ValueRef<'a> {
    pub fn is_child(&self) -> bool {
        match *self {
            ValueRef::String(..) => true,
            ValueRef::Table(ref table) => {
                match table.markup() {
                    TableMarkup::Inline{ .. } =>true,
                    TableMarkup::Implicit
                    | TableMarkup::Explicit{ .. } => false,
                }
            }
        }
    }
}

define_view!(StringNode, ValueNode);

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
        KeyMarkup::new(&self.0.key)
    }

    pub fn key_mut(&mut self) -> &mut KeyMarkup {
        KeyMarkup::new_mut(&mut self.0.key)
    }

    pub fn raw(&self) -> &str {
        match self.0.value.value {
            Value::String(ref data) => &data.raw,
            _ => unreachable!()
        }
    }
}

pub struct TableRef<'a> {
    data: Table<'a>
}

enum Table<'a> {
    Inline(&'a InlineTable),
    Implicit(&'a HashMap<String, IndirectChild>),
    Explicit(Ref<'a, Container>)
}

impl<'a> TableRef<'a> {
    pub fn get(&self, key: &str) -> Option<ValueRef> {
        match self.data {
            Table::Inline(ref data) => data.get(key),
            Table::Implicit(ref map) => implicit_table_get(map, key),
            Table::Explicit(ref data) => data.get(key),
        }
    }

    pub fn len_children(&self) -> usize {
        match self.data {
            Table::Inline(ref data) => data.len(),
            Table::Implicit(..) => 0,
            Table::Explicit(ref data) => data.len_children(),
        }
    }

    pub fn iter_children(&self) -> ChildrenValues {
        match self.data {
            Table::Inline(ref tbl) => tbl.data().values.direct.iter_children(),
            Table::Implicit(..) => ChildrenValues { iter: None },
            Table::Explicit(ref data) => data.iter_children(),
        }
    }

    pub fn len(&self) -> usize {
        match self.data {
            Table::Inline(ref data) => data.len(),
            Table::Implicit(ref map) => implicit_table_len_logical(map),
            Table::Explicit(ref data) => data.len_logical(),
        }
    }

    pub fn iter<'b>(&'b self) 
                    -> Box<Iterator<Item=(&'b str, ValueRef<'b>)>+'b> {
        match self.data {
            Table::Inline(ref data) => data.iter(),
            Table::Implicit(ref map) => implicit_table_iter_logical(map),
            Table::Explicit(ref data) => data.iter_logical(),
        }
    }

    pub fn markup(&self) -> TableMarkup {
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

pub enum TableMarkup<'a> {
    Inline{ key: &'a KeyMarkup, value: &'a ValueMarkup, comma_trivia: &'a str },
    Implicit,
    Explicit{ leading_trivia: &'a str, keys: &'a [TableKeyMarkup] },
}

pub enum ValueRefMut<'a> {
    String(&'a mut StringNode),
    InlineTable(&'a mut InlineTable),
    ImplicitTable(ImplicitTableRef<'a>),
    ExplicitTable(ExplicitTableRef<'a>),
}

impl<'a> ValueRefMut<'a> {
    pub fn is_child(&self) -> bool {
        match *self {
            ValueRefMut::String(..)
            | ValueRefMut::InlineTable(..) => true,
            ValueRefMut::ImplicitTable(..)
            | ValueRefMut::ExplicitTable(.. ) => false
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

    pub fn get(&self, key: &str) -> Option<ValueRef> {
        self.data().values.direct.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
        self.data_mut().values.direct.get_mut(key)
    }

    pub fn get_at(&self, idx: usize) -> ValueRef {
        ValueNode::as_cursor(self.data().values.direct.get_at(idx))
    }

    pub fn get_at_mut(&mut self, idx: usize) -> ValueRefMut {
        ValueNode::as_cursor_mut(self.data_mut().values.direct.get_at_mut(idx))
    }

    pub fn len(&self) -> usize {
        self.data().values.direct.len()
    }

    pub fn iter<'a>(&'a self)
                    -> Box<Iterator<Item=(&'a str, ValueRef<'a>)>+'a> {
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

    pub fn as_ref(&self) -> TableRef {
        TableRef {
            data: Table::Inline(self)
        }
    }
}

pub struct ImplicitTableRef<'a>(&'a mut HashMap<String, IndirectChild>);

fn implicit_table_get<'a>(t: &'a HashMap<String, IndirectChild>,
                          key: &str) -> Option<ValueRef<'a>> {
    t.get(key).map(IndirectChild::as_cursor)
}

fn implicit_table_len_logical<'a>(t: &'a HashMap<String, IndirectChild>)
                                  -> usize {
    t.len()
}

fn implicit_table_iter_logical<'a>(t: &'a HashMap<String, IndirectChild>)
                                   -> Box<Iterator<Item=(&'a str, ValueRef<'a>)> + 'a> {
    let iter = t.iter().map(indirect_cursor);
    Box::new(iter)
}

impl<'a> ImplicitTableRef<'a> {
    fn new(src: &'a mut HashMap<String, IndirectChild>) -> ImplicitTableRef {
        ImplicitTableRef(src)
    }

    pub fn get(&self, key: &str) -> Option<ValueRef> {
        implicit_table_get(self.0, key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
        self.0.get_mut(key).map(IndirectChild::as_cursor_mut)
    }

    pub fn len(&self) -> usize {
        implicit_table_len_logical(self.0)
    }

    pub fn iter<'b>(&'b self)
                    -> Box<Iterator<Item=(&'b str, ValueRef<'b>)>+'b> {
        implicit_table_iter_logical(self.0)
    }

    pub fn as_ref(&self) -> TableRef {
        TableRef {
            data: Table::Implicit(self.0)
        }
    }
}

pub struct ExplicitTableRef<'a>(&'a RefCell<Container>);

impl<'a> ExplicitTableRef<'a> {
    fn new(src: &'a RefCell<Container>) -> ExplicitTableRef {
        ExplicitTableRef(src)
    }

    fn borrow<'b>(&'b self) -> &'b Container {
        unsafe { transmute_lifetime(&*self.0.borrow()) }
    }

    fn borrow_mut<'b>(&'b mut self) -> &'b mut Container {
        unsafe { transmute_lifetime_mut(&mut *self.0.borrow_mut()) }
    }

    pub fn get(&self, key: &str) -> Option<ValueRef> {
        self.borrow().get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<ValueRefMut> {
        self.borrow_mut().data.get_mut(key)
    }

    pub fn len_children(&self) -> usize {
        self.borrow().len_children()
    }

    pub fn iter_children(&self) -> ChildrenValues {
        self.borrow().data.direct.iter_children()
    }

    pub fn len(&self) -> usize {
        self.borrow().len_logical()
    }

    pub fn iter<'b>(&'b self)
                    -> Box<Iterator<Item=(&'b str, ValueRef<'b>)>+'b> {
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

    pub fn as_ref(&self) -> TableRef {
        TableRef {
            data: Table::Explicit(self.0.borrow())
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