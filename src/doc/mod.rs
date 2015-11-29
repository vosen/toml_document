use std::cell::{RefCell, UnsafeCell, Ref, RefMut};
use std::collections::HashMap;
use std::collections::hash_map::{Entry};
use std::fmt::{Display, Error, Formatter};
use std::rc::Rc;
use std::iter::Map;
use std::slice::Iter;
use std::fmt::Write;
use std::str::Chars;
use std::iter::Peekable;

pub mod parser;
pub mod cursor;

static MALFORMED_LEAD_MSG: &'static str = "Malformed leading trivia";
static MALFORMED_TRAIL_MSG: &'static str = "Malformed trailing trivia";

fn check_ws(s: &str, error: &'static str) {
    for c in s.chars() {
        match c {
            ' ' | '\t' => { },
            _ => panic!(error)
        }
    }
}

fn eat_before_newline(mut chars: Peekable<Chars>) -> Peekable<Chars> {
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

fn eat_newline<'a>(mut chars: Peekable<Chars<'a>>,
               error: &'static str) -> Peekable<Chars<'a>> {
    match chars.next().unwrap_or_else(|| panic!(error)) {
        '\n' => { },
        '\r' => assert!(chars.next() == Some('\n'), error),
        _ => panic!(error)
    }
    chars
}

fn eat_eof(mut chars: Peekable<Chars>, error: &'static str) {
    assert!(chars.next() == None, error);
}

unsafe fn transmute_lifetime<'a, 'b, T>(x: &'a T) -> &'b T {
    ::std::mem::transmute(x)
}

unsafe fn transmute_lifetime_mut<'a, 'b, T>(x: &'a mut T) -> &'b mut T {
    ::std::mem::transmute(x)
}

struct TraversalPosition<'a> {
    direct: Option<&'a mut ValuesMap>,
    indirect: &'a mut HashMap<String, IndirectChild>
}

impl<'a> TraversalPosition<'a> {
    fn from_indirect(map: &mut HashMap<String, IndirectChild>)
                     -> TraversalPosition {
        TraversalPosition {
            direct: None,
            indirect: map 
        }
    }
}

// Main table representing the whole document.
// This structure preserves TOML document and its markup.
// Internally, a document is split in the following way:
//         +
//         |- values
//  a="b"  +
//         +
//  [foo]  |
//  x="y"  |- container_list
//  [bar]  |
//  c="d"  +
//         +
//         |- trail
//         +
pub struct RootTable {
    values: ValuesMap,
    // List of containers: tables and arrays that are present in the document.
    // Stored in the order they appear in the document.
    container_list: Vec<Rc<RefCell<Container>>>,
    // Index for quick traversal.
    container_index: HashMap<String, IndirectChild>,
    trail: String
}

impl RootTable {
    fn new() -> RootTable {
        RootTable {
            values: ValuesMap::new(),
            container_list: Vec::new(),
            container_index: HashMap::new(),
            trail: String::new(),
        }
    }

    // Converts editable document to a simplified representation.
    fn simplify(self) -> super::Table {
        self.values
            .simplify().into_iter()
            .chain(as_simplified_vec(&self.container_index))
            .collect()
    }

    pub fn serialize(&self, buf: &mut String) {
        self.values.serialize(buf);
        for table in self.container_list.iter() {
            table.borrow().serialize(buf);
        }
        buf.push_str(&*self.trail);
    }

    fn traverse(&mut self) -> TraversalPosition {
        TraversalPosition {
            direct: Some(&mut self.values),
            indirect: &mut self.container_index
        }
    }

    pub fn get(&self, key: &str) -> Option<cursor::ValueRef> {
        self.values
            .get(key)
            .or_else(|| {
                self.container_index
                    .get(key)
                    .map_or(None, |child| Some(child.as_cursor()))
            })
    }

    pub fn get_mut(&mut self, key: &str) -> Option<cursor::ValueRefMut> {
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
}

// Order-preserving map of values that are directly contained in
// a root table, table or an array.
// A map is represented in the following way:
//         +
//         |- kvp_list[0]
//  a="b"  +
//         +
//         |- kvp_list[1]
//  x="y"  +
//         +
//         |- trail
//         +
struct ValuesMap {
    // key-value pairs stored in the order they appear in a document
    kvp_list: Vec<Rc<RefCell<ValueNode>>>,
    // Index for quick traversal.
    kvp_index: HashMap<String, Rc<RefCell<ValueNode>>>,
    trail: String
}

impl ValuesMap {
    fn new() -> ValuesMap {
        ValuesMap {
            kvp_list: Vec::new(),
            kvp_index: HashMap::new(),
            trail: String::new()
        }
    }

    fn insert(&mut self, key: FormattedKey, value: FormattedValue) -> bool {
        let key_text = key.escaped.clone();
        let node = Rc::new(RefCell::new(ValueNode{
            key: key,
            value: value
        }));
        match self.kvp_index.entry(key_text) {
            Entry::Occupied(_) => return false,
            Entry::Vacant(entry) => {
                entry.insert(node.clone())
            }
        };
        self.kvp_list.push(node);
        true
    }

    fn set_last_value_trail(&mut self, s: String) {
        self.kvp_list
            .last()
            .as_ref()
            .unwrap()
            .borrow_mut()
            .value
            .markup
            .trail = s;
    }

    fn simplify(&self) -> Vec<(String, super::Value)> {
        self.kvp_list
            .iter()
            .map(|ref node| {
                let node = node.borrow();
                (node.key.escaped.clone(), node.value.value.as_simple_value())
            })
            .collect()
    }

    fn serialize(&self, buf: &mut String) {
        for ref node in self.kvp_list.iter() {
            let node = node.borrow();
            node.key.serialize(buf);
            buf.push('=');
            node.value.serialize(buf);
        }
    }

    fn serialize_inline(&self, buf: &mut String) {
        for (idx, ref node) in self.kvp_list.iter().enumerate() {
            let node = node.borrow();
            node.key.serialize(buf);
            buf.push('=');
            node.value.serialize(buf);
            if idx < self.kvp_list.len() - 1 {
                buf.push(',');
            }
        }
    }

    fn get(&self, key: &str) -> Option<cursor::ValueRef> {
        self.kvp_index
            .get(key)
            .map(|val| { ValueNode::as_cursor(val) })
    }

    fn get_mut(&mut self, key: &str) -> Option<cursor::ValueRefMut> {
        self.kvp_index
            .get_mut(key)
            .map(|val| { ValueNode::as_cursor_mut(val) })
    }

    fn len(&self) -> usize {
        self.kvp_index.len()
    }
}

// Value plus leading and trailing auxiliary text.
//   a  =  "qwertyu" \n
// +---+ +------------+
//   |         |
//  key      value
#[doc(hidden)] pub
struct ValueNode {
    key: FormattedKey,
    value: FormattedValue
}

impl ValueNode {
    fn as_cursor<'a>(r: &'a RefCell<Self>) -> cursor::ValueRef<'a> {
        cursor::ValueRef::_new_string(&*r.borrow())
    }

    fn as_cursor_mut<'a>(r: &'a RefCell<Self>) -> cursor::ValueRefMut<'a> {
        cursor::ValueRefMut::_new_string(&mut*r.borrow_mut())
    }
}

// Value plus leading and trailing auxiliary text.
// a =            "qwertyu"          \n
//    +----------++-------++----------+
//         |          |          |
//    markup.lead   value  markup.trail
#[doc(hidden)] pub
struct FormattedValue {
    value: Value,
    markup: ValueMarkup
}

impl FormattedValue {
    fn new(lead: String, v: Value) -> FormattedValue {
        FormattedValue {
            value: v,
            markup: ValueMarkup {
                lead: lead,
                trail: String::new()
            }
        }
    }

    fn serialize(&self, buf: &mut String) {
        buf.push_str(&*self.markup.lead);
        self.value.serialize(buf);
        buf.push_str(&*self.markup.trail);
    }
}

pub struct ValueMarkup {
    // auxiliary text between the equality sign and the value
    lead: String,
    // auxiliary text after the value, up to and including the first newline
    trail: String
}

impl ValueMarkup {
    pub fn new(lead: String, trail: String) -> ValueMarkup {
        let mut value = ValueMarkup {
            lead: String::new(),
            trail: String::new(),
        };
        value.set_leading_trivia(lead);
        value.set_trailing_trivia(trail);
        value
    }

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

struct StringData {
    escaped: String,
    raw: String
}

impl StringData {
    fn get(&self) -> &str {
        &*self.escaped
    }

    fn set_checked(&mut self, s: String) {
        self.raw = StringData::unescape(&*s);
        self.escaped = s;
    }

    fn unescape(s: &str) -> String {
        let mut buffer = String::with_capacity(s.len() + 2);
        buffer.push('"');
        for c in s.chars() {
            match c as u32 {
                0x08 => buffer.push_str(r#"\b"#),
                0x09 => buffer.push_str(r#"\t"#),
                0x0A => buffer.push_str(r#"\n"#),
                0x0C => buffer.push_str(r#"\f"#),
                0x0D => buffer.push_str(r#"\r"#),
                0x22 => buffer.push_str(r#"\""#),
                0x5C => buffer.push_str(r#"\\"#),
                c if c <= 0x1F => drop(write!(buffer, r#"\u{:04X}"#, c)),
                _ => buffer.push(c as char)
            }
        }
        buffer.push('"');
        buffer
    }
}

struct TableData {
    values: ContainerData,
    trail: String
}

enum Value {
    String(StringData),
    Integer { parsed: i64, raw: String },
    Float { parsed: f64, raw: String },
    Boolean(bool),
    Datetime(String),
    Array { values: Vec<FormattedValue>, comma_trail: String },
    InlineTable(TableData)
}

impl Value {
    fn new_table(map: ValuesMap, trail: String) -> Value {
        Value::InlineTable(
            TableData { 
                values: ContainerData {
                    direct: map,
                    indirect: HashMap::new()
                },
                trail: trail
            }
        )
    }

    fn as_simple_value(&self) -> super::Value {
        match self {
            &Value::String(StringData { ref escaped, .. }) => {
                super::Value::String(escaped.clone())
            }
            &Value::Integer { parsed, .. } => super::Value::Integer(parsed),
            &Value::Float { parsed, .. } => super::Value::Float(parsed),
            &Value::Boolean(x) => super::Value::Boolean(x),
            &Value::Datetime(ref x) => super::Value::Datetime(x.clone()),
            &Value::Array { ref values, .. } => {
                let values = values
                    .iter()
                    .map(|fv| fv.value.as_simple_value())
                    .collect();
                super::Value::Array(values)
            }
            &Value::InlineTable(TableData { ref values, .. }) => { 
                super::Value::Table(values.simplify().into_iter().collect())
            },
        }
    }

    fn type_str(&self) -> &'static str {
        match *self {
            Value::String(..) => "string",
            Value::Integer {..} => "integer",
            Value::Float {..} => "float",
            Value::Boolean(..) => "boolean",
            Value::Datetime(..) => "datetime",
            Value::Array {..} => "array",
            Value::InlineTable(..) => "table",
        }
    }

    fn is_table(&self) -> bool {
        match *self {
            Value::InlineTable(..) => true,
            _ => false
        }
    }

    fn as_table(&mut self) -> &mut ContainerData {
        match *self {
            Value::InlineTable(TableData { ref mut values, .. }) => values,
            _ => panic!()
        }
    }

    fn serialize(&self, buf: &mut String) {
        match *self {
            Value::String(StringData { ref raw, .. }) => buf.push_str(raw),
            Value::Integer { ref raw, .. } => buf.push_str(raw),
            Value::Float { ref raw, .. } => buf.push_str(raw),
            Value::Boolean(b) => buf.push_str(if b {"true"} else {"false"}),
            Value::Datetime(ref s) => buf.push_str(s),
            Value::Array { ref values, ref comma_trail } => {
                buf.push('[');
                for (idx, value) in values.iter().enumerate() {
                    value.serialize(buf);
                    if idx != values.len() - 1 { buf.push(',') }
                }
                buf.push_str(comma_trail);
                buf.push(']');
            }
            Value::InlineTable(TableData { ref values, ref trail }) => {
                buf.push('{');
                values.direct.serialize_inline(buf);
                buf.push_str(trail);
                buf.push('}');
            }
        }
    }
}

// Entry in the document index. This index is used for traversal (which is
// heavily used during parsing and adding new elements) and does not preserve
// ordering, just the structure 
// Some examples:
//  [a.b]
//  x="y"
// Document above contains single implicit table [a], which in turn contains
// single explicit table [a.b].
//  [a.b]
//  x="y"
//  [a]
// Document above contains single explicit table [a], which in turn contains
// single explicit table [a.b].
enum IndirectChild {
    ImplicitTable(HashMap<String, IndirectChild>),
    ExplicitTable(Rc<RefCell<Container>>),
    Array(Vec<Rc<RefCell<Container>>>)
}

impl IndirectChild {
    fn as_implicit(&mut self) -> &mut HashMap<String, IndirectChild> {
        if let IndirectChild::ImplicitTable(ref mut m) = *self { m }
        else { panic!() }
    }

    fn to_implicit(self) -> HashMap<String, IndirectChild> {
        if let IndirectChild::ImplicitTable(m) = self { m }
        else { panic!() }
    }

    fn simplify(&self) -> super::Value {
        match self {
            &IndirectChild::ImplicitTable(ref m) => {
                let kvp_vec = as_simplified_vec(m);
                super::Value::Table(kvp_vec.into_iter().collect())
            }
            &IndirectChild::ExplicitTable(ref m) => m.borrow().simplify(),
            &IndirectChild::Array(ref vec) => {
                let values = vec
                    .iter()
                    .map(|m| m.borrow().simplify())
                    .collect();
                super::Value::Array(values)
            }
        }
    }

    fn is_implicit(&self) -> bool {
        match *self {
            IndirectChild::ImplicitTable (..) => true,
            _ => false
        }
    }

    fn as_cursor(&self) -> cursor::ValueRef {
        match self {
            &IndirectChild::ImplicitTable(ref map) => {
                cursor::ValueRef::Table(
                    cursor::TableRef {
                        data: cursor::Table::Implicit(map)
                    }
                )
            }
            &IndirectChild::ExplicitTable(ref container) => {
                cursor::ValueRef::Table(
                    cursor::TableRef {
                        data: cursor::Table::Explicit(container.borrow())
                    }
                )
            }
            &IndirectChild::Array(ref data) => unimplemented!()
        }
    }

    fn as_cursor_mut(&mut self) -> cursor::ValueRefMut {
        match self {
            &mut IndirectChild::ImplicitTable(ref mut map) => {
                cursor::ValueRefMut::Table(
                    cursor::TableRefMut {
                        data: cursor::TableMut::Implicit(map)
                    }
                )
            }
            &mut IndirectChild::ExplicitTable(ref container) => {
                cursor::ValueRefMut::Table(
                    cursor::TableRefMut {
                        data: cursor::TableMut::Explicit(&container)
                    }
                )
            }
            &mut IndirectChild::Array(ref data) => unimplemented!()
        }
    }

    fn get(&self, key: &str) -> Option<cursor::ValueRef> {
        match self {
            &IndirectChild::ImplicitTable(ref map) => {
                map.get(key).map(IndirectChild::as_cursor)
            }
            &IndirectChild::ExplicitTable(ref container) => {
                unsafe { transmute_lifetime(&container.borrow().data) }.get(key)
            }
            &IndirectChild::Array(ref data) => unimplemented!()
        }
    }

    fn get_mut(&mut self, key: &str) -> Option<cursor::ValueRefMut> {
        match *self {
            IndirectChild::ImplicitTable(ref mut map) => {
                map.get_mut(key).map(IndirectChild::as_cursor_mut)
            }
            IndirectChild::ExplicitTable(ref mut container) => {
                let container_data = &mut container.borrow_mut().data;
                unsafe { transmute_lifetime_mut(container_data) }.get_mut(key)
            }
            IndirectChild::Array(ref data) => unimplemented!()
        }
    }
}

struct Container {
    data: ContainerData,
    // Path to the table, eg:
    //  [   a   .   b   ]
    //   +-----+ +-----+
    //      |       |
    //   keys[0] keys[1]
    keys: Vec<FormattedKey>,
    kind: ContainerKind,
    lead: String,
}

impl Container {
    fn new_array(data: ContainerData, ks: Vec<FormattedKey>, lead: String)
                     -> Container {
        Container { 
            data: data,
            keys: ks,
            lead: lead,
            kind: ContainerKind::Array,
        }
    }

    fn new_table(data: ContainerData, ks: Vec<FormattedKey>, lead: String)
                     -> Container {
        Container { 
            data: data,
            keys: ks,
            lead: lead,
            kind: ContainerKind::Table,
        }
    }

    fn serialize(&self, buf: &mut String) {
        buf.push_str(&*self.lead);
        if self.keys.len() > 0 {
            match self.kind {
                ContainerKind::Table => buf.push_str("["),
                ContainerKind::Array => buf.push_str("[["),
            }
            for (i, key) in self.keys.iter().enumerate() {
                key.serialize(buf);
                if i < self.keys.len() - 1 { buf.push('.') }
            }
            match self.kind {
                ContainerKind::Table => buf.push_str("]"),
                ContainerKind::Array => buf.push_str("]]"),
            }
        }
        self.data.serialize(buf);
    }

    fn simplify(&self) -> super::Value {
        super::Value::Table(self.data.simplify().into_iter().collect())
    }
}

// Direct children are key-values that appear below container declaration,
// indirect children are are all other containers that are logically defined
// inside the container. For example:
//  [a]
//  x="y"
// [a.b]
// q="w"
// In the document above, table container [a] contains single direct child
// (x="y") and single indirect child (table container [a.b])
struct ContainerData {
    direct: ValuesMap,
    indirect: HashMap<String, IndirectChild>
}

impl ContainerData {
    fn new() -> ContainerData {
        ContainerData {
            direct: ValuesMap::new(),
            indirect: HashMap::new()
        }
    }

    fn serialize(&self, buf: &mut String) {
        self.direct.serialize(buf);
    }

    fn simplify(&self) -> Vec<(String, super::Value)> {
        self.direct
            .simplify()
            .into_iter()
            .chain(as_simplified_vec(&self.indirect))
            .collect()
    }

    fn traverse(&mut self) -> TraversalPosition {
        TraversalPosition {
            direct: Some(&mut self.direct),
            indirect: &mut self.indirect
        }
    }

    fn get(&self, key: &str) -> Option<cursor::ValueRef> {
        self.direct
            .get(key)
            .or_else(|| {
                self.indirect
                    .get(key)
                    .map_or(None, |child| child.get(key))
            })
    }

    fn get_mut(&mut self, key: &str) -> Option<cursor::ValueRefMut> {
        if let some @ Some(_) =  self.direct.get_mut(key) {
            return some;
        };
        self.indirect
            .get_mut(key)
            .map_or(None, |child| child.get_mut(key))
    }

    fn len(&self) -> usize {
        self.direct.len() + self.indirect.len()
    }
}

#[derive(PartialEq, Eq, Hash)]
enum ContainerKind {
    Table,
    Array,
}

pub struct FormattedKey {
    escaped: String,
    raw: Option<String>,
    markup: KeyMarkup
}

struct KeyMarkup {
    lead: String,
    trail: String
}

impl FormattedKey {
    fn new(lead: String, key: (String, Option<String>), trail: String) -> FormattedKey {
        FormattedKey {
            escaped: key.0,
            raw: key.1,
            markup: KeyMarkup {
                lead: lead,
                trail: trail,
            }
        }
    }

    fn serialize(&self, buf: &mut String) {
        buf.push_str(&*self.markup.lead);
        match self.raw {
            Some(ref str_buf) => buf.push_str(&*str_buf),
            None => buf.push_str(&*self.escaped)
        };
        buf.push_str(&*self.markup.trail);
    }
}

fn as_simplified_vec(map: &HashMap<String, IndirectChild>)
                     -> Vec<(String, super::Value)> {
    map.iter().map(|(k, c)|(k.clone(), c.simplify())).collect()
}

trait Serializable {
    fn serialize(&self, buf: &mut String);
}

#[cfg(test)]
mod tests {
    use Parser;

    macro_rules! test_round_trip {
        ($name: ident, $text: expr) => (
            #[test]
            fn $name() {
                let mut p = Parser::new($text);
                let table = p.parse_doc().unwrap();
                let mut buf = String::new();
                table.serialize(&mut buf);
                if $text != buf {
                    panic!(format!("expected:\n{}\nactual:\n{}\n", $text, buf));
                }
            }
        )
    }

    test_round_trip!(empty, "  #asd \n ");
    test_round_trip!(single_table, "  #asd\t  \n [a]\n \t \n\n  #asdasdad\n ");
    test_round_trip!(root_key, " a = \"b\" \n ");
    test_round_trip!(array_with_values, " #as \n  \n  [[ a .b ]] \n  a = 1\n ");
    test_round_trip!(escaped, " str = \"adas \\\"Quote me\\\".sdas\" ");
    test_round_trip!(literal_string, " str = 'C:\\Users\\nodejs\\templates' ");
    test_round_trip!(array_empty," foo = [   ] ");
    test_round_trip!(array_non_empty, " foo = [ 1 , 2 ] ");
    test_round_trip!(array_trailing_comma, " foo = [ 1 , 2 , ] ");
    test_round_trip!(integer_with_sign, " foo = +10 ");
    test_round_trip!(underscore_integer, " foo = 1_000 ");
    test_round_trip!(inline_table, "\n a = { x = \"foo\"  , y = \"bar\"\t } ");
    test_round_trip!(linebrak_array, "foo = [\n  \"bar\", \n  \"baz\" \n ]");
}