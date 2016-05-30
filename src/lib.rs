#![doc(html_root_url = "http://vosen.github.io/toml_document/toml_document")]
//#![deny(missing_docs)]
#![cfg_attr(test, deny(warnings))]

#![deny(unused_variables)]
#![deny(dead_code)]
#![deny(unused_imports)]

use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::collections::hash_map::{Entry};
use std::rc::Rc;
use std::fmt::Write;
use std::str::Chars;
use std::iter::{Peekable};

mod parser;
mod public;
mod display;

pub use self::parser::ParserError;
pub use public::*;
pub use display::*;

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

fn escape_string(value: &str) -> String {
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
pub struct Document {
    values: ValuesMap,
    // List of containers: tables and arrays that are present in the document.
    // Stored in the order they appear in the document.
    container_list: Vec<Rc<RefCell<Container>>>,
    // Index for quick traversal.
    container_index: HashMap<String, IndirectChild>,
    trail: String
}

impl Document {
    fn traverse(&mut self) -> TraversalPosition {
        TraversalPosition {
            direct: Some(&mut self.values),
            indirect: &mut self.container_index
        }
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
struct ValuesMap {
    // key-value pairs stored in the order they appear in a document
    kvp_list: Vec<Rc<RefCell<ValueNode>>>,
    // Index for quick traversal.
    kvp_index: HashMap<String, Rc<RefCell<ValueNode>>>,
}

impl ValuesMap {
    fn new() -> ValuesMap {
        ValuesMap {
            kvp_list: Vec::new(),
            kvp_index: HashMap::new(),
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

    fn get_at_mut(&mut self, idx: usize) -> RefMut<ValueNode> {
        self.kvp_list[idx].borrow_mut()
    }
}

// Value plus leading and trailing auxiliary text.
//   a  =  "qwertyu" \n
// +---+ +------------+
//   |         |
//  key      value
struct ValueNode {
    key: FormattedKey,
    value: FormattedValue
}

// Value plus leading and trailing auxiliary text.
// a =            "qwertyu"          \n
//    +----------++-------++----------+
//         |          |          |
//    markup.lead   value  markup.trail
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
}

pub struct ValueMarkup {
    // auxiliary text between the equality sign and the value
    lead: String,
    // auxiliary text after the value, up to and including the first newline
    trail: String
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
    comma_trail: String
}

enum Value {
    String(StringData),
    Integer { parsed: i64, raw: String },
    Float { parsed: f64, raw: String },
    Boolean(bool),
    Datetime(String),
    InlineArray(InlineArrayData),
    InlineTable(TableData)
}

struct InlineArrayData {
    values: Vec<Box<FormattedValue>>,
    comma_trail: String
}

impl Value {
    fn new_table(map: ValuesMap, trail: String) -> Value {
        Value::InlineTable(
            TableData { 
                values: ContainerData {
                    direct: map,
                    indirect: HashMap::new()
                },
                comma_trail: trail
            }
        )
    }

    fn type_str(&self) -> &'static str {
        match *self {
            Value::String(..) => "string",
            Value::Integer {..} => "integer",
            Value::Float {..} => "float",
            Value::Boolean(..) => "boolean",
            Value::Datetime(..) => "datetime",
            Value::InlineArray(..) => "array",
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

    fn is_implicit(&self) -> bool {
        match *self {
            IndirectChild::ImplicitTable (..) => true,
            _ => false
        }
    }
}

pub struct Container {
    // Path to the table, including leading trivia 
    // and trailing trivia up to a newline, eg:
    //  \n                     +
    //  \n                     |- keys
    //  [   a   .   b   ]   \n +
    keys: ContainerKeys,
    data: ContainerData,
    kind: ContainerKind,
}

impl Container {
    fn new_array(data: ContainerData, ks: Vec<FormattedKey>, lead: String)
                     -> Container {
        Container { 
            data: data,
            keys: ContainerKeys::new(lead, ks),
            kind: ContainerKind::ArrayMember,
        }
    }

    fn new_table(data: ContainerData, ks: Vec<FormattedKey>, lead: String)
                     -> Container {
        Container { 
            data: data,
            keys: ContainerKeys::new(lead, ks),
            kind: ContainerKind::Table,
        }
    }
}

struct ContainerKeys {
    // trivia before `self.vec`
    lead: String,
    // Path to the table, eg:
    //  [   a   .   b   ]
    //   +-----+ +-----+
    //      |       |
    //   vec[0]   vec[1]
    vec: Vec<FormattedKey>,
    // trivia up to and including the first newline after `self.vec`
    trail: String,
}

impl ContainerKeys {
    fn new(lead: String, ks: Vec<FormattedKey>) -> ContainerKeys {
        ContainerKeys {
            lead: lead,
            vec: ks,
            trail: String::new()
        }
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
#[doc(hidden)]
pub struct ContainerData {
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

    fn traverse(&mut self) -> TraversalPosition {
        TraversalPosition {
            direct: Some(&mut self.direct),
            indirect: &mut self.indirect
        }
    }
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
pub enum ContainerKind {
    Table, 
    ArrayMember,
}

#[doc(hidden)]
pub struct FormattedKey {
    escaped: String,
    raw: Option<String>,
    markup: PrivKeyMarkup
}

struct PrivKeyMarkup {
    lead: String,
    trail: String
}

impl FormattedKey {
    fn new(lead: String, key: (String, Option<String>), trail: String) -> FormattedKey {
        FormattedKey {
            escaped: key.0,
            raw: key.1,
            markup: PrivKeyMarkup {
                lead: lead,
                trail: trail,
            }
        }
    }

    fn new_escaped(key: String) -> FormattedKey {
        let raw = escape_string(&key);
        let raw = if raw.len() == key.len() + 2 { None } else { Some(raw) };
        FormattedKey {
            escaped: key,
            raw: raw,
            markup: PrivKeyMarkup {
                lead: String::new(),
                trail: String::new(),
            }
        }
    }
}