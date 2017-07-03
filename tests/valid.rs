extern crate serde_json;
extern crate toml_document;

use serde_json::Value as Json;
use serde_json::Map as JsonMap;

use toml_document::{EntryRef, Document};

type LogicalValues<'a> = Box<Iterator<Item = (&'a str, EntryRef<'a>)> + 'a>;

fn val_to_json((key, value): (&str, EntryRef)) -> (String, Json) {
  fn typed_json(s: &str, json: Json) -> Json {
    let mut map = JsonMap::new();
        map.insert("type".to_owned(), Json::String(s.into()));
        map.insert("value".to_owned(), json);
        Json::Object(map)
  }
  match value {
    EntryRef::String(s) => {
        let json_string = Json::String(s.get().to_owned());
        (key.to_owned(), typed_json("string", json_string))
    }
    EntryRef::Integer(i) => {
        let json_string = Json::String(format!("{}", i.get()));
        (key.to_owned(), typed_json("integer", json_string))
    }
    EntryRef::Float(f) => {
        let json_string = Json::String({
            let s = format!("{:.15}", f.get());
            let s = s.trim_right_matches('0').to_owned();
            if s.ends_with('.') {format!("{}0", s)} else {s}
        });
        (key.to_owned(), typed_json("float", json_string))
    }
    EntryRef::Boolean(b) => {
        let json_string = Json::String(format!("{}", b.get()));
        (key.to_owned(), typed_json("bool", json_string))
    }
    EntryRef::Datetime(d) => {
        let json_string = Json::String(d.get().to_owned());
        (key.to_owned(), typed_json("datetime", json_string))
    }
    EntryRef::Array(arr) => {
        let is_table = match arr.iter().next() {
        Some(EntryRef::Table(..)) => true,
            _ => false,
        };
        let json = Json::Array(arr.iter().map(|e| val_to_json(("", e)).1).collect::<Vec<_>>());
        if is_table {
            (key.to_owned(), json)
        } else {
            (key.to_owned(), typed_json("array", json))
        }
    }
    EntryRef::Table(ref table) => {
      (key.to_owned(), to_json(table.iter()))
    },
  }
}

fn to_json(iter: LogicalValues) -> Json {
    Json::Object(iter.map(val_to_json).collect())
}

fn run(toml: &str, json: &str) {
    let doc = Document::parse(toml);
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // compare logical structure with jsons
    let json: Json = serde_json::from_str(json).unwrap();
    let toml_json = to_json(doc.iter());
    assert_eq!(json, toml_json);

    // check indexability of children
    for (idx, child) in doc.iter_children().enumerate() {
        assert_eq!(idx, doc.find(child).unwrap())
    }

    // check indexability of containers
    for (idx, container) in doc.iter_containers().enumerate() {
        assert_eq!(idx + doc.len_children(),
                   doc.find(container).unwrap())
    }

    // check round-trip equality
    assert_eq!(&doc.to_string(), toml);
}

macro_rules! test( ($name:ident, $toml:expr, $json:expr) => (
    #[test]
    fn $name() { run($toml, $json); }
) );

test!(array_empty,
       include_str!("valid/array-empty.toml"),
       include_str!("valid/array-empty.json"));
test!(array_nospaces,
       include_str!("valid/array-nospaces.toml"),
       include_str!("valid/array-nospaces.json"));
test!(arrays_hetergeneous,
       include_str!("valid/arrays-hetergeneous.toml"),
       include_str!("valid/arrays-hetergeneous.json"));
test!(arrays,
       include_str!("valid/arrays.toml"),
       include_str!("valid/arrays.json"));
test!(arrays_nested,
       include_str!("valid/arrays-nested.toml"),
       include_str!("valid/arrays-nested.json"));
test!(empty,
       include_str!("valid/empty.toml"),
       include_str!("valid/empty.json"));
test!(bool,
       include_str!("valid/bool.toml"),
       include_str!("valid/bool.json"));
test!(datetime,
       include_str!("valid/datetime.toml"),
       include_str!("valid/datetime.json"));
test!(example,
       include_str!("valid/example.toml"),
       include_str!("valid/example.json"));
test!(float,
       include_str!("valid/float.toml"),
       include_str!("valid/float.json"));
test!(implicit_and_explicit_after,
       include_str!("valid/implicit-and-explicit-after.toml"),
       include_str!("valid/implicit-and-explicit-after.json"));
test!(implicit_and_explicit_before,
       include_str!("valid/implicit-and-explicit-before.toml"),
       include_str!("valid/implicit-and-explicit-before.json"));
test!(implicit_groups,
       include_str!("valid/implicit-groups.toml"),
       include_str!("valid/implicit-groups.json"));
test!(integer,
       include_str!("valid/integer.toml"),
       include_str!("valid/integer.json"));
test!(key_equals_nospace,
       include_str!("valid/key-equals-nospace.toml"),
       include_str!("valid/key-equals-nospace.json"));
test!(key_special_chars,
       include_str!("valid/key-special-chars.toml"),
       include_str!("valid/key-special-chars.json"));
test!(key_with_pound,
       include_str!("valid/key-with-pound.toml"),
       include_str!("valid/key-with-pound.json"));
test!(long_float,
       include_str!("valid/long-float.toml"),
       include_str!("valid/long-float.json"));
test!(long_integer,
       include_str!("valid/long-integer.toml"),
       include_str!("valid/long-integer.json"));
test!(string_empty,
       include_str!("valid/string-empty.toml"),
       include_str!("valid/string-empty.json"));
test!(string_escapes,
       include_str!("valid/string-escapes.toml"),
       include_str!("valid/string-escapes.json"));
test!(string_simple,
       include_str!("valid/string-simple.toml"),
       include_str!("valid/string-simple.json"));
test!(string_with_pound,
       include_str!("valid/string-with-pound.toml"),
       include_str!("valid/string-with-pound.json"));
test!(table_array_implicit,
       include_str!("valid/table-array-implicit.toml"),
       include_str!("valid/table-array-implicit.json"));
test!(table_array_many,
       include_str!("valid/table-array-many.toml"),
       include_str!("valid/table-array-many.json"));
test!(table_array_nest,
       include_str!("valid/table-array-nest.toml"),
       include_str!("valid/table-array-nest.json"));
test!(table_array_one,
       include_str!("valid/table-array-one.toml"),
       include_str!("valid/table-array-one.json"));
test!(table_empty,
       include_str!("valid/table-empty.toml"),
       include_str!("valid/table-empty.json"));
test!(table_sub_empty,
       include_str!("valid/table-sub-empty.toml"),
       include_str!("valid/table-sub-empty.json"));
test!(table_whitespace,
       include_str!("valid/table-whitespace.toml"),
       include_str!("valid/table-whitespace.json"));
test!(table_with_pound,
       include_str!("valid/table-with-pound.toml"),
       include_str!("valid/table-with-pound.json"));
test!(unicode_escape,
       include_str!("valid/unicode-escape.toml"),
       include_str!("valid/unicode-escape.json"));
test!(unicode_literal,
       include_str!("valid/unicode-literal.toml"),
       include_str!("valid/unicode-literal.json"));
test!(hard_example,
       include_str!("valid/hard_example.toml"),
       include_str!("valid/hard_example.json"));
test!(example2,
       include_str!("valid/example2.toml"),
       include_str!("valid/example2.json"));
test!(example3,
       include_str!("valid/example-v0.3.0.toml"),
       include_str!("valid/example-v0.3.0.json"));
test!(example4,
       include_str!("valid/example-v0.4.0.toml"),
       include_str!("valid/example-v0.4.0.json"));
