extern crate toml_document;

use toml_document::{Document, ValueRef, DirectChild, IntegerValue};
use toml_document::{Container, ContainerKind, InlineTable, EntryRef, TableValue};

use std::iter;

// These tests make sure that automatically generated trivias for newly inserted
// or deleted elements are "nice" (eg. no trailing or leading newlines in the document

fn assert_value_eq(c1: &DirectChild, c2: &DirectChild) {
    match c1.value() {
        ValueRef::Integer(i1) => match c2.value() {
            ValueRef::Integer(i2) => assert_integer_eq(i1, i2),
            _ => panic!()
        },
        ValueRef::Table(t1) => match c2.value() {
            ValueRef::Table(t2) => assert_table_eq(t1, t2),
            _ => panic!()
        },
        _ => unreachable!()
    }
}

fn assert_integer_eq(v1: &IntegerValue, v2: &IntegerValue) {
    assert_eq!(v1.markup().get_leading_trivia(),
               v2.markup().get_leading_trivia());
    assert_eq!(v1.markup().get_trailing_trivia(),
               v2.markup().get_trailing_trivia());
}

fn assert_table_eq(t1: &InlineTable, t2: &InlineTable ) {
    assert_eq!(t1.markup().get_leading_trivia(),
               t2.markup().get_leading_trivia());
    assert_eq!(t1.markup().get_trailing_trivia(),
               t2.markup().get_trailing_trivia());
    for (c1, c2) in t1.iter().zip(t2.iter()) {
        assert_child_eq(c1, c2);
    }
}

fn assert_child_eq(c1: &DirectChild, c2: &DirectChild) {
    assert_eq!(c1.key().get_leading_trivia(),
               c2.key().get_leading_trivia());
    assert_eq!(c1.key().get_trailing_trivia(),
               c2.key().get_trailing_trivia());
    assert_eq!(c1.key().raw(),
               c2.key().raw());
    assert_value_eq(c1, c2);
}

fn assert_container_eq(c1: &Container, c2: &Container) {
    assert_eq!(c1.keys().get_leading_trivia(),
               c2.keys().get_leading_trivia());
    for (k1, k2) in c1.keys().markup().iter().zip(c2.keys().markup().iter()) {
        assert_eq!(k1.get_leading_trivia(),
                   k2.get_leading_trivia());
        assert_eq!(k1.get_trailing_trivia(),
                   k2.get_trailing_trivia());
        assert_eq!(k1.raw(),
                   k2.raw());
    }
    assert_eq!(c1.keys().get_trailing_trivia(),
               c2.keys().get_trailing_trivia());
    for (c1, c2) in c1.iter_children().zip(c2.iter_children()) {
        assert_child_eq(c1, c2);
    }
}

macro_rules! compare {
    ($name: ident, $text: expr, $builder: ident) => (
        #[test]
        fn $name() {
            let parsed = Document::parse($text).unwrap();
            let built : Document = $builder();
            assert_eq!(parsed.len_children(), built.len_children());
            for (c1, c2) in parsed.iter_children().zip(built.iter_children()) {
                assert_child_eq(c1, c2);
            }
            assert_eq!(parsed.len_containers(), built.len_containers());
            for (c1, c2) in parsed.iter_containers().zip(built.iter_containers()) {
                assert_container_eq(c1, c2);
            }
            assert_eq!(parsed.get_trailing_trivia(), built.get_trailing_trivia());
        }
    )
}

compare!(insert_single, "foo = 0", _insert_single);
fn _insert_single() -> Document {
    let mut built = Document::new();
    built.insert_integer(0, "foo".to_owned(), 0);
    built
}

compare!(insert_in_order, "foo = 0\nbar = 1", _insert_in_order);
fn _insert_in_order() -> Document {
    let mut built = Document::new();
    built.insert_integer(0, "foo".to_owned(), 0);
    built.insert_integer(1, "bar".to_owned(), 1);
    built
}

compare!(insert_out_of_order, "foo = 0\nbar = 1", _insert_out_of_order);
fn _insert_out_of_order() -> Document {
    let mut built = Document::new();
    built.insert_integer(0, "bar".to_owned(), 1);
    built.insert_integer(0, "foo".to_owned(), 0);
    built
}

compare!(insert_single_container, "[foo]", _insert_single_container);
fn _insert_single_container() -> Document {
    let mut built = Document::new();
    built.insert_container(0, vec!("foo".to_owned()).into_iter(), ContainerKind::Table);
    built
}

compare!(insert_in_order_container, "[foo]\n[bar]", _insert_in_order_container);
fn _insert_in_order_container() -> Document {
    let mut built = Document::new();
    built.insert_container(0, vec!("foo".to_owned()).into_iter(), ContainerKind::Table);
    built.insert_container(1, vec!("bar".to_owned()).into_iter(), ContainerKind::Table);
    built
}

compare!(insert_out_of_order_container, "[foo]\n[bar]", _insert_out_of_order_container);
fn _insert_out_of_order_container() -> Document {
    let mut built = Document::new();
    built.insert_container(0, vec!("bar".to_owned()).into_iter(), ContainerKind::Table);
    built.insert_container(0, vec!("foo".to_owned()).into_iter(), ContainerKind::Table);
    built
}

compare!(insert_into_inline_array, "foo = { a = 0, c = 1 }", _insert_into_inline);
fn _insert_into_inline() -> Document {
    let mut built = Document::new();
    {
        let mut table = built.insert_inline_table(0, "foo".to_owned());
        table.insert_integer(0, "a".to_owned(), 0);
        table.insert_integer(1, "c".to_owned(), 1);
    }
    built
}

#[test]
fn pass_trivia_to_value() {
    let text = "\na=\"b\"#IMPORTANT\n c=10";
    let mut doc = Document::parse(text).unwrap();
    doc.remove_preserve_trivia(0);
    assert_eq!("\n#IMPORTANT\n ", doc.get_child(0).key().get_leading_trivia());
}

#[test]
fn pass_trivia_to_container() {
    let text = "\ta=\"b\"\t \n [foo]";
    let mut doc = Document::parse(text).unwrap();
    doc.remove_preserve_trivia(0);
    assert_eq!("\t\t \n ", doc.get_container(0).keys().get_leading_trivia());
}

#[test]
fn pass_trivia_to_document() {
    let text = "\t\r\na=\"b\"\r\n";
    let mut doc = Document::parse(text).unwrap();
    doc.remove_preserve_trivia(0);
    assert_eq!("\t\r\n\r\n", doc.get_trailing_trivia());
}

#[test]
fn remove_middle_container() {
    let text = "[[a.b]]\n\t[[a.b.c]]\n[[a.b.c]]";
    let mut doc = Document::parse(text).unwrap();
    assert_eq!(3, doc.len_containers());
    doc.remove(1);
    assert_eq!(2, doc.len_containers());
    assert_eq!("\n", doc.get_container(1).keys().get_leading_trivia());
}

#[test]
fn remove_last_container() {
    let text = "[[a.b]]\n\t[[a.b.c]]\n[[a.b.c]]";
    let mut doc = Document::parse(text).unwrap();
    assert_eq!(3, doc.len_containers());
    doc.remove(2);
    assert_eq!(2, doc.len_containers());
    assert_eq!("\n\t", doc.get_container(1).keys().get_leading_trivia());
}

#[test]
fn add_to_empty_container() {
    let text = "[package]";
    let mut doc = Document::parse(text).unwrap();
    {
        let container = doc.insert_container(1, iter::once("test"), ContainerKind::ArrayMember);
        container.insert_string(0, "name", "bar");
    }
    assert_eq!("\n", doc.get_container(1).keys().get_trailing_trivia());
    assert_eq!("[package]\n[[test]]\nname = \"bar\"\n", doc.to_string());
}

#[test]
fn pass_trivia_to_container_from_container() {
    let text = "\n\n\n[foo]\na=\"b\"\t\t\n[bar]";
    let mut doc = Document::parse(text).unwrap();
    doc.remove_preserve_trivia(0);
    assert_eq!("\n\n\n\t\t\n", doc.get_container(0).keys().get_leading_trivia());
}

#[test]
fn pass_trivia_to_document_container() {
    let text = "\n\t   [foo]\na=\"b\"\n";
    let mut doc = Document::parse(text).unwrap();
    doc.remove_preserve_trivia(0);
    assert_eq!("\n\t   \n", doc.get_trailing_trivia());
}

#[test]
fn remove_implicits_from_inline() {
    let text = concat!("[a]\n",
                       "b = { x = \"foo\" }\n",
                       "[a.b.y]\n",
                       "z = \"bar\"");
    let mut doc = Document::parse(text).unwrap();
    doc.remove(1);
    assert_eq!(1, doc.len());
    match lookup(&doc, &["a", "b"]).unwrap() {
        EntryRef::Table(table) => {
            match table.to_value() {
                TableValue::Inline(..) => { }
                _ => panic!()
            }
        }
        _ => panic!()
    }
    assert_eq!("[a]\nb = { x = \"foo\" }\n", doc.to_string());
}

#[test]
fn lookup_implicit() {
    let text = concat!("[a]\n",
                       "b = { x = \"foo\" }\n",
                       "[a.b.y]\n",
                       "z = \"bar\"");
    let mut doc = Document::parse(text).unwrap();
    match lookup(&doc, &["a", "b", "y", "z"]).unwrap() {
        EntryRef::String(..) => { }
        _ => panic!()
    }
}

#[test]
fn remove_inline_from_implicits() {
    let text = concat!("[a]\n",
                       "b = { x = \"foo\" }\n",
                       "[a.b.y]\n",
                       "z = \"bar\"");
    let mut doc = Document::parse(text).unwrap();
    doc.remove(0);
    assert_eq!(1, doc.len());
    assert_eq!("[a.b.y]\nz = \"bar\"", doc.to_string());
    assert_implicit(&doc, &["a"]);
    assert_implicit(&doc, &["a", "b"]);
}

fn assert_implicit<'a>(doc: &'a Document, path: &'a [&'a str]) {
    match lookup(&doc, path).unwrap() {
        EntryRef::Table(table) => {
            match table.to_value() {
                TableValue::Implicit => { }
                TableValue::Inline(..) => {
                    panic!("[{}]: Expected implicit table, got inline", path.join("."))
                }
                TableValue::Explicit(..) => {
                    panic!("[{}]: Expected implicit table, got explicit", path.join("."))
                }
            }
        }
        _ => panic!("[{}]: Expected table, got something else", path.join("."))
    }
}

fn lookup<'a>(doc: &'a Document, path: &'a [&'a str]) -> Option<EntryRef<'a>> {
    fn lookup_inner<'a>(entry: EntryRef<'a>, path: &'a [&'a str]) -> Option<EntryRef<'a>> {
        if path.len() == 0 {
            Some(entry)
        } else {
            println!("loking for {}", path[0]);
            match entry {
                EntryRef::Table(table) => {
                    println!("table.len() = {}", table.len());
                    for (k, _) in table.iter() {
                        println!("key: {:?}", k);
                    }
                    table.get(path[0]).and_then(|entry| lookup_inner(entry, &path[1..]))
                }
                _ => {
                    None
                }
            }
        }
    }
    println!("{}", path[0]);
    doc.get(path[0]).and_then(|entry| lookup_inner(entry, &path[1..]))
}