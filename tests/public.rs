extern crate toml_document;

use toml_document::{Document, ValueRef, Parser, DirectChild, IntegerValue};
use toml_document::{Container, ContainerKind};

// These tests make sure that automatically generated trivias for newly inserted
// or deleted elements are "nice" (eg. no trailing or leading newlines in the document

fn unwrap_integer(c: &DirectChild) -> &IntegerValue {
    match c.value() {
        ValueRef::Integer(v) => v,
        _ => unreachable!()
    }
}

fn assert_child_eq(c1: &DirectChild, c2: &DirectChild) {
    assert_eq!(c1.key().get_leading_trivia(),
               c2.key().get_leading_trivia());
    assert_eq!(c1.key().get_trailing_trivia(),
               c2.key().get_trailing_trivia());
    assert_eq!(c1.key().raw(),
               c2.key().raw());
    let v1 = unwrap_integer(c1);
    let v2 = unwrap_integer(c2);
    assert_eq!(v1.markup().get_leading_trivia(),
               v2.markup().get_leading_trivia());
    assert_eq!(v1.markup().get_trailing_trivia(),
               v2.markup().get_trailing_trivia());
}

fn assert_container_eq(c1: &Container, c2: &Container) {
    for (k1, k2) in c1.keys().iter().zip(c2.keys().iter()) {
        assert_eq!(k1.get_leading_trivia(),
                   k2.get_leading_trivia());
        assert_eq!(k1.get_trailing_trivia(),
                   k2.get_trailing_trivia());
        assert_eq!(k1.raw(),
                   k2.raw());
    }
    for (c1, c2) in c1.iter_children().zip(c2.iter_children()) {
        assert_child_eq(c1, c2);
    }
    assert_eq!(c1.get_leading_trivia(),
               c2.get_leading_trivia())
}

macro_rules! compare {
    ($name: ident, $text: expr, $builder: ident) => (
        #[test]
        fn $name() {
            let parsed = Parser::new($text).parse().unwrap();
            let built = $builder();
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