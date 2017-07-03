use std::ascii::AsciiExt;
use std::char;
use std::collections::hash_map::{Entry, OccupiedEntry, VacantEntry};
use std::collections::{HashMap};
use std::error::Error;
use std::fmt;
use std::str;
use std::cell::{RefCell};
use std::rc::Rc;

use super::{Container, ContainerKind, ContainerData, FormattedValue, FormattedKey, IndirectChild}; 
use super::{Document, ValuesMap, TraversalPosition, InlineArrayData};
use super::Value as DocValue;
use super::{StringData, TableData};

macro_rules! try {
    ($e:expr) => (match $e { Some(s) => s, None => return None })
}

/// Parser for converting a string to a TOML `Value` instance.
///
/// This parser contains the string slice that is being parsed, and exports the
/// list of errors which have occurred during parsing.
pub struct Parser<'a> {
    input: &'a str,
    cur: str::CharIndices<'a>,
    aux_range: Option<(usize, usize)>,

    /// A list of all errors which have occurred during parsing.
    ///
    /// Not all parse errors are fatal, so this list is added to as much as
    /// possible without aborting parsing. If `None` is returned by `parse`, it
    /// is guaranteed that this list is not empty.
    pub errors: Vec<ParserError>,
}

/// A structure representing a parse error.
///
/// The data in this structure can be used to trace back to the original cause
/// of the error in order to provide diagnostics about parse errors.
#[derive(Debug)]
pub struct ParserError {
    /// The low byte at which this error is pointing at.
    lo: usize,
    /// One byte beyond the last character at which this error is pointing at.
    hi: usize,
    /// A human-readable description explaining what the error is.
    pub desc: String,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Parser<'a> {
        Parser {
            input: s,
            cur: s.char_indices(),
            errors: Vec::new(),
            aux_range: None,
        }
    }

    /// Converts a byte offset from an error message to a (line, column) pair
    ///
    /// All indexes are 0-based.
    #[allow(dead_code)]
    pub fn to_linecol(&self, offset: usize) -> (usize, usize) {
        let mut cur = 0;
        for (i, line) in self.input.lines().enumerate() {
            if cur + line.len() > offset {
                return (i, offset - cur)
            }
            cur += line.len() + 1;
        }
        (self.input.lines().count(), 0)
    }

    fn next_pos(&self) -> usize {
        self.cur.clone().next().map(|p| p.0).unwrap_or_else(|| self.input.len())
    }

    // Returns true and consumes the next character if it matches `ch`,
    // otherwise do nothing and return false
    fn eat(&mut self, ch: char) -> bool {
        match self.peek(0) {
            Some((_, c)) if c == ch => { self.cur.next(); true }
            Some(_) | None => false,
        }
    }

    // Peeks ahead `n` characters
    fn peek(&self, n: usize) -> Option<(usize, char)> {
        self.cur.clone().nth(n)
    }

    fn expect(&mut self, ch: char) -> bool {
        if self.eat(ch) { return true }
        let mut it = self.cur.clone();
        let lo = it.next().map(|p| p.0).unwrap_or_else(|| self.input.len());
        let hi = it.next().map(|p| p.0).unwrap_or_else(|| self.input.len());
        self.errors.push(ParserError {
            lo: lo,
            hi: hi,
            desc: match self.cur.clone().next() {
                Some((_, c)) => format!("expected `{}`, but found `{}`", ch, c),
                None => format!("expected `{}`, but found eof", ch)
            }
        });
        false
    }

    // Consumes whitespace ('\t' and ' ') until another character (or EOF) is
    // reached. Returns if any whitespace was consumed
    fn ws(&mut self) -> bool {
        let mut ret = false;
        loop {
            match self.peek(0) {
                Some((_, '\t')) |
                Some((_, ' ')) => { self.cur.next(); ret = true; }
                _ => break,
            }
        }
        ret
    }

    // Consumes the rest of the line after a comment character
    fn comment(&mut self) -> bool {
        if !self.eat('#') { return false }
        for (_, ch) in self.cur.by_ref() {
            if ch == '\n' { break }
        }
        true
    }

    // Consumes a newline if one is next
    fn newline(&mut self) -> Option<&str> {
        let start = self.next_pos();
        match self.peek(0) {
            Some((_, '\n')) => { 
                self.cur.next();
                Some(&self.input[start..self.next_pos()])
            }
            Some((_, '\r')) if self.peek(1).map(|c| c.1) == Some('\n') => {
                self.cur.next();
                self.cur.next();
                Some(&self.input[start..self.next_pos()])
            }
            _ => None
        }
    }

    fn skip_trivia(&mut self) {
        self.aux_range = Some(self.aux_range.unwrap_or_else(|| {
            let start = self.next_pos();
            loop {
                self.ws();
                if self.newline().is_some() { continue }
                if self.comment() { continue }
                break;
            }
            (start, self.next_pos())
        }));
    }

    fn eat_trivia(&mut self) -> String {
        self.skip_trivia();
        self.take_trivia()
    }

    fn eat_to_newline(&mut self) -> String {
        let start = self.next_pos();
        self.ws();
        self.newline();
        self.comment();
        self.input[start..self.next_pos()].to_string()
    }

    fn eat_ws(&mut self) -> String {
        let start = self.next_pos();
        self.ws();
        self.input[start..self.next_pos()].to_string()
    }

    fn take_trivia(&mut self) -> String {
        self.aux_range
            .take()
            .map(|(start, end)| &self.input[start..end])
            .unwrap_or("")
            .to_string()
    }

    /// TODO: write something here
    pub fn parse(&mut self) -> Option<Document> {
        let mut ret = Document::new();
        while self.peek(0).is_some() {
            self.skip_trivia();
            if self.eat('[') {
                let container_aux = self.take_trivia();
                let array = self.eat('[');
                self.next_pos();

                // Parse the name of the section
                let mut keys = Vec::new();
                loop {
                    let key_lead_aux = self.eat_ws();
                    if let Some(s) = self.key_name() {
                        keys.push(FormattedKey::new(key_lead_aux, s, self.eat_ws()));
                    }
                    if self.eat(']') {
                        if array && !self.expect(']') { return None }
                        break
                    }
                    if !self.expect('.') { return None }
                }
                if keys.is_empty() { return None }

                // Build the section table
                let mut container = ContainerData::new();
                if !self.values(&mut container.direct) { return None };
                let cnt_len = ret.container_list.len();
                let maybe_error = if array {
                    Parser::_insert_array(&mut ret,
                                          cnt_len,
                                          keys,
                                          container,
                                          container_aux)
                } else {
                    Parser::_insert_table(&mut ret,
                                          cnt_len,
                                          keys,
                                          container,
                                          container_aux)
                };
                if let Some(error) = maybe_error {
                    self.errors.push(error);
                } else {
                    let trail = self.eat_to_newline();
                    ret.container_list[cnt_len].borrow_mut().keys.trail = trail;
                }
            } else if !self.values(&mut ret.values) {
                 return None;
            }
        }
        if !self.errors.is_empty() {
            None
        } else {
            ret.trail = self.take_trivia();
            Some(ret)
        }
    }

    // Parse a single key name starting at `start`
    fn key_name(&mut self) -> Option<(String, Option<String>)> {
        let start = self.next_pos();
        let raw;
        let key = if self.eat('"') {
            let escaped = self.finish_string(start, false);
            raw = Some(&self.input[start..self.next_pos()]);
            escaped
        } else {
            let mut ret = String::new();
            for (_, ch) in self.cur.clone() {
                match ch {
                    'a' ... 'z' |
                    'A' ... 'Z' |
                    '0' ... '9' |
                    '_' | '-' => { self.cur.next(); ret.push(ch) }
                    _ => break,
                }
            }
            raw = None;
            Some(ret)
        };
        match key {
            Some(ref name) if name.is_empty() => {
                self.errors.push(ParserError {
                    lo: start,
                    hi: start,
                    desc: "expected a key but found an empty string".into(),
                });
                None
            }
            Some(name) => Some((name, raw.map(|x| x.to_string()))),
            None => None,
        }
    }

    // Parses the values into the given ValuesMap.
    // Returns true in case of success and false in case of error.
    fn values(&mut self, into: &mut ValuesMap) -> bool {
        loop {
            self.skip_trivia();
            match self.peek(0) {
                Some((_, '[')) | None => return true,
                Some(..) => {}
            }
            let key_lo = self.next_pos();
            let key = match self.key_name() {
                Some(s) => s,
                None => return false
            };
            let key = FormattedKey::new(self.take_trivia(), key, self.eat_ws());
            if !self.expect('=') { return false }
            let value = match self.value() {
                Some(value) => value,
                None => return false,
            };
            self.insert(into, key, value, key_lo);
            into.set_last_value_trail(self.eat_to_newline());
        }
    }

    // Parses a value
    fn value(&mut self) -> Option<FormattedValue> {
        let leading_ws = self.eat_ws().to_string();
        let value = match self.cur.clone().next() {
            Some((pos, '"')) => self.string(pos),
            Some((pos, '\'')) => self.literal_string(pos),
            Some((pos, 't')) |
            Some((pos, 'f')) => self.boolean(pos),
            Some((pos, '[')) => self.array(pos),
            Some((pos, '{')) => self.inline_table(pos),
            Some((pos, '-')) |
            Some((pos, '+')) => self.number_or_datetime(pos),
            Some((pos, ch)) if is_digit(ch) => self.number_or_datetime(pos),
            _ => {
                let mut it = self.cur.clone();
                let lo = it.next().map(|p| p.0).unwrap_or_else(|| self.input.len());
                let hi = it.next().map(|p| p.0).unwrap_or_else(|| self.input.len());
                self.errors.push(ParserError {
                    lo: lo,
                    hi: hi,
                    desc: "expected a value".into(),
                });
                return None
            }
        };
        value.map(|v| FormattedValue::new(leading_ws, v))
    }

    // Parses a single or multi-line string
    fn string(&mut self, start: usize) -> Option<DocValue> {
        if !self.expect('"') { return None }
        let mut multiline = false;

        // detect multiline literals, but be careful about empty ""
        // strings
        if self.eat('"') {
            if self.eat('"') {
                multiline = true;
                self.newline();
            } else {
                // empty
                return Some(DocValue::String(StringData {
                    escaped: String::new(),
                    raw: "\"\"".to_string()
                }))
            }
        }

        self.finish_string(start, multiline).map(|x|
            DocValue::String(StringData { 
                escaped: x,
                raw: self.input[start..self.next_pos()].to_string()
            })
        )
    }

    // Finish parsing a basic string after the opening quote has been seen
    fn finish_string(&mut self,
                     start: usize,
                     multiline: bool) -> Option<String> {
        let mut ret = String::new();
        loop {
            while multiline && self.newline().is_some() { ret.push('\n') }
            match self.cur.next() {
                Some((_, '"')) => {
                    if multiline {
                        if !self.eat('"') { ret.push_str("\""); continue }
                        if !self.eat('"') { ret.push_str("\"\""); continue }
                    }
                    return Some(ret)
                }
                Some((pos, '\\')) => {
                    if let Some(c) = escape(self, pos, multiline) {
                        ret.push(c);
                    }
                }
                Some((pos, ch)) if ch < '\u{1f}' => {
                    self.errors.push(ParserError {
                        lo: pos,
                        hi: pos + 1,
                        desc: format!("control character `{}` must be escaped",
                                      ch.escape_default().collect::<String>())
                    });
                }
                Some((_, ch)) => ret.push(ch),
                None => {
                    self.errors.push(ParserError {
                        lo: start,
                        hi: self.input.len(),
                        desc: "unterminated string literal".into(),
                    });
                    return None
                }
            }
        }

        fn escape(me: &mut Parser, pos: usize, multiline: bool) -> Option<char> {
            if multiline && me.newline().is_some() {
                while me.ws() || me.newline().is_some() { /* ... */ }
                return None
            }
            match me.cur.next() {
                Some((_, 'b')) => Some('\u{8}'),
                Some((_, 't')) => Some('\u{9}'),
                Some((_, 'n')) => Some('\u{a}'),
                Some((_, 'f')) => Some('\u{c}'),
                Some((_, 'r')) => Some('\u{d}'),
                Some((_, '"')) => Some('\u{22}'),
                Some((_, '\\')) => Some('\u{5c}'),
                Some((pos, c @ 'u')) |
                Some((pos, c @ 'U')) => {
                    let len = if c == 'u' {4} else {8};
                    let num = &me.input[pos+1..];
                    let num = if num.len() >= len && num.is_ascii() {
                        &num[..len]
                    } else {
                        "invalid"
                    };
                    if let Ok(n) = u32::from_str_radix(num, 16) {
                        if let Some(c) = char::from_u32(n) {
                            me.cur.by_ref().nth(len - 1);
                            return Some(c)
                        } else {
                            me.errors.push(ParserError {
                                lo: pos + 1,
                                hi: pos + 5,
                                desc: format!("codepoint `{:x}` is \
                                               not a valid unicode \
                                               codepoint", n),
                            })
                        }
                    } else {
                        me.errors.push(ParserError {
                            lo: pos,
                            hi: pos + 1,
                            desc: format!("expected {} hex digits \
                                           after a `{}` escape", len, c),
                        })
                    }
                    None
                }
                Some((pos, ch)) => {
                    let next_pos = me.next_pos();
                    me.errors.push(ParserError {
                        lo: pos,
                        hi: next_pos,
                        desc: format!("unknown string escape: `{}`",
                                      ch.escape_default().collect::<String>()),
                    });
                    None
                }
                None => {
                    me.errors.push(ParserError {
                        lo: pos,
                        hi: pos + 1,
                        desc: "unterminated escape sequence".into(),
                    });
                    None
                }
            }
        }
    }

    fn literal_string(&mut self, start: usize) -> Option<DocValue> {
        if !self.expect('\'') { return None }
        let mut multiline = false;
        let mut newline = None;
        let mut ret = String::new();

        // detect multiline literals
        if self.eat('\'') {
            if self.eat('\'') {
                multiline = true;
                newline = self.newline().map(|x| x.to_string());
            } else {
                return Some(DocValue::String(StringData {
                    raw: "''".to_string(),
                    escaped: ret
                }))
            }
        }

        loop {
            if !multiline && self.newline().is_some() {
                let next = self.next_pos();
                self.errors.push(ParserError {
                    lo: start,
                    hi: next,
                    desc: "literal strings cannot contain newlines".into(),
                });
                return None
            }
            match self.cur.next() {
                Some((_, '\'')) => {
                    if multiline {
                        if !self.eat('\'') { ret.push_str("'"); continue }
                        if !self.eat('\'') { ret.push_str("''"); continue }
                    }
                    break
                }
                Some((_, ch)) => ret.push(ch),
                None => {
                    self.errors.push(ParserError {
                        lo: start,
                        hi: self.input.len(),
                        desc: "unterminated string literal".into(),
                    });
                    return None
                }
            }
        }
        let raw = if multiline { 
            format!("'''{}{}'''", newline.unwrap_or_default(), ret)
        } else {
            format!("'{}'", ret)
        };
        Some(DocValue::String(StringData { raw: raw, escaped: ret }))
    }

    fn number_or_datetime(&mut self, start: usize) -> Option<DocValue> {
        let mut is_float = false;
        let prefix = try!(self.integer(start, false, true));
        let decimal = if self.eat('.') {
            is_float = true;
            Some(try!(self.integer(start, true, false)))
        } else {
            None
        };
        let exponent = if self.eat('e') || self.eat('E') {
            is_float = true;
            Some(try!(self.integer(start, false, true)))
        } else {
            None
        };
        let end = self.next_pos();
        let input = &self.input[start..end];
        let ret = if !is_float && !input.starts_with('+') &&
                     !input.starts_with('-') && self.eat('-') {
            self.datetime(start, end + 1)
        } else {
            let raw_range = input;
            let input = match (decimal, exponent) {
                (None, None) => prefix,
                (Some(ref d), None) => prefix + "." + d,
                (None, Some(ref e)) => prefix + "E" + e,
                (Some(ref d), Some(ref e)) => prefix + "." + d + "E" + e,
            };
            let input = input.trim_left_matches('+');
            if is_float {
                input.parse().ok().map(|x| {
                    DocValue::Float{ parsed: x, raw: raw_range.to_string() }
                })
            } else {
                input.parse().ok().map(|x| {
                    DocValue::Integer{ parsed: x, raw: raw_range.to_string() }
                })
            }
        };
        if ret.is_none() {
            self.errors.push(ParserError {
                lo: start,
                hi: end,
                desc: "invalid numeric literal".into(),
            });
        }
        ret
    }

    fn integer(&mut self, start: usize, allow_leading_zeros: bool,
               allow_sign: bool) -> Option<String> {
        let mut s = String::new();
        if allow_sign {
            if self.eat('-') { s.push('-'); }
            else if self.eat('+') { s.push('+'); }
        }
        match self.cur.next() {
            Some((_, '0')) if !allow_leading_zeros => {
                s.push('0');
                match self.peek(0) {
                    Some((pos, c)) if '0' <= c && c <= '9' => {
                        self.errors.push(ParserError {
                            lo: start,
                            hi: pos,
                            desc: "leading zeroes are not allowed".into(),
                        });
                        return None
                    }
                    _ => {}
                }
            }
            Some((_, ch)) if '0' <= ch && ch <= '9' => {
                s.push(ch);
            }
            _ => {
                let pos = self.next_pos();
                self.errors.push(ParserError {
                    lo: pos,
                    hi: pos,
                    desc: "expected start of a numeric literal".into(),
                });
                return None;
            }
        }
        let mut underscore = false;
        loop {
            match self.cur.clone().next() {
                Some((_, ch)) if '0' <= ch && ch <= '9' => {
                    s.push(ch);
                    self.cur.next();
                    underscore = false;
                }
                Some((_, '_')) if !underscore => {
                    self.cur.next();
                    underscore = true;
                }
                Some(_) | None => break,
            }
        }
        if underscore {
            let pos = self.next_pos();
            self.errors.push(ParserError {
                lo: pos,
                hi: pos,
                desc: "numeral cannot end with an underscore".into(),
            });
            None
        } else {
            Some(s)
        }
    }

    fn boolean(&mut self, start: usize) -> Option<DocValue> {
        let rest = &self.input[start..];
        if rest.starts_with("true") {
            for _ in 0..4 {
                self.cur.next();
            }
            Some(DocValue::Boolean(true))
        } else if rest.starts_with("false") {
            for _ in 0..5 {
                self.cur.next();
            }
            Some(DocValue::Boolean(false))
        } else {
            let next = self.next_pos();
            self.errors.push(ParserError {
                lo: start,
                hi: next,
                desc: format!("unexpected character: `{}`",
                              rest.chars().next().unwrap()),
            });
            None
        }
    }

    fn datetime(&mut self, start: usize, end_so_far: usize) 
                -> Option<DocValue> {
        let mut date = self.input[start..end_so_far].to_owned();
        for _ in 0..15 {
            match self.cur.next() {
                Some((_, ch)) => date.push(ch),
                None => {
                    self.errors.push(ParserError {
                        lo: start,
                        hi: end_so_far,
                        desc: "malformed date literal".into(),
                    });
                    return None
                }
            }
        }
        if Parser::_is_valid_datetime(&date) {
            Some(DocValue::Datetime(date.clone()))
        } else {
            self.errors.push(ParserError {
                lo: start,
                hi: start + date.len(),
                desc: "malformed date literal".into(),
            });
            None
        }
    }

    #[doc(hidden)]
    pub fn _is_valid_datetime(date: &str) -> bool {
        let mut it = date.chars();
        let mut valid = true;
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(|c| c == '-').unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(|c| c == '-').unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(|c| c == 'T').unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(|c| c == ':').unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(|c| c == ':').unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(is_digit).unwrap_or(false);
        valid = valid && it.next().map(|c| c == 'Z').unwrap_or(false);
        valid
    }

    fn array(&mut self, _start: usize) -> Option<DocValue> {
        if !self.expect('[') { return None }
        let mut ret = Vec::new();
        let mut type_str = None;
        loop {
            // Break out early if we see the closing bracket
            self.skip_trivia();
            if self.eat(']') {
                let mut trail_aux = self.take_trivia();
                if !ret.is_empty() {
                    trail_aux = format!(",{}", trail_aux);
                }
                return Some(DocValue::InlineArray(
                    InlineArrayData {
                        values: ret,
                        comma_trail: trail_aux
                    }
                ))
            }

            // Attempt to parse a value, triggering an error if it's the wrong
            // type.
            let start = self.next_pos();
            let lead = self.take_trivia();
            let mut value = try!(self.value());
            let end = self.next_pos();
            let expected = type_str.unwrap_or_else(|| value.value.type_str());
            if value.value.type_str() != expected {
                self.errors.push(ParserError {
                    lo: start,
                    hi: end,
                    desc: format!("expected type `{}`, found type `{}`",
                                  expected, value.value.type_str()),
                });
            } else {
                type_str = Some(expected);
                value.markup.lead = lead;
                value.markup.trail = self.eat_trivia();
                ret.push(Box::new(value));
            }

            // Look for a comma. If we don't find one we're done
            if !self.eat(',') { break }
        }
        if !self.expect(']') { return None }
        Some(DocValue::InlineArray(
            InlineArrayData { values: ret, comma_trail: "".to_string() }
        ))
    }

    fn inline_table(&mut self, _start: usize) -> Option<DocValue> {
        if !self.expect('{') { return None }
        let mut trail = self.eat_ws();
        let mut ret = ValuesMap::new();
        if self.eat('}') {
            return Some(DocValue::new_table(ret,trail))
        }
        loop {
            let lo = self.next_pos();
            let key_name = try!(self.key_name());
            let key = FormattedKey::new(trail.clone(), key_name, self.eat_ws());
            if !self.expect('=') { return None }
            let mut value = try!(self.value());
            value.markup.trail = self.eat_ws();
            self.insert(&mut ret, key, value, lo);
            if self.eat('}') { break }
            if !self.expect(',') { return None }
            trail = self.eat_ws();
        }
        Some(DocValue::new_table(ret, String::new()))
    }

    fn insert(&mut self, into: &mut ValuesMap, key: FormattedKey,
              value: FormattedValue, key_lo: usize) {
        let key_text =  key.escaped.clone();
        if !into.insert(key, value) {
            self.errors.push(ParserError {
                lo: key_lo,
                hi: key_lo + key_text.len(),
                desc: format!("duplicate key: `{}`", key_text),
            })
        }
    }

    fn insert_exec_recurse<F, U>(cur: TraversalPosition,
                                 cnt_idx: usize,
                                 arrays: Option<&HashMap<*const Container, usize>>,
                                 keys: Vec<FormattedKey>,
                                 key_idx: usize,
                                 f:F)
                                 -> Result<U, ParserError>
        where F: FnOnce(TraversalPosition, Vec<FormattedKey>) -> Result<U, ParserError> {
        if key_idx == keys.len() - 1 { 
            return f(cur, keys);
        }
        if let Some(Entry::Occupied(mut entry)) =
            cur.direct.map(|x| x.kvp_index.entry(keys[key_idx].escaped.clone())) {
                #[cfg_attr(feature = "cargo-clippy", allow(match_ref_pats))]
                match &mut entry.get_mut().borrow_mut().value.value {
                    &mut DocValue::InlineTable(TableData { ref mut values, .. }) => {
                        let next = values.traverse();
                        return Parser::insert_exec_recurse(next, cnt_idx, arrays, keys, key_idx+1, f);
                    }
                    &mut DocValue::InlineArray(InlineArrayData {values: ref mut vec, ..}) => {
                        let has_tables = match vec.first() {
                            None => false,
                            Some(v) => v.value.is_table()
                        };
                        if !has_tables {
                            let error_msg = format!(
                                "array `{}` does not contain tables",
                                &*keys[key_idx].escaped
                            );
                            return Result::Err(ParserError {
                                lo: 0,
                                hi: 0,
                                desc: error_msg
                            });
                        }
                        let idx_last = vec.len()-1;
                        let next = vec[idx_last].value.as_table().traverse();
                        return Parser::insert_exec_recurse(next, cnt_idx, arrays, keys, key_idx+1, f);
                    }
                    _ => {
                        return Result::Err(ParserError {
                            lo: 0,
                            hi: 0,
                            desc: format!("key `{}` was not previously a table",
                                          &*keys[key_idx].escaped)
                        });
                    }
                }
            }
        match cur.indirect.entry(keys[key_idx].escaped.clone()) {
            Entry::Occupied(mut entry) => match *entry.get_mut() {
                IndirectChild::ImplicitTable(ref mut map) => {
                    let next = TraversalPosition::from_indirect(map);
                    Parser::insert_exec_recurse(next, cnt_idx, arrays, keys, key_idx+1, f)
                }
                IndirectChild::ExplicitTable(ref mut c) => {
                    let c_data = &mut c.borrow_mut().data;
                    Parser::insert_exec_recurse(c_data.traverse(), cnt_idx, arrays, keys, key_idx+1, f)
                }
                IndirectChild::Array(ref mut vec) => {
                    let insert_idx = if let Some(map) = arrays {
                        let idx = vec.binary_search_by(|cnt| map[&(&*cnt.borrow() as *const Container)].cmp(&cnt_idx));
                        idx.map(|x| x - 1).unwrap_or_else(|x| x)
                    } else if cnt_idx == 0 {
                        0
                    } else {
                        vec.len() - 1
                    };
                    let mut c_data = &mut vec[insert_idx]
                                             .borrow_mut()
                                             .data;
                    Parser::insert_exec_recurse(c_data.traverse(), cnt_idx, arrays, keys, key_idx+1, f)
                }
            },
            Entry::Vacant(entry) => {
                let empty = HashMap::new();
                let map = entry.insert(IndirectChild::ImplicitTable(empty));
                let next = TraversalPosition::from_indirect(map.as_implicit());
                Parser::insert_exec_recurse(next, cnt_idx, arrays, keys, key_idx+1, f)
            }
        }
    }

    // Executes given function on the container keyed by the path `keys`,
    // inserting missing containers on the go
    fn insert_exec_container<F, U>(r: &mut Document,
                                   idx: usize,
                                   arrays: Option<&HashMap<*const Container, usize>>,
                                   keys: Vec<FormattedKey>,
                                   f:F) -> Result<U, ParserError>
                                   where F: FnOnce(TraversalPosition,
                                                   Vec<FormattedKey>) 
                                                   -> Result<U, ParserError> {
        Parser::insert_exec_recurse(r.traverse(), idx, arrays, keys, 0, f)
    }

    fn build_array_map(d: &Document, idx: usize) -> Option<HashMap<*const Container, usize>> {
        if idx > 0 && idx < d.container_list.len() {
            Some(d.container_list
                  .iter()
                  .enumerate()
                  .filter(|&(_, c)| c.borrow().kind == ContainerKind::ArrayMember)
                  .map(|(idx, c)| (&*c.borrow() as *const Container, idx))
                  .collect::<HashMap<_,_>>())
        } else {
            None
        }
        
    }

    fn try_insert_table(mut entry: OccupiedEntry<String, IndirectChild>,
                        container: Rc<RefCell<Container>>)
                        -> Result<Rc<RefCell<Container>>, ParserError> {
        if entry.get().is_implicit() {
            let table = IndirectChild::ExplicitTable(container.clone());
            let old = entry.insert(table);
            for (k,v) in old.to_implicit() {
                let key_copy = k.clone();
                if container.borrow_mut().data.indirect.insert(k, v).is_some() {
                    return Result::Err(ParserError {
                        lo: 0,
                        hi: 0,
                        desc: format!("duplicate key `{}` in table", key_copy),
                    });
                }
           }
           Result::Ok(container)
        }
        else {
            let keys = &container.borrow().keys;
            Result::Err(ParserError {
                lo: 0,
                hi: 0,
                desc: format!("redefinition of table `{}`",
                              &*keys.vec.last().as_ref().unwrap().escaped),
            })
        }
    }

    fn add_table(entry: VacantEntry<String, IndirectChild>,
                 container: Rc<RefCell<Container>>)
                 -> Result<Rc<RefCell<Container>>, ParserError> {
        entry.insert(IndirectChild::ExplicitTable(container.clone()));
        Result::Ok(container)
    }

    fn last_key_text(keys: &[FormattedKey]) -> String {
        keys.last().as_ref().unwrap().escaped.clone()
    }

    #[doc(hidden)]
    pub fn _insert_table(root: &mut Document, idx: usize, keys: Vec<FormattedKey>, 
                         table: ContainerData, lead: String) -> Option<ParserError> {
        let array_map = Parser::build_array_map(root, idx);
        let added = Parser::insert_exec_container(root, idx, array_map.as_ref(), keys, |seg, keys| {
            let key_text = Parser::last_key_text(&keys);
            if let Some(map) = seg.direct {
                if map.kvp_index.contains_key(&*key_text) {
                    let is_table = map.kvp_index[&*key_text]
                                      .borrow()
                                      .value
                                      .value
                                      .is_table();
                    let error_msg  = if is_table {
                        format!("redefinition of table `{}`", &*key_text)
                    } else {
                        format!("duplicate key `{}` in table", &*key_text)
                    };
                    return Result::Err(ParserError {
                        lo: 0,
                        hi: 0,
                        desc: error_msg
                    });
                }
            }
            let container = Container::new_table(table, keys, lead);
            let container = Rc::new(RefCell::new(container));
            match seg.indirect.entry(key_text) {
                Entry::Occupied(entry)
                    => Parser::try_insert_table(entry, container),
                Entry::Vacant(entry) 
                    => Parser::add_table(entry, container.clone())
            }
        });
        match added {
            Result::Ok(ptr) => { 
                root.container_list.insert(idx, ptr);
                None
            }
            Result::Err(err) => Some(err)
        } 
    }

    fn try_insert_array(mut entry: OccupiedEntry<String, IndirectChild>,
                        cnt_idx: usize,
                        arrays: Option<&HashMap<*const Container, usize>>,
                        container: Rc<RefCell<Container>>)
                        -> Result<Rc<RefCell<Container>>, ParserError> {
        match *entry.get_mut() {
            IndirectChild::ExplicitTable(_)
            | IndirectChild::ImplicitTable(_) => {
                let keys = &container.borrow().keys;
                Result::Err(ParserError {
                    lo: 0,
                    hi: 0,
                    desc:
                        format!(
                            "key `{}` was previously not an array",
                            Parser::last_key_text(&keys.vec)),
                })
            }
            IndirectChild::Array(ref mut vec) => {
                let insert_idx = if let Some(map) = arrays {
                    let idx = vec.binary_search_by(|cnt| map[&(&*cnt.borrow() as *const Container)].cmp(&cnt_idx));
                    idx.unwrap_or_else(|x| x)
                } else if cnt_idx == 0 {
                    0
                } else {
                    vec.len()
                };
                vec.insert(insert_idx, container.clone());
                Result::Ok(container)
            }
        }
    }

    fn add_array(entry: VacantEntry<String, IndirectChild>,
                 container: Rc<RefCell<Container>>)
                 -> Result<Rc<RefCell<Container>>, ParserError> {
        entry.insert(IndirectChild::Array(vec!(container.clone())));
        Result::Ok(container)
    }

    #[doc(hidden)]
    pub fn _insert_array(root: &mut Document, idx: usize,
                         keys: Vec<FormattedKey>, table: ContainerData,
                         lead: String) -> Option<ParserError> {
        let array_map = Parser::build_array_map(root, idx);
        let added = Parser::insert_exec_container(root, idx, array_map.as_ref(), keys, |seg, keys| {
            let key_text = Parser::last_key_text(&keys);
            if let Some(map) = seg.direct {
                if map.kvp_index.contains_key(&*key_text) {
                    return Result::Err(ParserError {
                        lo: 0,
                        hi: 0,
                        desc: format!("duplicate key `{}` in table", &*key_text)
                    });
                }
            }
            let container = Container::new_array(table, keys, lead);
            let container = Rc::new(RefCell::new(container));
            match seg.indirect.entry(key_text) {
                Entry::Occupied(entry)
                    => Parser::try_insert_array(entry,
                                                idx,
                                                array_map.as_ref(),
                                                container),
                Entry::Vacant(entry)
                    => Parser::add_array(entry, container.clone())
            }
        });
        match added {
            Result::Ok(ptr) => { 
                root.container_list.insert(idx, ptr);
                None
            }
            Result::Err(err) => Some(err)
        }
    }
}

impl Error for ParserError {
    fn description(&self) -> &str { "TOML parse error" }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.desc.fmt(f)
    }
}

fn is_digit(c: char) -> bool {
    match c { '0' ... '9' => true, _ => false }
}

#[cfg(test)]
mod tests {
    use {Document, EntryRef};
    use super::Parser;

    macro_rules! bad {
        ($s:expr, $msg:expr) => ({
            let mut p = Parser::new($s);
            assert!(p.parse().is_none());
            assert!(p.errors.iter().any(|e| e.desc.contains($msg)),
                    "errors: {:?}", p.errors);
        })
    }

    fn as_str<'a>(entry: EntryRef<'a>) -> Option<&'a str> {
        match entry {
            EntryRef::String(str_node) => Some(str_node.get()),
            _ => None
        }
    }

    fn as_float(entry: EntryRef) -> Option<f64> {
        match entry {
            EntryRef::Float(value) => Some(value.get()),
            _ => None
        }
    }

    fn as_integer(entry: EntryRef) -> Option<i64> {
        match entry {
            EntryRef::Integer(value) => Some(value.get()),
            _ => None
        }
    }

    fn as_bool(entry: EntryRef) -> Option<bool> {
        match entry {
            EntryRef::Boolean(value) => Some(value.get()),
            _ => None
        }
    }

    fn lookup<'a>(doc: &'a Document, path: &'a [&'a str])
                  -> Option<EntryRef<'a>> {
        doc.get(path[0]).and_then(|val| lookup_inner(val, &path[1..]))
    }

    fn lookup_inner<'a>(val: EntryRef<'a>, path: &'a [&'a str])
                        -> Option<EntryRef<'a>> {
        if path.len() == 0 { return Some(val) }
        let numeric = path[0].parse::<usize>();
        match numeric {
            Ok(index) => match val {
                EntryRef::Array(arr) =>
                    lookup_inner(arr.get(index), &path[1..]),
                _ => None
            },
            Err(..) => match val {
                EntryRef::Table(t) =>
                    lookup_inner(t.get(path[0]).unwrap(), &path[1..]),
                _ => None
            }
        }
    }

    #[test]
    fn crlf() {
        let mut p = Parser::new("\
[project]\r\n\
\r\n\
name = \"splay\"\r\n\
version = \"0.1.0\"\r\n\
authors = [\"alex@crichton.co\"]\r\n\
\r\n\
[[lib]]\r\n\
\r\n\
path = \"lib.rs\"\r\n\
name = \"splay\"\r\n\
description = \"\"\"\
A Rust implementation of a TAR file reader and writer. This library does not\r\n\
currently handle compression, but it is abstract over all I/O readers and\r\n\
writers. Additionally, great lengths are taken to ensure that the entire\r\n\
contents are never required to be entirely resident in memory all at once.\r\n\
\"\"\"\
");
        assert!(p.parse().is_some());
    }

    #[test]
    fn linecol() {
        let p = Parser::new("ab\ncde\nf");
        assert_eq!(p.to_linecol(0), (0, 0));
        assert_eq!(p.to_linecol(1), (0, 1));
        assert_eq!(p.to_linecol(3), (1, 0));
        assert_eq!(p.to_linecol(4), (1, 1));
        assert_eq!(p.to_linecol(7), (2, 0));
    }

    #[test]
    fn fun_with_strings() {
        let mut p = Parser::new(r#"
bar = "\U00000000"
key1 = "One\nTwo"
key2 = """One\nTwo"""
key3 = """
One
Two"""

key4 = "The quick brown fox jumps over the lazy dog."
key5 = """
The quick brown \


  fox jumps over \
    the lazy dog."""
key6 = """\
       The quick brown \
       fox jumps over \
       the lazy dog.\
       """
# What you see is what you get.
winpath  = 'C:\Users\nodejs\templates'
winpath2 = '\\ServerX\admin$\system32\'
quoted   = 'Tom "Dubs" Preston-Werner'
regex    = '<\i\c*\s*>'

regex2 = '''I [dw]on't need \d{2} apples'''
lines  = '''
The first newline is
trimmed in raw strings.
   All other whitespace
   is preserved.
'''
"#);
        let table = p.parse().unwrap();
        assert_eq!(table.get("bar").and_then(as_str), Some("\0"));
        assert_eq!(table.get("key1").and_then(as_str),
                   Some("One\nTwo"));
        assert_eq!(table.get("key2").and_then(as_str),
                   Some("One\nTwo"));
        assert_eq!(table.get("key3").and_then(as_str),
                   Some("One\nTwo"));

        let msg = "The quick brown fox jumps over the lazy dog.";
        assert_eq!(table.get("key4").and_then(as_str), Some(msg));
        assert_eq!(table.get("key5").and_then(as_str), Some(msg));
        assert_eq!(table.get("key6").and_then(as_str), Some(msg));

        assert_eq!(table.get("winpath").and_then(as_str),
                   Some(r"C:\Users\nodejs\templates"));
        assert_eq!(table.get("winpath2").and_then(as_str),
                   Some(r"\\ServerX\admin$\system32\"));
        assert_eq!(table.get("quoted").and_then(as_str),
                   Some(r#"Tom "Dubs" Preston-Werner"#));
        assert_eq!(table.get("regex").and_then(as_str),
                   Some(r"<\i\c*\s*>"));
        assert_eq!(table.get("regex2").and_then(as_str),
                   Some(r"I [dw]on't need \d{2} apples"));
        assert_eq!(table.get("lines").and_then(as_str),
                   Some("The first newline is\n\
                         trimmed in raw strings.\n   \
                            All other whitespace\n   \
                            is preserved.\n"));
    }

    #[test]
    fn tables_in_arrays() {
        let mut p = Parser::new(r#"
[[foo]]
  #…
  [foo.bar]
    #…

[[foo]]
  #…
  [foo.bar]
    #...
"#);
        let table = p.parse().unwrap();
        lookup(&table, &["foo","0","bar"]);
        lookup(&table, &["foo","1","bar"]);
    }

    #[test]
    fn fruit() {
        let mut p = Parser::new(r#"
[[fruit]]
  name = "apple"

  [fruit.physical]
    color = "red"
    shape = "round"

  [[fruit.variety]]
    name = "red delicious"

  [[fruit.variety]]
    name = "granny smith"

[[fruit]]
  name = "banana"

  [[fruit.variety]]
    name = "plantain"
"#);
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["fruit","0","name"])
                       .and_then(as_str),
                   Some("apple"));
        assert_eq!(lookup(&table, &["fruit","0","physical","color"])
                       .and_then(as_str),
                   Some("red"));
        assert_eq!(lookup(&table, &["fruit","0","physical","shape"])
                       .and_then(as_str),
                   Some("round"));
        assert_eq!(lookup(&table, &["fruit","0","variety","0","name"])
                       .and_then(as_str),
                   Some("red delicious"));
        assert_eq!(lookup(&table, &["fruit","0","variety","1","name"])
                       .and_then(as_str),
                   Some("granny smith"));
        assert_eq!(lookup(&table, &["fruit","1","name"]).and_then(as_str),
                   Some("banana"));
        assert_eq!(lookup(&table, &["fruit","1","variety","0","name"])
                       .and_then(as_str),
                   Some("plantain"));
    }

    #[test]
    fn stray_cr() {
        assert!(Parser::new("\r").parse().is_none());
        assert!(Parser::new("a = [ \r ]").parse().is_none());
        assert!(Parser::new("a = \"\"\"\r\"\"\"").parse().is_none());
        assert!(Parser::new("a = \"\"\"\\  \r  \"\"\"").parse().is_none());

        let mut p = Parser::new("foo = '''\r'''");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_str), Some("\r"));

        let mut p = Parser::new("foo = '\r'");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_str), Some("\r"));
    }

    #[test]
    fn blank_literal_string() {
        let mut p = Parser::new("foo = ''");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_str), Some(""));
    }

    #[test]
    fn many_blank() {
        let mut p = Parser::new("foo = \"\"\"\n\n\n\"\"\"");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_str), Some("\n\n"));
    }

    #[test]
    fn literal_eats_crlf() {
        let mut p = Parser::new("
            foo = \"\"\"\\\r\n\"\"\"
            bar = \"\"\"\\\r\n   \r\n   \r\n   a\"\"\"
        ");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_str), Some(""));
        assert_eq!(lookup(&table, &["bar"]).and_then(as_str), Some("a"));
    }

    #[test]
    fn string_no_newline() {
        assert!(Parser::new("a = \"\n\"").parse().is_none());
        assert!(Parser::new("a = '\n'").parse().is_none());
    }

    #[test]
    fn bad_leading_zeros() {
        assert!(Parser::new("a = 00").parse().is_none());
        assert!(Parser::new("a = -00").parse().is_none());
        assert!(Parser::new("a = +00").parse().is_none());
        assert!(Parser::new("a = 00.0").parse().is_none());
        assert!(Parser::new("a = -00.0").parse().is_none());
        assert!(Parser::new("a = +00.0").parse().is_none());
        assert!(Parser::new("a = 9223372036854775808").parse().is_none());
        assert!(Parser::new("a = -9223372036854775809").parse().is_none());
    }

    #[test]
    fn bad_floats() {
        assert!(Parser::new("a = 0.").parse().is_none());
        assert!(Parser::new("a = 0.e").parse().is_none());
        assert!(Parser::new("a = 0.E").parse().is_none());
        assert!(Parser::new("a = 0.0E").parse().is_none());
        assert!(Parser::new("a = 0.0e").parse().is_none());
        assert!(Parser::new("a = 0.0e-").parse().is_none());
        assert!(Parser::new("a = 0.0e+").parse().is_none());
        assert!(Parser::new("a = 0.0e+00").parse().is_none());
    }

    #[test]
    fn floats() {
        macro_rules! t {
            ($actual:expr, $expected:expr) => ({
                let f = format!("foo = {}", $actual);
                let mut p = Parser::new(&f);
                let table = p.parse().unwrap();
                assert_eq!(lookup(&table, &["foo"]).and_then(as_float),
                           Some($expected));
            })
        }

        t!("1.0", 1.0);
        t!("1.0e0", 1.0);
        t!("1.0e+0", 1.0);
        t!("1.0e-0", 1.0);
        t!("1.001e-0", 1.001);
        t!("2e10", 2e10);
        t!("2e+10", 2e10);
        t!("2e-10", 2e-10);
        t!("2_0.0", 20.0);
        t!("2_0.0_0e0_0", 20.0);
        t!("2_0.1_0e1_0", 20.1e10);
    }

    #[test]
    fn bare_key_names() {
        let mut p = Parser::new("
            foo = 3
            foo_3 = 3
            foo_-2--3--r23f--4-f2-4 = 3
            _ = 3
            - = 3
            8 = 8
            \"a\" = 3
            \"!\" = 3
            \"a^b\" = 3
            \"\\\"\" = 3
            \"character encoding\" = \"value\"
            \"ʎǝʞ\" = \"value\"
        ");
        let table = p.parse().unwrap();
        assert!(lookup(&table, &["foo"]).is_some());
        assert!(lookup(&table, &["-"]).is_some());
        assert!(lookup(&table, &["_"]).is_some());
        assert!(lookup(&table, &["8"]).is_some());
        assert!(lookup(&table, &["foo_3"]).is_some());
        assert!(lookup(&table, &["foo_-2--3--r23f--4-f2-4"]).is_some());
        assert!(lookup(&table, &["a"]).is_some());
        assert!(lookup(&table, &["!"]).is_some());
        assert!(lookup(&table, &["\""]).is_some());
        assert!(lookup(&table, &["character encoding"]).is_some());
        assert!(lookup(&table, &["ʎǝʞ"]).is_some());
    }

    #[test]
    fn bad_keys() {
        assert!(Parser::new("key\n=3").parse().is_none());
        assert!(Parser::new("key=\n3").parse().is_none());
        assert!(Parser::new("key|=3").parse().is_none());
        assert!(Parser::new("\"\"=3").parse().is_none());
        assert!(Parser::new("=3").parse().is_none());
        assert!(Parser::new("\"\"|=3").parse().is_none());
        assert!(Parser::new("\"\n\"|=3").parse().is_none());
        assert!(Parser::new("\"\r\"|=3").parse().is_none());
    }

    #[test]
    fn bad_table_names() {
        assert!(Parser::new("[]").parse().is_none());
        assert!(Parser::new("[.]").parse().is_none());
        assert!(Parser::new("[\"\".\"\"]").parse().is_none());
        assert!(Parser::new("[a.]").parse().is_none());
        assert!(Parser::new("[\"\"]").parse().is_none());
        assert!(Parser::new("[!]").parse().is_none());
        assert!(Parser::new("[\"\n\"]").parse().is_none());
        assert!(Parser::new("[a.b]\n[a.\"b\"]").parse().is_none());
    }

    #[test]
    fn table_names() {
        let mut p = Parser::new("
            [a.\"b\"]
            [\"f f\"]
            [\"f.f\"]
            [\"\\\"\"]
        ");
        let table = p.parse().unwrap();
        assert!(lookup(&table, &["a","b"]).is_some());
        assert!(lookup(&table, &["f f"]).is_some());
        assert!(lookup(&table, &["\""]).is_some());
    }

    #[test]
    fn invalid_bare_numeral() {
        assert!(Parser::new("4").parse().is_none());
    }

    #[test]
    fn inline_tables() {
        assert!(Parser::new("a = {}").parse().is_some());
        assert!(Parser::new("a = {b=1}").parse().is_some());
        assert!(Parser::new("a = {   b   =   1    }").parse().is_some());
        assert!(Parser::new("a = {a=1,b=2}").parse().is_some());
        assert!(Parser::new("a = {a=1,b=2,c={}}").parse().is_some());
        assert!(Parser::new("a = {a=1,}").parse().is_none());
        assert!(Parser::new("a = {,}").parse().is_none());
        assert!(Parser::new("a = {a=1,a=1}").parse().is_none());
        assert!(Parser::new("a = {\n}").parse().is_none());
        assert!(Parser::new("a = {").parse().is_none());
        assert!(Parser::new("a = {a=[\n]}").parse().is_some());
        assert!(Parser::new("a = {\"a\"=[\n]}").parse().is_some());
        assert!(Parser::new("a = [\n{},\n{},\n]").parse().is_some());
    }

    #[test]
    fn number_underscores() {
        macro_rules! t {
            ($actual:expr, $expected:expr) => ({
                let f = format!("foo = {}", $actual);
                let mut p = Parser::new(&f);
                let table = p.parse().unwrap();
                assert_eq!(lookup(&table, &["foo"]).and_then(as_integer),
                           Some($expected));
            })
        }

        t!("1_0", 10);
        t!("1_0_0", 100);
        t!("1_000", 1000);
        t!("+1_000", 1000);
        t!("-1_000", -1000);
    }

    #[test]
    fn bad_underscores() {
        assert!(Parser::new("foo = 0_").parse().is_none());
        assert!(Parser::new("foo = 0__0").parse().is_none());
        assert!(Parser::new("foo = __0").parse().is_none());
        assert!(Parser::new("foo = 1_0_").parse().is_none());
    }

    #[test]
    fn bad_unicode_codepoint() {
        bad!("foo = \"\\uD800\"", "not a valid unicode codepoint");
    }

    #[test]
    fn bad_strings() {
        bad!("foo = \"\\uxx\"", "expected 4 hex digits");
        bad!("foo = \"\\u\"", "expected 4 hex digits");
        bad!("foo = \"\\", "unterminated");
        bad!("foo = '", "unterminated");
    }

    #[test]
    fn empty_string() {
        let mut p = Parser::new("foo = \"\"");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_str), Some(""));
    }

    #[test]
    fn booleans() {
        let mut p = Parser::new("foo = true");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_bool), Some(true));

        let mut p = Parser::new("foo = false");
        let table = p.parse().unwrap();
        assert_eq!(lookup(&table, &["foo"]).and_then(as_bool), Some(false));

        assert!(Parser::new("foo = true2").parse().is_none());
        assert!(Parser::new("foo = false2").parse().is_none());
        assert!(Parser::new("foo = t1").parse().is_none());
        assert!(Parser::new("foo = f2").parse().is_none());
    }

    #[test]
    fn bad_nesting() {
        bad!("
            a = [2]
            [[a]]
            b = 5
        ", "duplicate key `a` in table");
        bad!("
            a = 1
            [a.b]
        ", "key `a` was not previously a table");
        bad!("
            a = []
            [a.b]
        ", "array `a` does not contain tables");
        bad!("
            a = []
            [[a.b]]
        ", "array `a` does not contain tables");
        bad!("
            [a]
            b = { c = 2, d = {} }
            [a.b]
            c = 2
        ", "redefinition of table `b`");
    }

    #[test]
    fn bad_table_redefine() {
        bad!("
            [a]
            foo=\"bar\"
            [a.b]
            foo=\"bar\"
            [a]
        ", "redefinition of table `a`");
        bad!("
            [a]
            foo=\"bar\"
            b = { foo = \"bar\" }
            [a]
        ", "redefinition of table `a`");
        bad!("
            [a]
            b = {}
            [a.b]
        ", "redefinition of table `b`");

        bad!("
            [a]
            b = {}
            [a]
        ", "redefinition of table `a`");
    }
}
