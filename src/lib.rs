/*!
A tool for implementing sound change algorithms.

This is strictly modeled after Mark Rosenfelder's [Sound Change Applier][sca],
with a necessary tweak for Rust's macro system.

[sca]: http://zompist.com/scahelp.html

Typical usage and the comparison with the original SCA rules:

```
#![feature(phase)]
#[phase(plugin, link)] extern crate soundchange;
#[phase(plugin, link)] extern crate log;

fn main() {
    fn boundary(c: Option<char>) -> bool { c.is_none() }
    fn vowel(c: Option<char>) -> bool { c.map_or(false, |c| "aeiou".contains_char(c)) }
    fn reverse(s: &str, out: &mut String) { out.extend(s.chars().rev()); }

    let s = "fihs".into_string();
    let s = subst_rules! { s.as_slice() with    // V=aeiou
        "f" [boundary] => "gh";                 // f/gh/_#
        "f" => "ph";                            // f/ph/_
        ["w"] vowel ["m" vowel "n"] => "o";     // V/o/w_mVn
        "sh" ["o"] => "ti";                     // sh/ti/_o
        [vowel] "hs" => reverse;                // sh/\\/V_
        "" [boundary] => "ing";                 // /ing/_#
    };
    assert_eq!(s.as_slice(), "phishing");
}
```

Note: You can use `RUST_LOG=4` for tracking any change on the string
and rules that trigger that change.

Any expression in the left side is considered a "condition" for searching,
and can be either `char`, `&str` or a function from `Option<char>` to `bool`.
The function argument is an `Option` since it can look at the string boundary.
`[]` indicates a context, and is not considered when the string gets transformed.
(The behavior of conditions can be also customized by implementing `IntoCond` trait
and/or `Search` trait. `&str` and the function implements both.)

Any expression in the right side is considered a "transformer",
and can be either `char`, `&str` or a function from `char` to `char`.
When multiple characters have been matched, a function will be invoked for each character.
(The behavior of transformers can be also customized by implementing `Transform` trait.)

The actual processing is driven through the `subst` function,
which is for convenience wrapped into the `subst_rules!` macro.
The syntax should be self-explanatory, except that it returns a `CowString`.
*/

#![feature(macro_rules, phase)]

#[phase(plugin, link)] extern crate log;

use std::str::{CowString, CharRange};

pub enum Cond<'a> {
    Char(char),
    CharOf(fn(Option<char>) -> bool),
    Str(&'a str),
    StrOf(for<'b> fn(&'b str) -> Option<&'b str>),
}

impl<'a> Cond<'a> {
    pub fn check_and_narrow_prefix<'b>(&self, s: &'b str) -> Option<&'b str> {
        match *self {
            Cond::Char(p) => {
                if s.is_empty() { return None; }
                let CharRange { ch, next } = s.char_range_at_reverse(s.len());
                if ch == p {Some(s.slice_to(next))} else {None}
            },
            Cond::CharOf(ref f) => {
                let (ch, next) = if s.is_empty() {
                    (None, 0)
                } else {
                    let CharRange { ch, next } = s.char_range_at_reverse(s.len());
                    (Some(ch), next)
                };
                if (*f)(ch) {Some(s.slice_to(next))} else {None}
            },
            Cond::Str(p) => if s.ends_with(p) {Some(s.slice_to(s.len() - p.len()))} else {None},
            Cond::StrOf(ref f) => (*f)(s),
        }
    }

    pub fn check_and_narrow_postfix<'b>(&self, s: &'b str) -> Option<&'b str> {
        match *self {
            Cond::Char(p) => {
                if s.is_empty() { return None; }
                let CharRange { ch, next } = s.char_range_at(0);
                if ch == p {Some(s.slice_from(next))} else {None}
            },
            Cond::CharOf(ref f) => {
                let (ch, next) = if s.is_empty() {
                    (None, 0)
                } else {
                    let CharRange { ch, next } = s.char_range_at(0);
                    (Some(ch), next)
                };
                if (*f)(ch) {Some(s.slice_from(next))} else {None}
            },
            Cond::Str(p) => if s.starts_with(p) {Some(s.slice_from(p.len()))} else {None},
            Cond::StrOf(ref f) => (*f)(s),
        }
    }
}

pub fn check_prefix(conds: &[Cond], mut prefix: &str) -> bool {
    for cond in conds.iter().rev() {
        match cond.check_and_narrow_prefix(prefix) {
            Some(s) => { prefix = s; }
            None => return false
        }
    }
    true
}

pub fn check_postfix(conds: &[Cond], mut postfix: &str) -> bool {
    for cond in conds.iter() {
        match cond.check_and_narrow_postfix(postfix) {
            Some(s) => { postfix = s; }
            None => return false
        }
    }
    true
}

pub trait IntoCond<'a> {
    fn into_pre_cond(self) -> Cond<'a>;
    fn into_post_cond(self) -> Cond<'a>;
}

impl<'a> IntoCond<'a> for Cond<'a> {
    fn into_pre_cond(self) -> Cond<'a> { self }
    fn into_post_cond(self) -> Cond<'a> { self }
}

impl<'a> IntoCond<'a> for char {
    fn into_pre_cond(self) -> Cond<'a> { Cond::Char(self) }
    fn into_post_cond(self) -> Cond<'a> { Cond::Char(self) }
}

impl<'a> IntoCond<'a> for fn(Option<char>) -> bool {
    fn into_pre_cond(self) -> Cond<'a> { Cond::CharOf(self) }
    fn into_post_cond(self) -> Cond<'a> { Cond::CharOf(self) }
}

impl<'a> IntoCond<'a> for &'a str {
    fn into_pre_cond(self) -> Cond<'a> { Cond::Str(self) }
    fn into_post_cond(self) -> Cond<'a> { Cond::Str(self) }
}

impl<'a> IntoCond<'a> for fn(&str) -> Option<&str> {
    fn into_pre_cond(self) -> Cond<'a> { Cond::StrOf(self) }
    fn into_post_cond(self) -> Cond<'a> { Cond::StrOf(self) }
}

pub trait Search {
    fn search_loop(&mut self, s: &str, preconds: &[Cond], postconds: &[Cond], f: |bool, &str|);
}

impl<'a> Search for &'a str {
    fn search_loop(&mut self, s: &str, mut preconds: &[Cond], mut postconds: &[Cond],
                   f: |bool, &str|) {
        // try to coalesce the string match. it makes the search quicker.
        let mut search: CowString = self.into_cow();
        loop {
            match preconds.last() {
                Some(&Cond::Char(p)) => {
                    let mut t = String::new();
                    t.push(p);
                    t.push_str(search.as_slice());
                    *search.to_mut() = t;
                }
                Some(&Cond::Str(p)) => {
                    *search.to_mut() = p.into_string() + search.as_slice();
                }
                _ => break,
            }
            preconds = preconds.init();
        }
        let preoffset = search.len() - self.len();
        let postoffset = search.len();
        loop {
            match postconds.head() {
                Some(&Cond::Char(p)) => { search.to_mut().push(p); }
                Some(&Cond::Str(p)) => { search.to_mut().push_str(p); }
                _ => break,
            }
            postconds = postconds.tail();
        }

        let mut lastmatch = 0;
        let mut start = 0;
        loop {
            let pos = match s.slice_from(start).find_str(search.as_slice()) {
                Some(i) => i + start,
                None => break,
            };

            if check_prefix(preconds, s.slice_to(pos)) &&
               check_postfix(postconds, s.slice_from(pos + search.len())) {
                let matchstart = pos + preoffset;
                let matchend = pos + postoffset;
                if lastmatch < matchstart {
                    f(false, s.slice(lastmatch, matchstart));
                }
                f(true, s.slice(matchstart, matchend));
                lastmatch = matchend;
            }

            if pos == s.len() { break; }
            start = s.char_range_at(pos).next;
        }

        if lastmatch < s.len() {
            f(false, s.slice_from(lastmatch));
        }
    }
}

impl<'a> Search for |Option<char>|: 'a -> bool {
    fn search_loop(&mut self, s: &str, preconds: &[Cond], postconds: &[Cond], f: |bool, &str|) {
        let mut lastmatch = 0;
        for (i, c) in s.char_indices() {
            let j = i + c.len_utf8();
            if (*self)(Some(c)) && check_prefix(preconds, s.slice_to(i))
                                && check_postfix(postconds, s.slice_from(j)) {
                if lastmatch < i {
                    f(false, s.slice(lastmatch, i));
                }
                f(true, s.slice(i, j));
                lastmatch = j;
            }
        }
        if lastmatch < s.len() {
            f(false, s.slice_from(lastmatch));
        }
    }
}

impl Search for fn(Option<char>) -> bool {
    fn search_loop(&mut self, s: &str, preconds: &[Cond], postconds: &[Cond], f: |bool, &str|) {
        (|c| (*self)(c)).search_loop(s, preconds, postconds, f)
    }
}

pub trait Transform {
    fn transform(&mut self, s: &str, buf: &mut String);
}

impl<'a> Transform for &'a str {
    fn transform(&mut self, _s: &str, buf: &mut String) { buf.push_str(*self); }
}

impl<'a> Transform for |char|: 'a -> char {
    fn transform(&mut self, s: &str, buf: &mut String) {
        for c in s.chars() { buf.push((*self)(c)); }
    }
}

impl<'a> Transform for |&str, &mut String|: 'a {
    fn transform(&mut self, s: &str, buf: &mut String) { (*self)(s, buf); }
}

impl Transform for fn(char) -> char {
    fn transform(&mut self, s: &str, buf: &mut String) { (|c| (*self)(c)).transform(s, buf) }
}

impl Transform for fn(&str, &mut String) {
    fn transform(&mut self, s: &str, buf: &mut String) { (*self)(s, buf); }
}

pub fn subst<'a, From: Search, To: Transform>(s: &'a str, preconds: &[Cond], mut from: From,
                                              postconds: &[Cond], mut to: To) -> CowString<'a> {
    let mut buf = String::new();
    let mut unmatched = Some(0);
    from.search_loop(s, preconds, postconds, |found, ss| {
        if unmatched.is_some() {
            let last = unmatched.unwrap();
            if !found && last == s.subslice_offset(ss) {
                unmatched = Some(last + ss.len());
            } else {
                buf.push_str(s.slice_to(last));
                unmatched = None;
            }
        }
        if found {
            assert!(unmatched.is_none());
            to.transform(ss, &mut buf);
        } else if unmatched.is_none() {
            buf.push_str(ss);
        }
    });

    match unmatched {
        Some(last) => s.slice_to(last).into_cow(),
        None => buf.into_cow()
    }
}

#[macro_export]
macro_rules! subst_rules(
    ($e:expr with $($t:tt)*) => ({
        use std::str::CowString;
        use soundchange::{Search, Transform, Cond, IntoCond};

        #[inline(always)]
        fn subst<'a, From: Search, To: Transform>(s: &'a str, preconds: &[Cond], from: From,
                                                  postconds: &[Cond], to: To,
                                                  rulestring: &str) -> CowString<'a> {
            let ret = ::soundchange::subst(s, preconds, from, postconds, to);
            if s != ret.as_slice() { debug!("{} --> {} ({})", s, ret, rulestring); }
            ret
        }

        subst_rules!($e With $($t)*)
    });

    ($e:expr With) => ($e.into_owned()); // XXX oops!
    ($e:expr With [$($pre:tt)*] $from:tt [$($post:tt)*] => $to:expr; $($t:tt)*) =>
        (subst_rules!(subst($e.as_slice(),
                            &[$($pre.into_pre_cond()),*], $from, &[$($post.into_post_cond()),*],
                            $to,
                            concat!("[", stringify!($($pre)*), "] ", stringify!($from),
                                   " [", stringify!($($post)*), "] => ", stringify!($to)))
                      With $($t)*));
    ($e:expr With [$($pre:tt)*] $from:tt => $to:expr; $($t:tt)*) =>
        (subst_rules!(subst($e.as_slice(),
                            &[$($pre.into_pre_cond()),*], $from, &[],
                            $to,
                            concat!("[", stringify!($($pre)*), "] ", stringify!($from),
                                   " => ", stringify!($to)))
                      With $($t)*));
    ($e:expr With $from:tt [$($post:tt)*] => $to:expr; $($t:tt)*) =>
        (subst_rules!(subst($e.as_slice(),
                            &[], $from, &[$($post.into_post_cond()),*],
                            $to,
                            concat!(stringify!($from), " [", stringify!($($post)*), "] => ",
                                    stringify!($to)))
                      With $($t)*));
    ($e:expr With $from:tt => $to:expr; $($t:tt)*) =>
        (subst_rules!(subst($e.as_slice(),
                            &[], $from, &[],
                            $to,
                            concat!(stringify!($from), " => ", stringify!($to)))
                      With $($t)*));
)

#[test]
fn test_subst() {
    assert_eq!(subst("hello", &[], "l", &[], "(ell)").as_slice(), "he(ell)(ell)o");

    assert_eq!(subst("hello", &["a".into_pre_cond()], "l", &[], "(ell)").as_slice(), "hello");
    assert_eq!(subst("hello", &["e".into_pre_cond()], "l", &[], "(ell)").as_slice(), "he(ell)lo");
    assert_eq!(subst("hello", &[], "l", &["a".into_post_cond()], "(ell)").as_slice(), "hello");
    assert_eq!(subst("hello", &[], "l", &["o".into_post_cond()], "(ell)").as_slice(), "hel(ell)o");

    fn vowel(c: Option<char>) -> bool { c.map_or(false, |c| "aeiou".contains_char(c)) }
    fn nasal(c: Option<char>) -> bool { c.map_or(false, |c| "nm".contains_char(c)) }
    fn clike(c: Option<char>) -> bool { c.map_or(false, |c| "ckx".contains_char(c)) }
    fn no_vowel(c: Option<char>) -> bool { !vowel(c) }
    fn boundary(c: Option<char>) -> bool { c.is_none() }

    assert_eq!(subst("francis",
                     &["fr".into_pre_cond(), vowel.into_pre_cond()],
                     nasal,
                     &[clike.into_post_cond(), "is".into_post_cond()],
                     "(n)").as_slice(),
               "fra(n)cis");
    assert_eq!(subst("humankind",
                     &[],
                     "man",
                     &["kind".into_post_cond(), boundary.into_post_cond()],
                     "woman").as_slice(),
               "huwomankind");

    assert_eq!(subst("alter", &[no_vowel.into_pre_cond()], "l", &[], "f").as_slice(), "alter");
    assert_eq!(subst("will", &[no_vowel.into_pre_cond()], "l", &[], "f").as_slice(), "wilf");
    assert_eq!(subst("list", &[no_vowel.into_pre_cond()], "l", &[], "f").as_slice(), "fist");

    assert_eq!(subst("pufffffff",
                     &["f".into_pre_cond()], "f", &["f".into_post_cond()], "ph").as_slice(),
               "pufphphphphphf");

    assert!(subst("hello", &[], "l", &[], "(ell)").is_owned());
    assert!(subst("bovine", &[], "l", &[], "(ell)").is_borrowed());

    assert_eq!(subst("syzygy", &[], "", &[], "/").as_slice(), "/s/y/z/y/g/y/");
    assert_eq!(subst("syzygy", &[boundary.into_pre_cond()], "", &[], "/").as_slice(), "/syzygy");
    assert_eq!(subst("syzygy", &[], "", &[boundary.into_pre_cond()], "/").as_slice(), "syzygy/");
}

