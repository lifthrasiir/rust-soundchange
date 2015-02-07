#![feature(core, io, collections)]

extern crate "soundchange-english" as english;

use std::old_io as io;
use std::str;
use english::Word;

// if the "word" contains a hyphen or space, we should split it.
fn map_words<Sep, F>(mut s: &str, sep: Sep, mut f: F) -> String
        where Sep: str::CharEq + Clone, F: FnMut(&str) -> String {
    let mut ret = String::new();
    loop {
        match s.find(sep.clone()) {
            Some(i) => {
                let j = s.char_range_at(i).next;
                if i > 0 { ret.push_str(&f(&s[..i])); }
                ret.push_str(&s[i..j]);
                s = &s[j..];
            }
            None => {
                if !s.is_empty() { ret.push_str(&f(s)); }
                return ret;
            }
        }
    }
}

fn main() {
    let mut stdin = io::BufferedReader::new(io::stdin());
    for line in stdin.lines().filter_map(|s| s.ok()) {
        let mut line = line.trim();

        // if the line contains -->, drop that and following
        if let Some(sep) = line.find_str("-->") {
            line = line[..sep].trim_right();
        }

        // if the line looks like a word-actual pair, process that
        if let Some(sep) = line.find('/') {
            let word = line[..sep].trim_right();
            let actual = line[sep + 1..].trim_left();
            let expected = map_words(word, &[' ', '-'][], |w| Word::from_english(w).to_string());
            println!("{} / {} --> {}", word, actual, expected);
        }
    }
}
