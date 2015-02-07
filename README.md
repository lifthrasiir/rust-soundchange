# Rust-soundchange

A tool for implementing sound change algorithms.

This is strictly modeled after Mark Rosenfelder's [Sound Change Applier][sca],
with a necessary tweak for Rust's macro system.

[sca]: http://zompist.com/scahelp.html

Typical usage and the comparison with the original SCA rules:

```
#[macro_use] extern crate soundchange;
#[macro_use] extern crate log;

use soundchange::{CharOf, StrTo};

fn main() {
    // custom conditions
    let is_boundary = |c: Option<char>| c.is_none();
    let is_vowel = |c: Option<char>| c.map_or(false, |c| "aeiou".contains_char(c));
    let boundary = CharOf(&is_boundary);
    let vowel = CharOf(&is_vowel);

    // custom transformers
    let make_reverse = |s: &str, out: &mut String| out.extend(s.chars().rev());
    let reverse = StrTo(&make_reverse);

    let s = "fihs".to_string();
    let s = subst_rules! { s =>                 // V=aeiou
        "f" [boundary] => "gh";                 // f/gh/_#
        "f" => "ph";                            // f/ph/_
        ["w"] vowel ["m" vowel "n"] => "o";     // V/o/w_mVn
        "sh" ["o"] => "ti";                     // sh/ti/_o
        [vowel] "hs" => reverse;                // sh/\\/V_
        "" [boundary] => "ing";                 // /ing/_#
    };
    assert_eq!(s, "phishing");
}
```

Note: You can use `RUST_LOG=4` for tracking any change on the string
and rules that trigger that change. Also, a large number of rules may trigger
a recursion limit on rustc; you have to split them in that case.

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

## Case Study: English Pronunciation

`src/english` provides a reimplementation of Mark Rosenfelder's
[pronunciation algorithm][spell] for English.

[spell]: http://zompist.com/spell.html
