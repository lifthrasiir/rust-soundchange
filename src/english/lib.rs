/*!
A reimplementation of Mark Rosenfelder's [pronunciation algorithm][spell] for English.
Mostly an example for rust-soundchange, but also serves as an approximate algorithm.

[spell]: http://zompist.com/spell.html
*/

#[macro_use] extern crate soundchange;
#[macro_use] extern crate log;

use std::char;
use std::str;
use std::fmt;
use std::num::FromPrimitive;

use soundchange::{CharOf, CharTo};

/// A phoneme for English.
///
/// While variants are named after their common respelling
/// (slightly adjusted to fit in the ASCII and to be unique)
/// they do not directly represent an unique sound (say, voiceless bilabial stop /p/).
/// A phoneme rather represents an (conceptual) equivalence class for related sounds,
/// so that it is a minimal unit that can give the difference in meaning.
/// Different dialects of English commonly have different sounds for the same phoneme.
//
// Mark Rosenfelder's scheme doesn't distinguish alphabets and phonemes, so we need to be careful.
// we reuse most consonants, and then remap many vowels into UPPERCASED letters or
// digits or punctuations (ugh). for convenience, "long" vowels [1] are always assigned UPPERCASE.
//
// [1] they are actually not "long", but they have matching "short" vowels.
//     this is a direct result of the Great Vowel Shift, and they were indeed differentiated
//     only by the longness before the GVS, hence the name.
#[derive(PartialEq, Eq, FromPrimitive, Copy)]
pub enum Phoneme {
    // consonants
    P  = 'p' as int, // [p]    paper PAYP@R
    B  = 'b' as int, // [b]    book BUUK
    T  = 't' as int, // [t]    take TAYK
    D  = 'd' as int, // [d]    dead DED
    G  = 'g' as int, // [g]    get GET
    K  = 'k' as int, // [k]    talk TAWK
    M  = 'm' as int, // [m]    moon MOON
    N  = 'n' as int, // [n]    new NOO
    NG = 'ñ' as int, // [n`]   sing SING
    F  = 'f' as int, // [f]    four FOHR
    V  = 'v' as int, // [v]    vine VAIN
    TH = '+' as int, // [T]    thin THIN
//  DH               // [D]    this DHIS (not distinguished from TH in the computer rules)
    S  = 's' as int, // [s]    so SOH
    Z  = 'z' as int, // [z]    zoo ZOO
    SH = '$' as int, // [S]    shake SAYK
//  ZH               // [Z]    measure MEJEWR (only appears as a part of J in the computer rules)
    CH = 'ç' as int, // [tS]   chew CHOO
    J  = 'j' as int, // [dZ]   judge JUJ
    R  = 'r' as int, // [r]    ran RAN
    L  = 'l' as int, // [l]    late LAYT
    H  = 'h' as int, // [h]    hang HAYNG
    Y  = 'y' as int, // [j]    you JOO
    W  = 'w' as int, // [w]    cow KAW

    // untranslated consonants (should not appear in the final result)
    _C = 'c' as int,
    _Q = 'q' as int,
    _X = 'x' as int,

    // vowels
    AY = 'ä' as int, // [e(I)] rate RAYT
    A  = 'â' as int, // [{]    rat RAT
    EE = 'ë' as int, // [i:]   meet MEET
    E  = 'ê' as int, // [E]    met MET
    AI = 'ï' as int, // [aI]   bite BAIT
    I  = 'î' as int, // [I]    bit BIT
    OH = 'ö' as int, // [oU]   note NOHT
    O  = 'ô' as int, // [A]    not NOT
    EW = 'ü' as int, // [ju:]  cute KEWT
    U  = 'û' as int, // [V]    cut KUT
    OO = 'u' as int, // [u:]   coot KOOT
    AW = 'ù' as int, // [O:]   dog DAWG
    UU = 'ò' as int, // [U]    cook KUUK
    UH = '@' as int, // [@]    above UHBOV

    // untranslated vowels (should not appear in the final result)
    _A = 'a' as int,
    _E = 'e' as int,
    _I = 'i' as int,
    _O = 'o' as int,
}

impl Phoneme {
    #[inline]
    pub fn to_char(&self) -> char {
        char::from_u32(*self as u32).unwrap()
    }

    #[inline]
    pub fn from_char(c: char) -> Option<Phoneme> {
        FromPrimitive::from_u32(c as u32)
    }

    #[inline]
    pub fn is_vowel(&self) -> bool {
        match *self {
            Phoneme::AY | Phoneme::A  | Phoneme::EE | Phoneme::E  |
            Phoneme::AI | Phoneme::I  | Phoneme::OH | Phoneme::O  |
            Phoneme::EW | Phoneme::U  | Phoneme::OO | Phoneme::AW |
            Phoneme::UU | Phoneme::UH | Phoneme::_A | Phoneme::_E |
            Phoneme::_I | Phoneme::_O => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_consonant(&self) -> bool {
        match *self {
            Phoneme::P  | Phoneme::B  | Phoneme::T  | Phoneme::D  |
            Phoneme::G  | Phoneme::K  | Phoneme::M  | Phoneme::N  |
            Phoneme::NG | Phoneme::F  | Phoneme::V  | Phoneme::TH |
            Phoneme::S  | Phoneme::Z  | Phoneme::SH | Phoneme::CH |
            Phoneme::J  | Phoneme::R  | Phoneme::L  | Phoneme::H  |
            Phoneme::Y  | Phoneme::W  | Phoneme::_C | Phoneme::_Q |
            Phoneme::_X => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_untranslated(&self) -> bool {
        match *self {
            Phoneme::_C | Phoneme::_Q | Phoneme::_X | Phoneme::_A |
            Phoneme::_E | Phoneme::_I | Phoneme::_O => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_short_vowel(&self) -> bool {
        // AW `ù`, UU `ò` and UH `@` are neutral
        match *self {
            Phoneme::A  | Phoneme::E  | Phoneme::I  | Phoneme::O  |
            Phoneme::U  | Phoneme::AW | Phoneme::UU | Phoneme::UH => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_long_vowel(&self) -> bool {
        // AW `ù`, UU `ò` and UH `@` are neutral
        match *self {
            Phoneme::AY | Phoneme::EE | Phoneme::AI | Phoneme::OH |
            Phoneme::EW | Phoneme::AW | Phoneme::UU | Phoneme::UH => true,
            _ => false,
        }
    }

    #[inline]
    pub fn to_short_vowel(&self) -> Phoneme {
        match *self {
            Phoneme::AY | Phoneme::_A => Phoneme::A,
            Phoneme::EE | Phoneme::_E => Phoneme::E,
            Phoneme::AI | Phoneme::_I => Phoneme::I,
            Phoneme::OH | Phoneme::_O => Phoneme::O,
            Phoneme::EW | Phoneme::OO => Phoneme::U,
            p => p,
        }
    }

    #[inline]
    pub fn to_long_vowel(&self) -> Phoneme {
        match *self {
            Phoneme::A | Phoneme::_A => Phoneme::AY,
            Phoneme::E | Phoneme::_E => Phoneme::EE,
            Phoneme::I | Phoneme::_I => Phoneme::AI,
            Phoneme::O | Phoneme::_O => Phoneme::OH,
            Phoneme::U | Phoneme::OO => Phoneme::EW,
            p => p,
        }
    }
}

impl fmt::Show for Phoneme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.to_char().fmt(f) }
}

/// A sequence of phonemes.
pub struct Word { s: String }

impl Word {
    /// Creates a word from given phonemes.
    pub fn from_phonemes(ps: &[Phoneme]) -> Word {
        Word { s: ps.iter().map(|p| p.to_char()).collect() }
    }

    /// Generates a pronunciation of given English word.
    pub fn from_english(s: &str) -> Word {
        Word { s: spell_to_sound(s) }
    }

    /// Iterates through phonemes.
    pub fn phonemes<'a>(&'a self) -> WordPhonemes<'a> {
        WordPhonemes { base: self.s.chars() }
    }

    /// Returns a string notation of phonemes.
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.s.as_slice()
    }
}

impl fmt::Show for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { self.s.fmt(f) }
}

/// An iterator for every phoneme inside given word.
#[derive(Clone)]
pub struct WordPhonemes<'a> { base: str::Chars<'a> }

impl<'a> Iterator for WordPhonemes<'a> {
    type Item = Phoneme;
    fn next(&mut self) -> Option<Phoneme> {
        self.base.next().map(|c| Phoneme::from_char(c).unwrap())
    }
    fn size_hint(&self) -> (uint, Option<uint>) {
        self.base.size_hint()
    }
}

impl<'a> DoubleEndedIterator for WordPhonemes<'a> {
    fn next_back(&mut self) -> Option<Phoneme> {
        self.base.next_back().map(|c| Phoneme::from_char(c).unwrap())
    }
}

/// The actual alphabet-to-phoneme algorithm.
fn spell_to_sound(s: &str) -> String {
    // filter out any non-latin characters
    fn to_ascii_lower_or_none(c: char) -> Option<char> {
        match c {
            'a'...'z' => Some(c),
            'A'...'Z' => Some(char::from_u32(c as u32 + 32).unwrap()),
            _ => None,
        }
    }
    let buf: String = s.chars().filter_map(to_ascii_lower_or_none).collect();
    let s = buf.as_slice();

    // predicates
    let is_vowel = |&: c: Option<char>| {
        c.and_then(Phoneme::from_char).map_or(false, |c| c.is_vowel())
    };
    let is_consonant = |&: c: Option<char>| {
        c.and_then(Phoneme::from_char).map_or(false, |c| c.is_consonant())
    };
    let is_not_vowel = |&: c: Option<char>| !is_vowel(c);
    let is_unknown_vowel = |&: c: Option<char>| {
        match c.and_then(Phoneme::from_char) {
            Some(Phoneme::_A) |
            Some(Phoneme::_E) |
            Some(Phoneme::_I) |
            Some(Phoneme::_O) |
            // any untranslated `u` is automatically a phoneme, but it is unknown before that
            Some(Phoneme::OO) => true,
            _ => false
        }
    };
    let is_long_vowel = |&: c: Option<char>| {
        c.and_then(Phoneme::from_char).map_or(false, |c| c.is_long_vowel())
    };
    let is_boundary = |&: c: Option<char>| c.is_none();
    let is_not_boundary = |&: c: Option<char>| c.is_some();

    // transformers
    let make_short_vowel = |&: c: char| {
        Phoneme::from_char(c).map_or(c, |c| c.to_short_vowel().to_char())
    };
    let make_long_vowel = |&: c: char| {
        Phoneme::from_char(c).map_or(c, |c| c.to_long_vowel().to_char())
    };

    let vowel = CharOf(&is_vowel);
    let consonant = CharOf(&is_consonant);
    let no_vowel = CharOf(&is_not_vowel);
    let unknown_vowel = CharOf(&is_unknown_vowel);
    let long_vowel = CharOf(&is_long_vowel);
    let boundary = CharOf(&is_boundary);
    let no_boundary = CharOf(&is_not_boundary);
    let to_short_vowel = CharTo(&make_short_vowel);
    let to_long_vowel = CharTo(&make_long_vowel);

    // we need to split rules to work with the recursion limit
    let s = subst_rules! { s with
        // digraphs
        "ch" => "ç";
        "sh" => "$";
        "ph" => "f";
        "th" => "+";
        "qu" => "kw";

        // spelling-level changes
        "w" ["r"] => "";
        "w" ["ho"] => "";
        ["w"] "h" => "";
        [boundary "r"] "h" => "";
        ["x"] "h" => "";
        [vowel] "h" [boundary] => "";
        [boundary "e"] "x" [vowel] => "gz";
        "x" => "ks";

        // gh
        "gh" [vowel] => "g";
        [consonant] vowel ["gh"] => to_long_vowel;
        "ough" ["t"] => "ò";
        "augh" ["t"] => "ò";
        "ough" => "ö";
        "gh" => "";

        // unpronouncable combinations
        [boundary] "g" ["n"] => "";
        [boundary] "k" ["n"] => "";
        [boundary] "m" ["n"] => "";
        [boundary] "p" ["t"] => "";
        [boundary] "p" ["s"] => "";
        [boundary] "t" ["m"] => "";

        // medial y = i
        [boundary consonant] "y" [boundary] => "ï";
        [boundary consonant consonant] "y" [boundary] => "ï";
        [boundary consonant consonant consonant] "y" [boundary] => "ï";
        "ey" => "ë";
        "ay" => "ä";
        "oy" => "öy";
        [consonant] "y" [no_vowel] => "i";
        [consonant] "y" ["e" boundary] => "i";
        [no_vowel consonant] "ie" [boundary] => "ï";

        // sSl simplification
        ["s"] "t" ["l" vowel boundary] => "";

        // affrication of t + front vowel
        [no_boundary] "ci" [vowel] => "$";
        [no_boundary] "ti" [vowel] => "$";
        [no_boundary] "tu" [vowel] => "çu";
        [no_boundary] "tu" ["r" vowel] => "çu";
        [no_boundary] "tu" ["l" vowel] => "çu";
        [consonant] "si" ["o"] => "$";
        [vowel] "si" ["o"] => "j";
        [consonant] "s" ["ur"] => "$";
        [vowel] "s" ["ur"] => "j";
        ["k"] "s" ["u" vowel] => "$";
        ["k"] "s" ["ur"] => "$";
        ["k"] "s" ["ul"] => "$";

        // intervocalic s
        ["e"] "s" [vowel] => "z";
        ["i"] "s" [vowel] => "z";
        ["o"] "s" [vowel] => "z";
        ["u"] "s" [vowel] => "z";
    };

    let s = subst_rules! { s.as_slice() with
        // al to ol
        "a" ["ls"] => "ò";
        "a" ["lr"] => "ò";
        "a" ["ll" boundary] => "ò";
        "a" ["lm" boundary] => "ò";
        "a" ["lm" vowel boundary] => "ò";
        [no_vowel] "a" ["lt"] => "ò";
        [no_vowel] "a" ["ld"] => "ò";
        [no_vowel] "a" ["l+"] => "ò";
        [no_boundary] "al" ["k"] => "ò";

        // soft c and g
        "c" ["e"] => "s";
        "c" ["i"] => "s";
        "c" ["ê"] => "s";
        "c" ["î"] => "s";
        "c" ["y"] => "s";
        "c" => "k";
        [no_boundary] "ge" ["a"] => "j";
        [no_boundary] "ge" ["o"] => "j";
        "g" ["e"] => "j";
        "g" ["i"] => "j";
        "g" ["ê"] => "j";
        "g" ["î"] => "j";
        "g" ["y"] => "j";

        // g-hardening guF
        [boundary] "gu" ["e"] => "g";
        [boundary] "gu" ["i"] => "g";
        [boundary] "gu" ["ê"] => "g";
        [boundary] "gu" ["î"] => "g";
        [boundary] "gu" ["y"] => "g";
        "gu" ["e" boundary] => "g";

        // reverse-written final liquids
        [consonant] "re" [boundary] => "@r";
        [consonant] "le" [boundary] => "@l";
    };

    let s = subst_rules! { s.as_slice() with
        // vowels are long medially and short before 2 consonants or a final one
        [no_vowel] unknown_vowel [consonant vowel] => to_long_vowel;
        [no_vowel] unknown_vowel [consonant no_vowel] => to_short_vowel;

        // special but general rules
        "î" ["nd" boundary] => "ï";
        "ô" ["ss" boundary] => "ò";
        "ô" ["g" boundary] => "ò";
        "ô" ["f" consonant] => "ò";
        "ô" ["lt"] => "ö";
        "ô" ["ld"] => "ö";
        "ô" ["l+"] => "ö";
        ["w"] "â" ["$"] => "ò";
        ["w"] "â" ["ç"] => "ò";
        ["w"] "â" ["tç"] => "ò";
        ["w"] "â" ["t"] => "ô";
        ["w"] "â" ["d"] => "ô";
        ["w"] "â" ["n"] => "ô";
        ["w"] "â" ["s"] => "ô";
        ["w"] "â" ["+"] => "ô";

        // soft gn
        "îg" ["m" no_vowel] => "ï";
        "îg" ["n" no_vowel] => "ï";
        "îg" ["ñ" no_vowel] => "ï";
        ["ei"] "g" ["n"] => "";

        // handle ous
        "ou" ["s" no_vowel] => "@";

        // remove silent -e
        [vowel consonant] "e" [boundary] => "";
        [vowel consonant consonant] "e" [boundary] => "";
        [vowel consonant consonant consonant] "e" [boundary] => "";
    };

    let s = subst_rules! { s.as_slice() with
        // common affixes
        [no_boundary no_boundary no_boundary] "ë" ["mênt" boundary] => "";
        [no_boundary no_boundary no_boundary] "ë" ["nêss" boundary] => "";
        [no_boundary no_boundary no_boundary] "ë" ["li" boundary] => "";
        [no_boundary no_boundary no_boundary] "ë" ["fûl" boundary] => "";
        [no_boundary no_boundary no_boundary] "ï" ["nêss" boundary] => "ë";

        // shorten (1-char) weak penults after a long
        [long_vowel consonant] long_vowel [consonant vowel boundary] => to_short_vowel;
        [long_vowel consonant consonant] long_vowel [consonant vowel boundary] => to_short_vowel;
        [long_vowel consonant consonant consonant] long_vowel [consonant vowel boundary] =>
            to_short_vowel;

        // double vowels
        "eau" => "ö";
        "ai" => "ä";
        "au" => "ò";
        "âw" => "ò";
        "ee" => "ë";
        "ea" => "ë";
        ["s"] "ei" => "ë";
        "ei" => "ä";
        "eo" => "ë@";
        "êw" => "ü";
        "eu" => "ü";
        "ie" => "ë";
        ["i"] vowel => "@";
        [boundary consonant] "i" => "ï";
        [boundary consonant consonant] "i" => "ï";
        "i" ["@"] => "ë";
        "oa" => "ö";
        "oe" [boundary] => "ö";
        "oo" ["k"] => "ù";
        "oo" => "u";
        "oul" ["d" boundary] => "ù";
        "ou" => "ôw";
        "oi" => "öy";
        "ua" => "ü@";
        "ue" => "u";
        "ui" => "u";
        "ôw" [boundary] => "ö";
    };

    let s = subst_rules! { s.as_slice() with
        // pesky final syllables
        // XXX the original `english.tc` is incorrect: `V/@/VC(V)_l#` should be `U/@/VC(V)_l#`
        [vowel consonant] unknown_vowel ["l" boundary] => "@";
        [vowel consonant] "ê" ["n" boundary] => "@";
        [vowel consonant] "î" ["n" boundary] => "@";
        [vowel consonant] "â" ["n" boundary] => "@";
        [vowel consonant] "ô" ["n" boundary] => "@";
        [vowel consonant vowel] unknown_vowel ["l" boundary] => "@";
        [vowel consonant consonant] "ê" ["n" boundary] => "@";
        [vowel consonant consonant] "î" ["n" boundary] => "@";
        [vowel consonant consonant] "â" ["n" boundary] => "@";
        [vowel consonant consonant] "ô" ["n" boundary] => "@";

        // suffix simplifications
        [no_boundary no_boundary no_boundary] "a" ["b@l" boundary] => "@";
        [no_boundary no_boundary no_boundary] "ä" ["b@l" boundary] => "@";
        [no_boundary no_boundary no_boundary] "â" ["b@l" boundary] => "@";
        [no_boundary "l"] "ë" ["@n" boundary] => "y";
        [no_boundary "n"] "ë" ["@n" boundary] => "y";

        // unpronounceable finals
        ["m"] "b" [boundary] => "";
        ["m"] "n" [boundary] => "";

        // color the final vowels
        "a" [boundary] => "@";
        "e" [boundary] => "ë";
        "i" [boundary] => "ë";
        "o" [boundary] => "ö";

        // vowels before r
        "ôw" ["r" no_boundary] => "ö";
        "ô" ["r"] => "ö";
        "ò" ["r"] => "ö";
        ["w"] "â" ["r" no_vowel] => "ö";
        "ê" ["rr"] => "ä";
        "ë" ["ri" consonant] => "ä";
        "ë" ["rï" consonant] => "ä";
        "ë" ["rî" consonant] => "ä";
        "â" ["rr"] => "ä";
        "â" ["r" no_vowel] => "ô";
        "â" ["r"] => "ä";
        "ê" ["r"] => "@";
        "î" ["r"] => "@";
        "û" ["r"] => "@";
        "ù" ["r"] => "@";
    };

    let s = subst_rules! { s.as_slice() with
        // handle ng
        "ng" ["p"] => "ñ";
        "ng" ["t"] => "ñ";
        "ng" ["k"] => "ñ";
        "ng" ["b"] => "ñ";
        "ng" ["d"] => "ñ";
        "ng" ["g"] => "ñ";
        "ng" ["f"] => "ñ";
        "ng" ["s"] => "ñ";
        "ng" ["$"] => "ñ";
        "ng" ["+"] => "ñ";
        "ng" [boundary] => "ñ";
        "n" ["g"] => "ñ";
        "n" ["k"] => "ñ";
        "ô" ["ñ"] => "ò";
        "â" ["ñ"] => "ä";

        // more morphophonological rules
        ["b"] "s" [boundary] => "z";
        ["d"] "s" [boundary] => "z";
        ["g"] "s" [boundary] => "z";
        "s" ["m" boundary] => "z";

        // double consonants
        "s" ["s"] => "";
        "s" ["$"] => "";
        "t" ["t"] => "";
        "t" ["ç"] => "";
        "p" ["p"] => "";
        "k" ["k"] => "";
        "b" ["b"] => "";
        "d" ["d"] => "";
        "d" ["j"] => "";
        "g" ["g"] => "";
        "n" ["n"] => "";
        "m" ["m"] => "";
        "r" ["r"] => "";
        "l" ["l"] => "";
        "f" ["f"] => "";
        "z" ["z"] => "";

    };

    s
}

#[test]
fn test_spell_to_sound() {
    assert_eq!(spell_to_sound("coffee").as_slice(), "kòfë");
    assert_eq!(spell_to_sound("market").as_slice(), "môrkêt");
    assert_eq!(spell_to_sound("ugly").as_slice(), "ûglë");
    assert_eq!(spell_to_sound("high").as_slice(), "hï");
    assert_eq!(spell_to_sound("canal").as_slice(), "känâl");
    assert_eq!(spell_to_sound("although").as_slice(), "òl+ö");
    assert_eq!(spell_to_sound("assure").as_slice(), "â$ür");
    assert_eq!(spell_to_sound("fish").as_slice(), "fî$");
}
