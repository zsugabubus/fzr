#![allow(clippy::missing_safety_doc, clippy::mut_from_ref)]
use bumpalo::Bump;
use core::ffi::c_int;
use fzr::{
    Haystack, Match, Memory, Pattern, PatternError, Token, find_exact, find_fuzzy, parse_haystack,
};
use std::{cmp::Reverse, collections::BinaryHeap, ffi::CStr};

mod lua;

enum Needle {
    Never,
    Always,
    Fuzzy(Pattern),
    Exact(Pattern, bool),
}

impl Needle {
    fn parse(s: &str) -> Self {
        if let Some(s) = s.strip_prefix('\'') {
            if let Some(s) = s.strip_prefix(' ') {
                Pattern::new(s).map(Needle::Fuzzy)
            } else {
                Pattern::new(s).map(|pat| Needle::Exact(pat, false))
            }
        } else if let Some(s) = s.strip_prefix('^') {
            if let Some(s) = s.strip_prefix(' ') {
                Pattern::new(s).map(Needle::Fuzzy)
            } else {
                Pattern::new(s).map(|pat| Needle::Exact(pat, true))
            }
        } else {
            Pattern::new(s).map(Needle::Fuzzy)
        }
        .unwrap_or_else(|e| match e {
            PatternError::Empty => Needle::Always,
            PatternError::TooLong => Needle::Never,
        })
    }

    fn find<'m>(&self, haystack: &Haystack, memory: &'m mut Memory) -> Option<Match<'m>> {
        match self {
            Self::Never => None,
            Self::Always => unreachable!(),
            Self::Exact(pat, anchor_start) => {
                find_exact(haystack, pat, *anchor_start, false, memory)
            }
            Self::Fuzzy(pat) => find_fuzzy(haystack, pat, memory),
        }
    }
}

#[ouroboros::self_referencing]
struct Searcher {
    bump: Bump,
    #[borrows(bump)]
    #[covariant]
    haystacks: &'this [Haystack<'this, &'this str, &'this [Token]>],
    #[borrows(bump)]
    keys: &'this mut [u32],
    matches_len: usize,
}

enum ResultItem {
    Value,
    Index,
    Positions,
    FlatPositions,
    Group(Box<ResultItem>),
    Seq(Vec<ResultItem>),
}

impl ResultItem {
    fn parse(s: &str) -> Result<Self, &'static str> {
        fn peek(input: &[u8], pos: usize) -> Option<u8> {
            input.get(pos).copied()
        }

        fn atom(input: &[u8], pos: usize) -> Result<Option<(usize, ResultItem)>, &'static str> {
            match input.get(pos).copied() {
                Some(b'v') => Ok(Some((pos + 1, ResultItem::Value))),
                Some(b'i') => Ok(Some((pos + 1, ResultItem::Index))),
                Some(b'p') => Ok(Some((pos + 1, ResultItem::Positions))),
                Some(b'P') => Ok(Some((pos + 1, ResultItem::FlatPositions))),
                Some(b'{') => match seq(input, pos + 1)? {
                    Some((pos, item)) => match peek(input, pos) {
                        Some(b'}') => Ok(Some((pos + 1, ResultItem::Group(Box::new(item))))),
                        Some(_) => Err("invalid character"),
                        None => Err("expected '}' before end of string"),
                    },
                    None => match peek(input, pos + 1) {
                        Some(b'}') => Err("invalid '{}'"),
                        Some(_) => Err("invalid character"),
                        None => Err("expected '}' before end of string"),
                    },
                },
                _ => Ok(None),
            }
        }

        fn seq(input: &[u8], mut pos: usize) -> Result<Option<(usize, ResultItem)>, &'static str> {
            let mut items = Vec::with_capacity(1);
            loop {
                pos = match atom(input, pos)? {
                    Some((pos, item)) => {
                        items.push(item);
                        pos
                    }
                    None => {
                        return if items.is_empty() {
                            Ok(None)
                        } else if items.len() == 1 {
                            Ok(Some((pos, items.pop().unwrap())))
                        } else {
                            Ok(Some((pos, ResultItem::Seq(items))))
                        };
                    }
                };
            }
        }

        let bytes = s.as_bytes();
        match seq(bytes, 0)? {
            Some((pos, item)) => {
                let eof = pos == bytes.len();
                if eof {
                    Ok(item)
                } else {
                    Err("invalid character")
                }
            }
            None => {
                if bytes.is_empty() {
                    Err("empty")
                } else {
                    Err("invalid character")
                }
            }
        }
    }

    fn count_items(&self, m: &Option<Match>) -> usize {
        match self {
            Self::Value => 1,
            Self::Index => 1,
            Self::Positions => m.as_ref().map_or(1, |m| m.ranges().len()),
            Self::FlatPositions => Self::Positions.count_items(m) * 2,
            Self::Group(..) => 1,
            Self::Seq(items) => items.iter().map(|item| item.count_items(m)).sum(),
        }
    }

    fn has_positions(&self) -> bool {
        match self {
            Self::Value | Self::Index => false,
            Self::Positions | Self::FlatPositions => true,
            Self::Group(item) => item.has_positions(),
            Self::Seq(items) => items.iter().any(|item| item.has_positions()),
        }
    }
}

impl lua::Userdata for Needle {
    const NAME: &CStr = c"fzr.Needle";
}

impl lua::Userdata for Searcher {
    const NAME: &CStr = c"fzr.Searcher";
}

impl lua::Userdata for Memory {
    const NAME: &CStr = c"fzr.Memory";
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn luaopen_fzr(lua: lua::StateRef) -> c_int {
    unsafe {
        lua.create_usermeta::<Needle>();
        lua.pop();

        lua.create_usermeta::<Searcher>();
        lua.pop();

        lua.create_usermeta::<Memory>();
        lua.pop();

        lua.push_map(4);
        lua.set_table_str_fn("create_needle", create_needle);
        lua.set_table_str_fn("create_searcher", create_searcher);
        lua.set_table_str_fn("create_memory", create_memory);
        lua.set_table_str_fn("find", find);
    }
    1
}

pub unsafe extern "C" fn create_needle(lua: lua::StateRef) -> c_int {
    let s = unsafe { lua.get_str(1).unwrap_or("") };
    let needle = Needle::parse(s);
    unsafe {
        lua.push_userdata(needle);
    }
    1
}

pub unsafe extern "C" fn create_searcher(lua: lua::StateRef) -> c_int {
    let array = 1;
    let n = unsafe { lua.get_len(array) as c_int };

    let searcher = SearcherBuilder {
        bump: Bump::new(),
        keys_builder: |bump| bump.alloc_slice_fill_iter(0..n as u32),
        haystacks_builder: |bump| {
            let mut tokens = Vec::with_capacity(16);
            bump.alloc_slice_fill_iter((0..n).map(|i| unsafe {
                lua.get_index_raw(array, i + 1);

                let s = lua.get_str(-1).unwrap_or("");

                tokens.clear();
                parse_haystack(s, &mut tokens);

                let value = &*bump.alloc_str(s);
                let tokens = &*bump.alloc_slice_copy(tokens.as_slice());

                lua.pop();

                Haystack::from_parts(value, tokens)
            }))
        },
        matches_len: 0,
    }
    .build();

    unsafe {
        lua.push_userdata(searcher);
    }
    1
}

pub unsafe extern "C" fn create_memory(lua: lua::StateRef) -> c_int {
    unsafe {
        lua.push_userdata(Memory::new());
    }
    1
}

pub unsafe extern "C" fn find(lua: lua::StateRef) -> c_int {
    unsafe fn get_option<T>(
        lua: lua::StateRef,
        key: &str,
        get: impl FnOnce(lua::StateRef, i32) -> Option<T>,
    ) -> Option<T> {
        unsafe {
            if lua.is_nil(4) {
                return None;
            }
            lua.get_table_str(4, key);
            let value = if lua.is_nil(-1) { None } else { get(lua, -1) };
            lua.pop();
            value
        }
    }

    unsafe fn push_result(
        lua: lua::StateRef,
        item: &ResultItem,
        i: i32,
        m: &Option<Match>,
        v: &str,
        k: usize,
    ) -> i32 {
        match item {
            ResultItem::Value => {
                unsafe {
                    lua.push_str(v);
                    lua.set_index_raw(-2, i);
                }
                i + 1
            }
            ResultItem::Index => {
                unsafe {
                    lua.push_number((k + 1) as f64);
                    lua.set_index_raw(-2, i);
                }
                i + 1
            }
            ResultItem::Positions => {
                if let Some(m) = m {
                    m.ranges().fold(i, |i, range| {
                        unsafe {
                            lua.push_array(2);
                            lua.push_number(range.start as f64);
                            lua.set_index_raw(-2, 1);
                            lua.push_number((range.end) as f64);
                            lua.set_index_raw(-2, 2);
                            lua.set_index_raw(-2, i);
                        }
                        i + 1
                    })
                } else {
                    i
                }
            }
            ResultItem::FlatPositions => {
                if let Some(m) = m {
                    m.ranges().fold(i, |i, range| {
                        unsafe {
                            lua.push_number(range.start as f64);
                            lua.set_index_raw(-2, i);
                            lua.push_number(range.end as f64);
                            lua.set_index_raw(-2, i + 1);
                        }
                        i + 2
                    })
                } else {
                    i
                }
            }
            ResultItem::Group(item) => unsafe {
                lua.push_array(item.count_items(m));
                push_result(lua, item, 1, m, v, k);
                lua.set_index_raw(-2, i);
                i + 1
            },
            ResultItem::Seq(items) => items
                .iter()
                .fold(i, |i, item| unsafe { push_result(lua, item, i, m, v, k) }),
        }
    }

    let Some(needle) = (unsafe { lua.get_userdata::<Needle>(1) }) else {
        return unsafe { lua.false_error("invalid needle") };
    };
    let Some(searcher) = (unsafe { lua.get_userdata::<Searcher>(2) }) else {
        return unsafe { lua.false_error("invalid searcher") };
    };
    let Some(memory) = (unsafe { lua.get_userdata::<Memory>(3) }) else {
        return unsafe { lua.false_error("invalid memory") };
    };
    let limit = unsafe {
        get_option(lua, "limit", |lua, index| {
            Some(lua.get_number(index) as usize)
        })
    }
    .unwrap_or_else(|| searcher.borrow_haystacks().len());
    let shrink = unsafe { get_option(lua, "shrink", |lua, index| Some(lua.get_boolean(index))) }
        .unwrap_or(false);
    let result_spec =
        unsafe { get_option(lua, "result", |lua, index| lua.get_str(index)) }.unwrap_or("v");
    let result = match ResultItem::parse(result_spec) {
        Ok(x) => x,
        Err(s) => return unsafe { lua.false_error(&format!("invalid result: {s}")) },
    };

    if matches!(needle, Needle::Always) {
        let haystacks = searcher.borrow_haystacks();
        let v = haystacks.iter().take(limit);

        unsafe {
            lua.push_array(v.len() * result.count_items(&None));
        }

        v.enumerate().fold(1, |i, (k, haystack)| unsafe {
            push_result(lua, &result, i, &None, haystack.value(), k)
        });

        return 1;
    }

    let mut heap = BinaryHeap::with_capacity(limit + 1);

    searcher.with_mut(|searcher| {
        let matches_len = *searcher.matches_len;

        let mut i = 0;
        let mut n = if shrink {
            matches_len
        } else {
            searcher.keys.len()
        };
        assert!(n <= searcher.keys.len());

        while i < n {
            let k = searcher.keys[i] as usize;
            let haystack = unsafe { searcher.haystacks.get_unchecked(k) };

            if let Some(score) = needle.find(haystack, memory).map(|m| m.score()) {
                i += 1;
                heap.push((Reverse(score), k));
                if heap.len() > limit as usize {
                    heap.pop();
                }
            } else {
                n -= 1;
                searcher.keys.swap(i, n);
            }
        }

        *searcher.matches_len = n;
    });

    let haystacks = searcher.borrow_haystacks();
    let v = heap.into_sorted_vec();

    unsafe {
        lua.push_array(v.len() * result.count_items(&None));
    }

    v.into_iter().fold(1, |i, (_, k)| {
        let haystack = unsafe { haystacks.get_unchecked(k) };

        let m = if result.has_positions() {
            needle.find(haystack, memory)
        } else {
            None
        };

        unsafe { push_result(lua, &result, i, &m, haystack.value(), k) }
    });

    1
}
