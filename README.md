# matchrs

[GoogleTest](https://google.github.io/googletest/)-inspired matchers for Rust.

```rust
assert_that!(foo(), either!(is_some(eq(1), is_none())));
```

This library is the fruit of yak-shaving, please let me know if you want me to
document it more thorougly.

## Automatic struct and enum matchers

In order to match on structs and their fields, macros can be used.

```rust
use matchrs::{assert_that, Match, matchers::{boxed::boxed, cmp::eq}};
use std::fmt::{Debug, Display};

// Define an expression; we use the `Match` derive to generate `is_variant`
// functions for each variant of `Expr`.
#[derive(Debug, Match)]
enum Expr {
    Inf,
    Lit(i32),
    Neg { v: Box<Expr> },
    Add(Box<Expr>, Box<Expr>),
    Sub { l: Box<Expr>, r: Box<Expr> },
}

let expr = Expr::Add(Expr::Lit(1).into(),
                     Expr::Neg { v: Expr::Lit(-2).into() }.into());

assert_that!(
    expr,
    Expr::is_add(
        boxed(Expr::is_lit(eq(1))),
        boxed(Expr::is_neg(
            boxed(Expr::is_lit(eq(-2)))))));
```

## Defining matchers

Matchers can be defined using the `#[matcher]` macro; for instance, `is_ok` is
defined as such:

```rust
//                                   #2 vvvvvvvvvvvvvvvvvvvvvvvvvv
#[matcher(expected = ("is {} value {}", switch("ok and", "err or"),
                                        ok_matcher.describe(options)))]
//                 #1 ^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ #3
pub fn is_ok<T: Debug, E: Debug, M: Matcher<T>>(
    ok_matcher: M,
    value: &Result<T, E>,
) -> Option<OkExplanation<M::Explanation>> {
    if let Ok(value) = value {
        Some(OkExplanation {
            inner: ok_matcher.match_or_explain(value)?,
        })
    } else {
        None
    }
}
```

Notes:
1. `expected` has special support for format strings; if the given expression
   cannot be passed as an argument to `write!`, then `expected` should be an
   expression which returns `std::fmt::Result` with access to the formatter
   under the name `f`.
2. In `expected`, the `fn switch(&'static str, &'static str) -> &'static str`
   function is made available; the first string is chosen for normal matchers,
   and the second one is chosen if the matcher is negated.
3. Variables are available in `expected`; matchers have a `.describe(options)`
   method which returns a `Display` value that can be included in the output
   of the expectation of `is_ok`.

`is_ok` can be used the following way:

```rust
assert_that!(Ok::<i32, ()>(1), is_ok(eq(1)));
```

Note the `OkExplanation` struct above. `Explanation`s are used to provide
additional details when a `Matcher` fails; if no such details are to be
given, `NoExplanation` can be returned.

`#[derive(Explanation)]` can be used to easily define new `Explanation`
types:

```rust
#[derive(Explanation)]
#[matchrs(
    explanation = match inner { Some(x) => x.fmt(f), None => Ok(()) },
    is_empty = inner.as_ref().map(|x| x.is_empty()).unwrap_or(true))]
pub struct OkExplanation<E: Explanation> {
    inner: Option<E>,
}
```
