use std::convert::Infallible;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Write;

pub mod matchers;

/// Generates matchers for each field of a struct, or each variant of an enum.
///
/// ### Example
/// This macro can be used to match on fields on structs.
///
/// ```
/// # use matchrs::{assert_that, Match, matchers::cmp::eq};
/// # use std::fmt::{Debug, Display};
/// #[derive(Debug, Match)]
/// struct Person<'a, Age: Debug + Display + 'a> {
///     name: &'a str,
///     age: Age,
/// }
///
/// let alice = Person { name: "Alice", age: 42 };
///
/// assert_that!(alice, Person::age_is(eq(42)));
/// ```
///
/// ```should_panic
/// # use matchrs::{assert_that, Match, matchers::cmp::eq};
/// # use std::fmt::{Debug, Display};
/// # #[derive(Debug, Match)]
/// # struct Person<'a, Age: Debug + Display + 'a> {
/// #     name: &'a str,
/// #     age: Age,
/// # }
///
/// let bob = Person { name: "Bob", age: "<unknown>" };
///
/// assert_that!(bob, Person::name_is(eq("Carl")));  // fails
/// ```
///
/// ### Example
/// It can also be used to match on variants of enums.
/// ```
/// # use matchrs::{assert_that, Match, matchers::{boxed::boxed, cmp::eq}};
/// # use std::fmt::{Debug, Display};
/// #[derive(Debug, Match)]
/// enum Expr {
///     Inf,
///     Lit(i32),
///     Neg { v: Box<Expr> },
///     Add(Box<Expr>, Box<Expr>),
///     Sub { l: Box<Expr>, r: Box<Expr> },
/// }
///
/// let expr = Expr::Add(Expr::Lit(1).into(), Expr::Neg { v: Expr::Lit(-2).into() }.into());
///
/// assert_that!(
///     expr,
///     Expr::is_add(
///         boxed(Expr::is_lit(eq(1))),
///         boxed(Expr::is_neg(
///             boxed(Expr::is_lit(eq(-2)))))));
/// ```
pub use matchrs_derive::Match;

/// Implements [`Explanation`] for a value.
pub use matchrs_derive::Explanation;

/// Transforms a function into a [`Matcher`].
///
/// ### Example
/// ```
/// # use matchrs::{matcher, Matcher};
/// # use std::fmt::Debug;
/// #[matcher(expected = "ok value")]
/// fn is_ok<T: Debug, E: Debug, M: Matcher<T>>(ok_matcher: M, value: &Result<T, E>) -> bool {
///     if let Ok(value) = value { ok_matcher.match_or_explain(value).is_none() } else { false }
/// }
/// ```
pub use matchrs_derive::matcher;

/// A matcher which validates an input value `T`.
pub trait Matcher<T: ?Sized + Debug> {
    /// The type of the explanation returned by [`Matcher::match_or_explain`].
    ///
    /// [`NoExplanation`] should be used for cases where no explanation will
    /// ever be printed.
    type Explanation: Explanation;

    /// If the given value matches, returns [`None`]. Otherwise, returns an
    /// [`Explanation`] of why the match did not succeed.
    fn match_or_explain(&self, value: &T) -> Option<Self::Explanation>;

    /// Describes what kind of value is expected by this matcher.
    ///
    /// Messages should begin with a verb such as "is" or "must".
    fn describe<'d>(&'d self, options: &'d DescribeOptions) -> Description<'d, Self>;
}

impl<'m, T: Debug, M: Matcher<T>> Matcher<T> for &'m M {
    type Explanation = M::Explanation;

    fn match_or_explain(&self, value: &T) -> Option<Self::Explanation> {
        (*self).match_or_explain(value)
    }

    fn describe<'d>(&'d self, options: &'d DescribeOptions) -> Description<'d, Self> {
        Description::new(self, options, |&s, options, f| s.describe(options).fmt(f))
    }
}

/// The description returned by [`Matcher::describe`].
#[derive(Clone, Copy)]
pub struct Description<'a, T: ?Sized> {
    value: &'a T,
    options: &'a DescribeOptions,
    fmt: fn(&'a T, &'a DescribeOptions, &mut Formatter<'_>) -> std::fmt::Result,
}

impl<'a, T> Description<'a, T> {
    /// Creates a new description given its value, options, and formatting
    /// implementation.
    pub fn new(
        value: &'a T,
        options: &'a DescribeOptions,
        fmt: fn(&'a T, &'a DescribeOptions, &mut Formatter<'_>) -> std::fmt::Result,
    ) -> Self {
        Self {
            value,
            options,
            fmt,
        }
    }
}

impl<'a, T> Display for Description<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (self.fmt)(self.value, self.options, f)
    }
}

/// Explanation why [`Matcher::match_or_explain`] failed.
pub trait Explanation: Display {
    /// Returns whether the explanation is empty, i.e. there is no explanation.
    fn is_empty(&self) -> bool {
        struct DidWrite(bool);

        impl Write for DidWrite {
            fn write_str(&mut self, s: &str) -> std::fmt::Result {
                self.0 |= !s.is_empty();
                Ok(())
            }

            fn write_char(&mut self, _c: char) -> std::fmt::Result {
                self.0 = true;
                Ok(())
            }
        }

        let mut did_write = DidWrite(false);
        let _ = write!(&mut did_write, "{}", self);

        did_write.0
    }
}

impl Explanation for Infallible {
    fn is_empty(&self) -> bool {
        true
    }
}

impl Explanation for String {
    fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }
}

impl Explanation for &'_ str {
    fn is_empty(&self) -> bool {
        self.len() > 0
    }
}

/// Marker type used when no [explanation](Matcher::Explanation) is given by
/// [`Matcher::match_or_explain`].
#[derive(Clone, Copy, Debug, Default)]
pub struct NoExplanation;

impl Display for NoExplanation {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

impl Explanation for NoExplanation {
    fn is_empty(&self) -> bool {
        true
    }
}

/// Object given to [`Matcher::describe`].
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct DescribeOptions {
    /// Whether the description should be negated.
    pub is_negated: bool,
}

impl DescribeOptions {
    /// Returns a new [`DescribeOptions`].
    pub const fn new() -> Self {
        Self { is_negated: false }
    }

    /// Returns a new negated [`DescribeOptions`].
    pub const fn negated(&self) -> Self {
        Self {
            is_negated: !self.is_negated,
        }
    }
}

/// Asserts that the given `value` is accepted by the specified `matcher`.
pub fn assert_that<T: Debug>(value: &T, matcher: impl Matcher<T>) {
    let explanation = match matcher.match_or_explain(value) {
        Some(explanation) => explanation,
        None => return,
    };

    let mut message = format!(
        "assertion failed:\n\texpected: {}\n\tactual: {:?}",
        matcher.describe(&DescribeOptions::new()),
        value,
    );

    if !explanation.is_empty() {
        write!(&mut message, " ({explanation})").unwrap();
    }

    panic!("{message}")
}

/// Asserts that the given [`Matcher`] matches the specified value.
#[macro_export]
macro_rules! assert_that {
    ( $value: expr, $matcher: expr) => {
        $crate::assert_that(&$value, $matcher)
    };
}
