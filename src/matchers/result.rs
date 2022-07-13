use std::fmt::{Debug, Display, Formatter};

use crate::{self as matchrs, matcher, Explanation, Matcher};

macro_rules! define_explanation {
    ( $name: ident, $variant: ident, $doc: literal, $fmt: literal ) => {
        #[doc = $doc]
        pub struct $name<E: Explanation> {
            inner: E,
        }

        impl<E: Explanation> Display for $name<E> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                if self.inner.is_empty() {
                    Ok(())
                } else {
                    write!(f, $fmt, self.inner)
                }
            }
        }

        impl<E: Explanation> Explanation for $name<E> {
            fn is_empty(&self) -> bool {
                self.inner.is_empty()
            }
        }
    };
}

define_explanation!(
    OkExplanation,
    Ok,
    "[`Explanation`] returned by [`is_ok`].",
    "is ok and value {}"
);

define_explanation!(
    ErrExplanation,
    Err,
    "[`Explanation`] returned by [`is_err`].",
    "is err and error {}"
);

/// Returns a matcher that succeeds if the input value is [`Ok`] and its inner
/// value matches `ok_matcher`.
///
/// ```
/// # use matchrs::{assert_that, matchers::{cmp::eq, result::is_ok}};
/// assert_that!(Ok::<i32, ()>(1), is_ok(eq(1)));
/// ```
///
/// ```should_panic
/// # use matchrs::{assert_that, matchers::{ANY, cmp::eq, result::is_ok}};
/// assert_that!(Ok::<i32, ()>(1), is_ok(eq(2)));  // fails
/// assert_that!(Err::<i32, ()>(()), is_ok(ANY));  // fails
/// ```
#[matcher(expected = ("is {} value {}", switch("ok and", "err or"), ok_matcher.describe(options)), name = Ok)]
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

/// Returns a matcher that succeeds if the input value is [`Err`] and its inner
/// error matches `err_matcher`.
///
/// ```
/// # use matchrs::{assert_that, matchers::{cmp::eq, result::is_err}};
/// assert_that!(Err::<(), i32>(1), is_err(eq(1)));
/// ```
///
/// ```should_panic
/// # use matchrs::{assert_that, matchers::{ANY, cmp::eq, result::is_err}};
/// assert_that!(Err::<(), i32>(1), is_err(eq(2)));  // fails
/// assert_that!(Ok::<(), i32>(()), is_err(ANY));  // fails
/// ```
#[matcher(expected = ("is {} error {}", switch("err and", "ok or"), err_matcher.describe(options)), name = Err)]
pub fn is_err<T: Debug, E: Debug, M: Matcher<E>>(
    err_matcher: M,
    value: &Result<T, E>,
) -> Option<ErrExplanation<M::Explanation>> {
    if let Err(error) = value {
        Some(ErrExplanation {
            inner: err_matcher.match_or_explain(error)?,
        })
    } else {
        None
    }
}
