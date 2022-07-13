use std::fmt::{Debug, Display};

use crate::{self as matchrs, matcher, Explanation, Matcher};

/// [`Explanation`] returned by [`is_some`].
pub struct SomeExplanation<E: Explanation> {
    inner_explanation: Option<E>,
}

impl<E: Explanation> Display for SomeExplanation<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(inner) = &self.inner_explanation {
            if inner.is_empty() {
                Ok(())
            } else {
                write!(f, "is some and value {}", inner)
            }
        } else {
            f.write_str("is none")
        }
    }
}

impl<E: Explanation> Explanation for SomeExplanation<E> {
    fn is_empty(&self) -> bool {
        self.inner_explanation
            .as_ref()
            .map(|x| x.is_empty())
            .unwrap_or(false)
    }
}

/// Returns a matcher that succeeds if the input value is [`Some`] and its inner
/// value matches `some_matcher`.
///
/// ```
/// # use matchrs::{assert_that, matchers::{cmp::eq, option::is_some}};
/// assert_that!(Some(1), is_some(eq(1)));
/// ```
///
/// ```should_panic
/// # use matchrs::{assert_that, matchers::{ANY, option::is_some}};
/// assert_that!(None::<i32>, is_some(ANY));  // fails.
/// ```
#[matcher(expected = ("is {} value {}", switch("some and", "none or"), some_matcher.describe(options)), name = Some)]
pub fn is_some<T: Debug, M: Matcher<T>>(
    some_matcher: M,
    value: &Option<T>,
) -> Option<SomeExplanation<M::Explanation>> {
    if let Some(value) = value {
        Some(SomeExplanation {
            inner_explanation: Some(some_matcher.match_or_explain(value)?),
        })
    } else {
        Some(SomeExplanation {
            inner_explanation: None,
        })
    }
}

/// Returns a [`Matcher`] that succeeds if the input value is [`None`].
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, matchers::option::is_none};
/// assert_that!(None::<()>, is_none());
/// ```
///
/// ```should_panic
/// # use matchrs::{assert_that, matchers::{ANY, option::is_none}};
/// assert_that!(Some(1), is_none());  // fails.
/// ```
#[matcher(expected = ("{}", switch("is none", "is some")), name = None)]
pub fn is_none<T: Debug>(value: &Option<T>) -> bool {
    value.is_none()
}
