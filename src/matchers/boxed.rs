use std::fmt::{Debug, Display};

use crate::{self as matchrs, matcher, Matcher};

/// Returns a [`Matcher`] that operates on the value stored in a [`Box`].
#[matcher(expected = inner_matcher.describe(options).fmt(f))]
pub const fn boxed<T: Debug, M: Matcher<T>>(
    inner_matcher: M,
    value: &Box<T>,
) -> Option<M::Explanation> {
    inner_matcher.match_or_explain(value.as_ref())
}
