use std::{cmp::Ordering, fmt::Debug};

use crate::{self as matchrs, matcher};

use super::{not, Not};

/// A [`Matcher`](crate::Matcher) that succeeds if its input value is `true`.
pub const IS_TRUE: EqMatcher<bool> = eq(true);

/// A [`Matcher`](crate::Matcher) that succeeds if its input value is `false`.
pub const IS_FALSE: EqMatcher<bool> = eq(false);

/// Returns a [`Matcher`] that succeeds if the input value is equal to the
/// specified expected value.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, matchers::cmp::eq};
/// assert_that!(2 + 2, eq(4));
/// ```
#[matcher(expected = ("is {} to {:?}", switch("equal", "not equal"), expected))]
pub const fn eq<T: Debug + PartialEq>(expected: T, value: &T) -> bool {
    expected == value
}

/// Returns a [`Matcher`] that succeeds if the input value is different from the
/// specified forbidden value.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, matchers::cmp::ne};
/// assert_that!(2 + 2, ne(3));
/// ```
pub const fn ne<T: Debug + PartialEq>(forbidden: T) -> Not<EqMatcher<T>> {
    not(eq(forbidden))
}

/// Returns a [`Matcher`] that succeeds if the input value is greater than the
/// specified expected value.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, matchers::cmp::is_gt};
/// assert_that!(2 + 1, is_gt(2));
/// ```
#[matcher(expected = ("is {} {:?}", switch("greater than", "lower than or equal to"), lt_value))]
pub const fn is_gt<T: Debug + PartialOrd>(lt_value: T, value: &T) -> bool {
    value.partial_cmp(lt_value) == Some(Ordering::Greater)
}

/// Returns a [`Matcher`] that succeeds if the input value is greater than or
/// equal to the specified expected value.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, matchers::cmp::is_ge};
/// assert_that!(2, is_ge(2));
/// assert_that!(2 + 1, is_ge(2));
/// ```
#[matcher(expected = ("is {} {:?}", switch("greater than or equal to", "lower than"), le_value))]
pub const fn is_ge<T: Debug + PartialOrd>(le_value: T, value: &T) -> bool {
    matches!(
        value.partial_cmp(le_value),
        Some(Ordering::Greater | Ordering::Equal)
    )
}

/// Returns a [`Matcher`] that succeeds if the input value is less than the
/// specified expected value.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, matchers::cmp::is_lt};
/// assert_that!(2 - 1, is_lt(2));
/// ```
#[matcher(expected = ("is {} {:?}", switch("less than", "greater than or equal to"), gt_value))]
pub const fn is_lt<T: Debug + PartialOrd>(gt_value: T, value: &T) -> bool {
    // Note: we can't just define `is_lt` as `not(is_ge(_))` since we're working with
    // partial ordering and `is_lt` != `not(is_ge(_))`.
    value.partial_cmp(gt_value) == Some(Ordering::Less)
}

/// Returns a [`Matcher`] that succeeds if the input value is less than or
/// equal to the specified expected value.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, matchers::cmp::is_le};
/// assert_that!(3, is_le(3));
/// assert_that!(3 - 1, is_le(3));
/// ```
#[matcher(expected = ("is {} {:?}", switch("lower than or equal to", "greater than"), ge_value))]
pub const fn is_le<T: Debug + PartialOrd>(ge_value: T, value: &T) -> bool {
    matches!(
        value.partial_cmp(ge_value),
        Some(Ordering::Less | Ordering::Equal)
    )
}
