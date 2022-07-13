use std::fmt::Debug;

use crate::{self as matchrs, matcher, Matcher};

/// Returns a matcher that succeeds if the input slice is empty.
///
/// ```
/// # use matchrs::{assert_that, matchers::iter::is_empty};
/// assert_that!(&[] as &[i32], is_empty());
/// assert_that!(Vec::<i32>::new(), is_empty());
/// ```
///
/// ```should_panic
/// # use matchrs::{assert_that, matchers::iter::is_empty};
/// assert_that!(&[1], is_empty());  // fails
/// ```
#[matcher(expected = ("{} slice", switch("an empty", "a non-empty")), name = Empty)]
pub fn is_empty<T: Debug, V: AsRef<[T]> + Debug>(value: &V) -> bool {
    value.as_ref().is_empty()
}

/// Returns a matcher that succeeds if each of the items in the input slice
/// matches `item_matcher`.
///
/// ```
/// # use matchrs::{assert_that, matchers::{cmp::eq, iter::each_item}};
/// assert_that!(&[], each_item(eq(1)));
/// assert_that!(&[1, 1], each_item(eq(1)));
/// assert_that!(vec![1], each_item(eq(1)));
/// ```
///
/// ```should_panic
/// # use matchrs::{assert_that, matchers::{ANY, cmp::eq, iter::each_item}};
/// assert_that!(&[1, 2, 3], each_item(eq(1)));  // fails
/// ```
#[matcher(expected = ("a slice where {} item {}", switch("each", "any"), item_matcher.describe(options)))]
pub fn each_item<'i, T: Debug, V: AsRef<[T]> + Debug, M: Matcher<T>>(
    item_matcher: M,
    value: &V,
) -> Option<M::Explanation> {
    for item in value.as_ref() {
        if let Some(explanation) = item_matcher.match_or_explain(item) {
            return Some(explanation);
        }
    }

    None
}
