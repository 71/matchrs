use std::convert::Infallible;
use std::fmt::{Debug, Display};

pub mod boxed;
pub mod cmp;
pub mod helpers;
pub mod iter;
pub mod option;
pub mod result;
pub mod tuple;

use crate::{DescribeOptions, Description, Matcher, NoExplanation};

/// A [`Matcher`] that always matches.
pub struct Always;

impl<T: Debug> Matcher<T> for Always {
    type Explanation = Infallible;

    fn match_or_explain(&self, _value: &T) -> Option<Self::Explanation> {
        None
    }

    fn describe<'d>(&'d self, options: &'d DescribeOptions) -> Description<'d, Self> {
        Description::new(self, options, |_, DescribeOptions { is_negated, .. }, f| {
            f.write_str(if *is_negated {
                "is nothing"
            } else {
                "is anything"
            })
        })
    }
}

/// A [`Matcher`] that matches any value.
pub const ANY: Always = Always;

/// [`Matcher`] that negates its inner matcher.
pub struct Not<M> {
    matcher_to_negate: M,
}

impl<T: Debug, M: Matcher<T>> Matcher<T> for Not<M> {
    type Explanation = NoExplanation;

    fn match_or_explain<'a>(&'a self, value: &'a T) -> Option<Self::Explanation> {
        if self.matcher_to_negate.match_or_explain(value).is_some() {
            return None;
        }

        Some(NoExplanation)
    }

    fn describe<'d>(&'d self, options: &'d DescribeOptions) -> Description<'d, Self> {
        Description::new(self, options, |Self { matcher_to_negate }, options, f| {
            matcher_to_negate.describe(&options.negated()).fmt(f)
        })
    }
}

/// Returns a [`Matcher`] that negates the specified matcher.
pub const fn not<T: Debug, M: Matcher<T>>(matcher_to_negate: M) -> Not<M> {
    Not { matcher_to_negate }
}

/// A [`Matcher`] that never matches.
pub type Never = Not<Always>;

/// A [`Matcher`] that matches no value.
pub const NEVER: Never = Not {
    matcher_to_negate: ANY,
};
