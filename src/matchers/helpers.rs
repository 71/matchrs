use std::fmt::{Debug, Display, Formatter};

use crate::{
    self as matchrs, matcher, DescribeOptions, Description, Explanation, Matcher, NoExplanation,
};

/// [`Explanation`] returned by [`property`].
#[derive(Explanation)]
#[matchrs(explanation = "`{name}` {inner}", is_empty = inner.is_empty())]
pub struct PropertyExplanation<E: Explanation> {
    name: &'static str,
    inner: E,
}

/// Returns a [`Matcher`] that succeeds if the specified property of the given
/// value matches `prop_matcher`.
///
/// See [`field!`] and [`property!`] for less redundant expressions.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, Matcher, matchers::{cmp::IS_FALSE, helpers::property}};
/// assert_that!(
///     vec![1], property("is_empty()", IS_FALSE, |v: &Vec<_>, m| m.match_or_explain(&v.is_empty())));
/// ```
#[matcher(expected = ("struct where `{name}` {}", prop_matcher.describe(options)))]
pub fn property<T: Debug, P: Debug, M: Matcher<P>, F: Fn(&T, &M) -> Option<M::Explanation>>(
    name: &'static str,
    prop_matcher: M,
    map: F,
    value: &T,
) -> Option<PropertyExplanation<M::Explanation>> {
    Some(PropertyExplanation {
        name,
        inner: map(value, &prop_matcher)?,
    })
}

/// Returns a [`Matcher`] that succeeds if the specified field of the input
/// object matches the given `field_matcher`.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, field, matchers::cmp::eq};
/// #[derive(Debug)]
/// struct Foo { foo: &'static str }
///
/// assert_that!(Foo { foo: "foo" }, field!(Foo::foo, eq("foo")));
/// ```
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, field, matchers::cmp::eq};
/// #[derive(Debug)]
/// struct Bar(&'static str, &'static str);
///
/// assert_that!(Bar("foo", "bar"), field!(Bar::1, eq("bar")));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! field {
    ( $( $name: tt $(::< $($args: ty),* >)? )::+, $field_matcher: expr ) => {
        $crate::matchers::helpers::property(
            __extract_from_path!(@name $($name $(::<$($args),*>)?)::+),
            $field_matcher,
            |x: &__extract_from_path!(@type $($name $(::<$($args),*>)?)::+), inner| {
                $crate::Matcher::match_or_explain(
                    inner,
                    &__extract_from_path!(@x $($name $(::<$($args),*>)?)::+)
                )
            },
        )
    };
}

/// Returns a [`Matcher`] that succeeds if the specified property of the input
/// object matches the given `prop_matcher`.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, property, matchers::cmp::{IS_FALSE, IS_TRUE}};
/// assert_that!(vec![], property!(Vec::<i32>::is_empty, IS_TRUE));
/// assert_that!(vec![1, 2, 3], property!(Vec::is_empty, IS_FALSE));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! property {
    ( $( $name: ident $(::< $($args: ty),* >)? )::+, $prop_matcher: expr ) => {
        $crate::matchers::helpers::property(
            std::concat!(__extract_from_path!(@name $($name $(::<$($args),*>)?)::+), "()"),
            $prop_matcher,
            |x, inner| {
                $crate::Matcher::match_or_explain(
                    inner,
                    &$($name $(::<$($args),*>)?)::+(x)
                )
            },
        )
    };
}

/// Extracts the `name` or `type` of a path given to [`field!`] or
/// [`property!`].
///
/// ### Example
/// ```
/// # use matchrs::__extract_from_path;
/// assert_eq!(__extract_from_path!(@name bar::foo), "foo");
/// assert_eq!(__extract_from_path!(@name bar::baz::<usize>::foo), "foo");
/// ```
#[macro_export(local_inner_macros)]
macro_rules! __extract_from_path {
    ( @name      @path (:: $($path: tt)*) $name: tt ) => { std::stringify!($name) };
    ( @type      @path (:: $($path: tt)*) $name: tt ) => { $($path)* };
    ( @$x: ident @path (:: $($path: tt)*) $name: tt ) => { $x.$name };

    ( @$selected: ident @path ($($path: tt)*) $name: ident :: $( $rest: tt )* ) => {
        __extract_from_path!( @$selected @path ($($path)* :: $name) $( $rest )* )
    };

    ( @$selected: ident @path ($($path: tt)*) < $( $args: ty ),* > :: $( $rest: tt )* ) => {
        __extract_from_path!( @$selected @path ($($path)* < $( $args ),* >) $( $rest )* )
    };

    ( @$selected: ident @path ($($path: tt)*) $($rest: tt)* ) => {
        std::compile_error!(std::concat!("expected path, got ", std::stringify!($($rest)*)))
    };

    ( @$selected: ident $( $rest: tt )* ) => {
        __extract_from_path!( @$selected @path () $( $rest )* )
    };
}

/// [`Explanation`] returned by [`variant`].
#[derive(Explanation)]
#[matchrs(
    explanation = match inner { Some(x) => x.fmt(f), None => Ok(()) },
    is_empty = inner.as_ref().map(|x| x.is_empty()).unwrap_or(true))]
pub struct VariantExplanation<E: Explanation> {
    name: &'static str,
    inner: Option<E>,
}

/// Returns a [`Matcher`] that succeeds if the given value has the specified
/// variant, and the variant matches the specified matcher.
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, Matcher, matchers::{cmp::eq, helpers::variant}};
/// assert_that!(Ok(1), variant("Ok", eq(1), |v: &Result<_, ()>, m| match v {
///     Ok(x) => Some(Some(m.match_or_explain(x)?)),
///     Err(_) => Some(None),
/// }));
/// ```
#[matcher(expected = ("variant {} `{name}` {} {}", switch("is", "is not"), switch("and", "or"), variant_matcher.describe(options)))]
pub const fn variant<
    T: Debug,
    V: Debug,
    M: Matcher<V>,
    F: Fn(&T, &M) -> Option<Option<M::Explanation>>,
>(
    name: &'static str,
    variant_matcher: M,
    map: F,
    value: &T,
) -> Option<VariantExplanation<M::Explanation>> {
    Some(VariantExplanation {
        name,
        inner: map(value, variant_matcher)?,
    })
}

/// Returns a [`Matcher`] given its implementation of [`Matcher::match_or_explain`]
/// and [`Matcher::describe`].
pub fn infer<'m, T, E, MatchOrExplain, Describe>(
    match_or_explain: MatchOrExplain,
    describe: Describe,
) -> impl Matcher<T> + 'm
where
    T: 'm + Debug,
    E: 'm + Explanation,
    MatchOrExplain: 'm + Fn(&T) -> Option<E>,
    Describe: 'm + Fn(&DescribeOptions, &mut Formatter<'_>) -> std::fmt::Result,
{
    struct M<F, D> {
        match_or_explain: F,
        describe: D,
    }

    impl<'m, T, E, MatchOrExplain, Describe> Matcher<T> for M<MatchOrExplain, Describe>
    where
        T: 'm + Debug,
        E: 'm + Explanation,
        MatchOrExplain: 'm + Fn(&T) -> Option<E>,
        Describe: 'm + Fn(&DescribeOptions, &mut Formatter<'_>) -> std::fmt::Result,
    {
        type Explanation = E;

        fn match_or_explain(&self, value: &T) -> Option<Self::Explanation> {
            (self.match_or_explain)(value)
        }

        fn describe<'a>(&'a self, options: &'a DescribeOptions) -> Description<'a, Self> {
            Description::new(self, options, |s, d, f| (s.describe)(d, f))
        }
    }

    M {
        match_or_explain,
        describe,
    }
}

/// Returns a [`Matcher`] given its implementation of [`Matcher::match_or_explain`].
pub fn matches<'m, X, T, E, F>(expected: X, match_or_explain: F) -> impl Matcher<T> + 'm
where
    X: 'm + Display,
    T: 'm + Debug,
    E: 'm + Explanation,
    F: 'm + Fn(&T) -> Option<E>,
{
    infer(match_or_explain, move |_, out| write!(out, "{}", expected))
}

/// Returns a [`Matcher`] given its implementation of [`Matcher::match_or_explain`].
///
/// Equivalent to the [`matches`] function, but uses the string representation
/// of the function to describe its expected input.
#[macro_export]
macro_rules! matches {
    ( $p: pat => $body: expr ) => {
        $crate::matchers::matches(stringify!($body), |$pat| $body)
    };

    ( $body: expr ) => {
        $crate::matchers::matches(stringify!($body), $body)
    };
}

/// Returns a [`Matcher`] that fails if the given condition is true.
pub fn satisfies<'a, X, T, F>(expected: X, is_match: F) -> impl Matcher<T> + 'a
where
    X: 'a + Display,
    T: 'a + Debug,
    F: 'a + Fn(&T) -> bool,
{
    matches(expected, move |v: &T| {
        if is_match(v) {
            None
        } else {
            Some(NoExplanation)
        }
    })
}

/// Returns a [`Matcher`] that fails if the given condition is true.
///
/// Equivalent to the [`satisfies`] function, but uses the string representation
/// of the condition to describe its expected input.
#[macro_export]
macro_rules! satisfies {
    ( $p: pat => $body: expr ) => {
        $crate::matchers::satisfies(stringify!($body), |$pat| $body)
    };

    ( $body: expr ) => {
        $crate::matchers::satisfies(stringify!($body), $body)
    };
}
