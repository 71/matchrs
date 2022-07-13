//! [`Matcher`]s based on tuples of matchers.
//!
//! See also [`either!`] and [`all!`].
//!
//! ### Example
//! ```
//! use matchrs::{assert_that, matchers::cmp::eq};
//! assert_that!((&1, &2, &3), (eq(1), eq(2), eq(3)));
//! ```
use std::{
    fmt::{Debug, Display, Formatter},
    marker::PhantomData,
};

use crate::{DescribeOptions, Description, Explanation, Matcher};

/// [`Explanation`] returned by tuple [`Matcher`]s.
pub struct TupleExplanation<T> {
    explanations: T,
}

/// [`Matcher`] which succeeds if any of its inner matchers succeeds.
///
/// See [`either!`].
pub struct Either<T: Debug, Tuple>(Tuple, PhantomData<*const T>);

impl<T: Debug> Either<T, ()> {
    #[doc(hidden)]
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> crate::matchers::Never {
        crate::matchers::NEVER
    }
}

impl<T: Debug, M: Matcher<T>> Either<T, (M,)> {
    #[doc(hidden)]
    #[allow(clippy::new_ret_no_self)]
    pub fn new(m: M) -> M {
        m
    }
}

/// [`Explanation`] returned by [`Either`] [`Matcher`]s.
pub struct EitherExplanation<T> {
    explanations: T,
}

/// [`Matcher`] which succeeds if all of its inner matchers succeed.
///
/// See [`all!`].
pub struct All<T: Debug, Tuple>(Tuple, PhantomData<*const T>);

impl<T: Debug> All<T, ()> {
    #[doc(hidden)]
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> crate::matchers::Always {
        crate::matchers::ANY
    }
}

impl<T: Debug, M: Matcher<T>> All<T, (M,)> {
    #[doc(hidden)]
    #[allow(clippy::new_ret_no_self)]
    pub fn new(m: M) -> M {
        m
    }
}

/// [`Explanation`] returned by [`All`] [`Matcher`]s.
pub struct AllExplanation<T> {
    explanations: T,
}

macro_rules! define_tuple_matcher {
    ( $( $i: tt $v_ident: ident: $ty_ident: ident $mty_ident: ident )* ) => {
        // impl `Either<(...)>`
        impl<T: Debug, $( $mty_ident: Matcher<T> ),*> Either<T, ( $( $mty_ident ),* )> {
            /// Creates a new [`Either`] matcher given all of its inner
            /// matchers.
            #[allow(clippy::too_many_arguments)]
            pub const fn new( $( $v_ident: $mty_ident ),* ) -> Self {
                Self(( $( $v_ident ),* ), PhantomData)
            }
        }

        // impl `All<(...)>`
        impl<T: Debug, $( $mty_ident: Matcher<T> ),*> All<T, ( $( $mty_ident ),* )> {
            /// Creates a new [`All`] matcher given all of its inner matchers.
            #[allow(clippy::too_many_arguments)]
            pub const fn new( $( $v_ident: $mty_ident ),* ) -> Self {
                Self(( $( $v_ident ),* ), PhantomData)
            }
        }

        // impl `Display for TupleExplanation<(...)>`
        impl<$( $ty_ident: Explanation ),*> Display for
                TupleExplanation<( $(Option<$ty_ident>),* )> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                let Self { explanations } = self;
                let mut wrote = false;

                $(
                    if let Some(explanation) = &explanations.$i {
                        if wrote {
                            f.write_str(" and ")?;
                        } else {
                            wrote = true;
                        }

                        write!(f, "element #{} {}", $i, explanation)?;
                    }
                )*

                let _ = wrote;

                Ok(())
            }
        }

        // impl `Display for EitherExplanation<(...)>`
        impl<$( $ty_ident: Explanation ),*> Display for EitherExplanation<( $($ty_ident),* )> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                let Self { explanations } = self;

                $(
                    if $i > 0 {
                        f.write_str(" and ")?;
                    }

                    explanations.$i.fmt(f)?;
                )*

                Ok(())
            }
        }

        // impl `Display for AllExplanation<(...)>`
        impl<$( $ty_ident: Explanation ),*> Display for AllExplanation<( $(Option<$ty_ident>),* )> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                let Self { explanations } = self;
                let mut wrote = false;

                $(
                    if let Some(explanation) = &explanations.$i {
                        if wrote {
                            f.write_str(" and ")?;
                        } else {
                            wrote = true;
                        }

                        explanation.fmt(f)?;
                    }
                )*

                let _ = wrote;

                Ok(())
            }
        }

        // impl `Explanation for TupleExplanation<(...)>`
        impl<$( $ty_ident: Explanation ),*> Explanation for
                TupleExplanation<( $(Option<$ty_ident>),* )> {
            fn is_empty(&self) -> bool {
                let Self { explanations } = self;

                $( explanations.$i.as_ref().map(|x| x.is_empty()).unwrap_or(true) )&&*
            }
        }

        // impl `Explanation for EitherExplanation<(...)>`
        impl<$( $ty_ident: Explanation ),*> Explanation for EitherExplanation<( $($ty_ident),* )> {
            fn is_empty(&self) -> bool {
                let Self { explanations } = self;

                $( explanations.$i.is_empty() )&&*
            }
        }

        // impl `Explanation for AllExplanation<(...)>`
        impl<$( $ty_ident: Explanation ),*> Explanation for
                AllExplanation<( $(Option<$ty_ident>),* )> {
            fn is_empty(&self) -> bool {
                let Self { explanations } = self;

                $( explanations.$i.as_ref().map(|x| x.is_empty()).unwrap_or(true) )&&*
            }
        }

        // impl `Matcher<(...)> for (...)`
        impl<$( $ty_ident: Debug, )* $( $mty_ident: Matcher<$ty_ident> ),*>
                Matcher<( $(&$ty_ident),* )> for ( $( $mty_ident ),* ) {
            type Explanation = TupleExplanation<( $(Option<$mty_ident::Explanation>),* )>;

            fn match_or_explain(&self, value: &( $(&$ty_ident),* )) -> Option<Self::Explanation> {
                let explanations = ( $( self.$i.match_or_explain(value.$i) ),* );

                if $( explanations.$i.is_none() )&&* {
                    None
                } else {
                    Some(TupleExplanation { explanations })
                }
            }

            fn describe<'d>(&'d self, options: &'d DescribeOptions) -> Description<'d, Self> {
                Description::new(self, options, |matchers, options, f| {
                    let join_with = if options.is_negated { " or " } else { " and " };

                    $(
                        if $i > 0 {
                            f.write_str(join_with)?;
                        }

                        write!(f, "(element #{} {})", $i, matchers.$i.describe(options))?;
                    )*

                    Ok(())
                })
            }
        }

        // impl `Matcher<(...)> for Either<(...)>`
        impl<T: Debug, $( $mty_ident: Matcher<T> ),*>
                Matcher<T> for Either<T, ( $( $mty_ident ),* )> {
            type Explanation = EitherExplanation<( $($mty_ident::Explanation),* )>;

            fn match_or_explain(&self, value: &T) -> Option<Self::Explanation> {
                let explanations = ( $( self.0.$i.match_or_explain(value)? ),* );

                Some(EitherExplanation { explanations })
            }

            fn describe<'d>(&'d self, options: &'d DescribeOptions) -> Description<'d, Self> {
                Description::new(self, options, |Either(matchers, _), options, f| {
                    let join_with = if options.is_negated { " and " } else { " or " };

                    $(
                        if $i > 0 {
                            f.write_str(join_with)?;
                        }

                        matchers.$i.describe(options).fmt(f)?;
                    )*

                    Ok(())
                })
            }
        }

        // impl `Matcher<(...)> for All<(...)>`
        impl<T: Debug, $( $mty_ident: Matcher<T> ),*> Matcher<T> for All<T, ( $( $mty_ident ),* )> {
            type Explanation = AllExplanation<( $(Option<$mty_ident::Explanation>),* )>;

            fn match_or_explain(&self, value: &T) -> Option<Self::Explanation> {
                let explanations = ( $( self.0.$i.match_or_explain(value) ),* );

                if $( explanations.$i.is_none() )&&* {
                    None
                } else {
                    Some(AllExplanation { explanations })
                }
            }

            fn describe<'d>(&'d self, options: &'d DescribeOptions) -> Description<'d, Self> {
                Description::new(self, options, |All(matchers, _), options, f| {
                    let join_with = if options.is_negated { " or " } else { " and " };

                    $(
                        if $i > 0 {
                            f.write_str(join_with)?;
                        }

                        matchers.$i.describe(options).fmt(f)?;
                    )*

                    Ok(())
                })
            }
        }
    };
}

/// Returns a [`Matcher`] that succeeds if any of the given matchers succeeds.
///
/// This macro is a simple wrapper around [`Either`].
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, either, matchers::cmp::eq};
/// assert_that!(2, either!(eq(1), eq(2)));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! either {
    ( $( $matchers: expr ),* ) => {
        // Since we define `Either::new` multiple times, we have to help the
        // compiler infer the number of elements in the tuple.
        $crate::matchers::tuple::Either::<_, ($(__underscore![$matchers],)*)>::new( $($matchers),* )
    };
}

/// Returns a [`Matcher`] that succeeds if all of the given matchers succeed.
///
/// This macro is a simple wrapper around [`All`].
///
/// ### Example
/// ```
/// # use matchrs::{assert_that, all, matchers::cmp::ne};
/// assert_that!(3, all!(ne(1), ne(2)));
/// ```
#[macro_export(local_inner_macros)]
macro_rules! all {
    ( $( $matchers: expr ),* ) => {
        // Since we define `All::new` multiple times, we have to help the
        // compiler infer the number of elements in the tuple.
        $crate::matchers::tuple::All::<_, ($(__underscore![$matchers],)*)>::new( $($matchers),* )
    };
}

/// Ignores its input, and produces a single `_` token.
#[doc(hidden)]
#[macro_export]
macro_rules! __underscore {
    ( $( $t: tt )* ) => {
        _
    };
}

// Note: `Debug` impls are defined for up to 12 tuple items, so we similarly
// only define all the `impl`s for up to 12 inner matchers.

define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME    5  f: F MF    6  g: G MG    7  h: H MH
                       8  i: I MI    9  j: J MJ    10 k: K MK    11 l: L ML );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME    5  f: F MF    6  g: G MG    7  h: H MH
                       8  i: I MI    9  j: J MJ    10 k: K MK               );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME    5  f: F MF    6  g: G MG    7  h: H MH
                       8  i: I MI    9  j: J MJ                             );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME    5  f: F MF    6  g: G MG    7  h: H MH
                       8  i: I MI                                           );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME    5  f: F MF    6  g: G MG    7  h: H MH );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME    5  f: F MF    6  g: G MG               );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME    5  f: F MF                             );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD
                       4  e: E ME                                           );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC    3  d: D MD );
define_tuple_matcher!( 0  a: A MA    1  b: B MB    2  c: C MC               );
define_tuple_matcher!( 0  a: A MA    1  b: B MB                             );
