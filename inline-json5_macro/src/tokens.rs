//! Loess is grammar agnostic, so anything more specific than [`Punct`] or [`Literal`] is built from scratch here.
//! (I may include some of the utility containers in future versions.)
//!
//! This is a bit more involved since it requires custom conditional parsing.

use std::str::FromStr;

use loess::{Error, ErrorPriority, Errors, Input, IntoTokens, PeekFrom, PopFrom};
use proc_macro2::{Delimiter, Ident, Literal, Punct, TokenStream, TokenTree, extra::DelimSpan};

// This is probably the most interesting, so I'll put it first.
// I'll make the implementation a bit more general so that you can reuse it more easily.
/// Repeating `T` separated by `Delimiter`.
pub struct Delimited<T, Delimiter>(pub Vec<(T, Option<Delimiter>)>);

impl<T: PeekFrom, D> PeekFrom for Delimited<T, D> {
	fn peek_from(input: &Input) -> bool {
		input.is_empty() || T::peek_from(input)
	}
}

impl<T: PopFrom, D: PeekFrom + PopFrom> PopFrom for Delimited<T, D> {
	fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
		let mut items = vec![];
		while !input.is_empty() {
			if let Ok(value) = T::pop_from(input, errors) {
				if let Ok(delimiter) = (!input.is_empty())
					.then(|| D::pop_from(input, errors))
					.transpose()
				{
					items.push((value, delimiter));
				} else {
					// An error has been emitted at this point, so "silently" recover.
					items.push((value, recover(input)))
				}
			} else {
				// An error has been emitted at this point, so "silently" recover.
				recover::<D>(input);
			}
		}

		/// Skips tokens until it finds `Delimiter` or `input` is empty.
		///
		/// Then, parses a `Delimiter` if available.
		///
		/// # Returns
		///
		/// [`Some`], unless the input is reached first.
		fn recover<Delimiter: PeekFrom + PopFrom>(input: &mut Input) -> Option<Delimiter> {
			// Errors that are reported here are very likely to be noise.
			// There should have been an error that was reported before reaching this point, so suppressing them is okay.
			let mut suppressed_errors = Errors::new();
			while !input.is_empty() {
				let len_before = input.len();
				match Delimiter::peek_pop_from(input, &mut suppressed_errors) {
					Ok(Some(delimiter)) => return Some(delimiter),
					Ok(None) => drop(input.tokens.pop_front()), // Not found; skip and continue.
					Err(()) => {
						if input.len() == len_before {
							// Stalled. Skip one.
							drop(input.tokens.pop_front());
						}
						// Continue.
					}
				}
			}
			None
		}

		Ok(Self(items))
	}
}

impl<T: IntoTokens, D: IntoTokens> IntoTokens for Delimited<T, D> {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		// Note that no check to ensure `None` only appears last is performed here.
		// Such a case should be impossible with the current `PopFrom`-implementations.
		// Even if it happened, it would just lead to a compile error further down the line, though.
		for (value, delimiter) in self.0 {
			value.into_tokens(root, tokens);
			delimiter.into_tokens(root, tokens);
		}
	}
}

// I like this format of grammar-like documentation, personally, but it really does nothing here.
/// `[` [`T`](`TokenStream`) `]`
pub struct SquareBrackets<T = TokenStream> {
	pub contents: T,
	pub span: DelimSpan,
}

impl<T> PeekFrom for SquareBrackets<T> {
	fn peek_from(input: &Input) -> bool {
		// If your grammar is more complex, you may need to require `T: PeekFrom` and check inside.
		// Here that's not needed, though.
		matches!(input.front(), Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Bracket)
	}
}

impl<T: PopFrom> PopFrom for SquareBrackets<T> {
	fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
		input
			.pop_or_replace(|tts, _| match tts {
				[TokenTree::Group(group)] if group.delimiter() == Delimiter::Bracket => {
					let mut input = Input {
						tokens: group.stream().into_iter().collect(),
						end: group.span_close(),
					};
					let this = match T::pop_from(&mut input, errors) {
						Ok(contents) => Self {
							contents,
							span: group.delim_span(),
						},
						Err(()) => return Err([group.into()]),
					};
					if !input.is_empty() {
						errors.push(Error::new(
							ErrorPriority::UNCONSUMED_IN_DELIMITER,
							"Unconsumed token inside `[…]`.",
							[input.front_span()],
						));
					}
					Ok(this)
				}
				other => Err(other),
			})
			.map_err(|spans| errors.push(Error::new(ErrorPriority::TOKEN, "Expected `[`.", spans)))
	}
}

/// `{` [`T`](`TokenStream`) `}`
pub struct Braces<T = TokenStream> {
	pub contents: T,
	pub span: DelimSpan,
}

impl<T> PeekFrom for Braces<T> {
	fn peek_from(input: &Input) -> bool {
		// If your grammar is more complex, you may need to require `T: PeekFrom` and check inside.
		// Here that's not needed, though.
		matches!(input.front(), Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Brace)
	}
}

impl<T: PopFrom> PopFrom for Braces<T> {
	fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
		input
			.pop_or_replace(|tts, _| match tts {
				[TokenTree::Group(group)] if group.delimiter() == Delimiter::Brace => {
					let mut input = Input {
						tokens: group.stream().into_iter().collect(),
						end: group.span_close(),
					};
					let this = match T::pop_from(&mut input, errors) {
						Ok(contents) => Self {
							contents,
							span: group.delim_span(),
						},
						Err(()) => return Err([group.into()]),
					};
					if !input.is_empty() {
						errors.push(Error::new(
							ErrorPriority::UNCONSUMED_IN_DELIMITER,
							"Unconsumed token inside `{…}`.",
							[input.front_span()],
						));
					}
					Ok(this)
				}
				other => Err(other),
			})
			.map_err(|spans| errors.push(Error::new(ErrorPriority::TOKEN, "Expected `{`.", spans)))
	}
}

/// `(` [`T`](`TokenStream`) `)`
pub struct Parentheses<T = TokenStream> {
	pub contents: T,
	pub span: DelimSpan,
}

impl<T> PeekFrom for Parentheses<T> {
	fn peek_from(input: &Input) -> bool {
		// If your grammar is more complex, you may need to require `T: PeekFrom` and check inside.
		// Here that's not needed, though.
		matches!(input.front(), Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis)
	}
}

impl<T: PopFrom> PopFrom for Parentheses<T> {
	fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
		input
			.pop_or_replace(|tts, _| match tts {
				[TokenTree::Group(group)] if group.delimiter() == Delimiter::Parenthesis => {
					let mut input = Input {
						tokens: group.stream().into_iter().collect(),
						end: group.span_close(),
					};
					let this = match T::pop_from(&mut input, errors) {
						Ok(contents) => Self {
							contents,
							span: group.delim_span(),
						},
						Err(()) => return Err([group.into()]),
					};
					if !input.is_empty() {
						errors.push(Error::new(
							ErrorPriority::UNCONSUMED_IN_DELIMITER,
							"Unconsumed token inside `(…)`.",
							[input.front_span()],
						));
					}
					Ok(this)
				}
				other => Err(other),
			})
			.map_err(|spans| errors.push(Error::new(ErrorPriority::TOKEN, "Expected `(`.", spans)))
	}
}

macro_rules! punctuation {
    ($($name:ident $char:literal),*$(,)?) => {$(
        // It's not really necessary to capture the `Punct` here,
        // but it's often nice to reuse input in the output directly.
        #[doc = concat!('`', $char, '`')]
        pub struct $name(pub Punct);

        impl PeekFrom for $name {
            fn peek_from(input: &Input) -> bool {
                // JSON5 doesn't have multi-punct punctuation, so spacing can be safely ignored here.
                matches!(input.front(), Some(TokenTree::Punct(punct)) if punct.as_char() == $char)
            }
        }

        impl PopFrom for $name {
            fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
                input
                    .pop_or_replace(|tts, _| match tts {
                        [TokenTree::Punct(punct)] if punct.as_char() == $char => Ok(Self(punct)),
                        other => Err(other),
                    })
                    .map_err(|spans| errors.push(Error::new(ErrorPriority::TOKEN, concat!("Expected `", $char, "`."), spans)))
            }
        }

        impl IntoTokens for $name {
            fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
                self.0.into_tokens(root, tokens)
            }
        }
    )*};
}

punctuation! {
	Colon ':',
	Comma ',',
	Dot '.',
	Minus '-',
	Plus '+',
}

macro_rules! keywords {
    ($($name:ident $str:literal),*$(,)?) => {$(
        #[doc = concat!('`', $str, '`')]
        pub struct $name(pub Ident);

        impl PeekFrom for $name {
            fn peek_from(input: &Input) -> bool {
                matches!(input.front(), Some(TokenTree::Ident(ident)) if ident == $str)
            }
        }

        impl PopFrom for $name {
            fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
                input
                    .pop_or_replace(|tts, _| match tts {
                        [TokenTree::Ident(ident)] if ident == $str => Ok(Self(ident)),
                        other => Err(other),
                    })
                    .map_err(|spans| {
                        errors.push(Error::new(
                            ErrorPriority::TOKEN,
                            concat!("Expected `", $str, "`."),
                            spans,
                        ))
                    })
            }
        }
    )*};
}

keywords! {
	False "false",
	Infinity "infinity",
	NaN "NaN",
	Null "null",
	True "true",
}

pub struct NumberLiteral(pub Option<Dot>, pub Literal);

impl PeekFrom for NumberLiteral {
	fn peek_from(input: &Input) -> bool {
		Dot::peek_from(input)
			|| if let Some(TokenTree::Literal(literal)) = input.front() {
				let s = literal.to_string();
				if s.starts_with("0x") {
					return true;
				}
				for c in s.chars() {
					if !(c.is_ascii_digit() || c == '.' || c == 'e') {
						return false;
					}
				}
				true
			} else {
				false
			}
	}
}

impl PopFrom for NumberLiteral {
	fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
		if let Some(dot) = Dot::peek_pop_from(input, errors).expect("infallible") {
			input
				.pop_or_replace(|tts, _| {
					if let [TokenTree::Literal(literal)] = tts {
						let s = literal.to_string();
						for c in s.chars() {
							if !(c.is_ascii_digit() || c == 'e') {
								return Err([literal.into()]);
							}
						}
						Ok(Self(Some(dot), literal))
					} else {
						Err(tts)
					}
				})
				.map_err(|spans| spans.into_iter().collect::<Vec<_>>())
		} else {
			input
				.pop_or_replace(|tts, _| {
					if let [TokenTree::Literal(literal)] = tts {
						let s = literal.to_string();
						if s.starts_with("0x") {
							return Ok(Self(None, literal));
						}
						for c in s.chars() {
							if !(c.is_ascii_digit() || c == '.' || c == 'e') {
								return Err([literal.into()]);
							}
						}
						Ok(Self(None, literal))
					} else {
						Err(tts)
					}
				})
				.map_err(|spans| spans.into_iter().collect())
		}
		.map_err(|spans| {
			errors.push(Error::new(
				ErrorPriority::TOKEN,
				concat!("Expected number literal."),
				spans,
			))
		})
	}
}

impl IntoTokens for NumberLiteral {
	fn into_tokens(self, _root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		match self.0 {
			Some(Dot(_)) => tokens.extend([Literal::from_str(&format!("0.{}", self.1))
				.expect("This should be reasonably constrained by the parsing step.")
				.into()]),
			None => tokens.extend([self.1.into()]),
		}
	}
}

pub struct String(pub Literal);

impl PeekFrom for String {
	fn peek_from(input: &Input) -> bool {
		matches!(input.front(), Some(TokenTree::Literal(literal)) if literal.to_string().starts_with('"'))
	}
}

impl PopFrom for String {
	fn pop_from(input: &mut Input, _errors: &mut Errors) -> Result<Self, ()> {
		// This should always be guarded behind `::peek_from`, so I'll use assertions here.

		let Some(TokenTree::Literal(literal)) = input.tokens.pop_front() else {
			unreachable!("Expected String.")
		};

		assert!(literal.to_string().starts_with('"'), "Expected String.");

		Ok(Self(literal))
	}
}

pub struct Identifier(pub Ident);

impl PeekFrom for Identifier {
	fn peek_from(input: &Input) -> bool {
		matches!(input.front(), Some(TokenTree::Ident(ident)) if !ident.to_string().starts_with("r#"))
	}
}

impl PopFrom for Identifier {
	fn pop_from(input: &mut Input, errors: &mut Errors) -> Result<Self, ()> {
		input
			.pop_or_replace(|tts, _| match tts {
				[TokenTree::Ident(ident)] if !ident.to_string().starts_with("r#") => {
					Ok(Self(ident))
				}
				other => Err(other),
			})
			.map_err(|spans| {
				errors.push(Error::new(
					ErrorPriority::TOKEN,
					"Expected plain identifier.",
					spans,
				))
			})
	}
}
