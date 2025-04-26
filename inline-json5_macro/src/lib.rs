use loess::{
	Errors, Input, IntoTokens, PeekFrom, SimpleSpanned, grammar, parse_all, parse_once,
	quote_into_mixed_site,
};
use proc_macro2::{Delimiter, Group, Literal, Span, TokenStream, TokenTree};

mod tokens;
use tokens::{
	Braces, Colon, Comma, Delimited, False, Identifier, Infinity, Minus, NaN, Null, NumberLiteral,
	Parentheses, Plus, SquareBrackets, String, True,
};

#[proc_macro]
pub fn json5(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	hook_panics();
	json5_(input.into()).into()
}

// Having this as separate function makes type inference a bit easier.
fn json5_(input: TokenStream) -> TokenStream {
	let mut input = Input {
		tokens: input.into_iter().collect(),
		end: Span::call_site(), // Use `Span::end` of the last input token instead once that's stable!
	};
	let mut errors = Errors::new();

	let Ok(SquareBrackets { contents: root, .. }) = parse_once(&mut input, &mut errors) else {
		return errors.collect_tokens(&TokenStream::new());
	};

	// `parse_all` checks that the input was fully consumed when the iterator is dropped.
	let value = parse_all::<Value>(&mut input, &mut errors).next();

	// Since we have a (non-empty) `root`, this is *fully* hygienic.
	// Otherwise, it would expect `::core` to be the standard library `core` crate.
	let mut output = errors.collect_tokens(&root);

	if let Some(value) = value {
		value.into_tokens(&root, &mut output)
	} else {
		quote_into_mixed_site!(Span::mixed_site(), &root, &mut output, {
			{#error { "Expected JSON5 value." }}
			{#root}::json::JsonValue::Null
		})
	}

	// This is a value macro but those `compile_error!`s are statements,
	// so let's wrap the output in a block!
	TokenTree::Group(Group::new(Delimiter::Brace, output)).into()
}

// Note that `grammar!`'s `PeekFrom` implementations only check for the first field.
grammar! {
	enum Value: PopFrom, IntoTokens {
		String(String),
		Number(Number),
		Object(Object),
		Array(Array),
		True(True),
		False(False),
		Null(Null),
		InlineRust(InlineRust),
	} else "Expected JSON5 value."; // The error message that's used when no variant is peeked successfully.

	struct Array: PeekFrom, PopFrom (SquareBrackets<Delimited<Value, Comma>>);

	struct Object: PeekFrom, PopFrom (Braces<Delimited<Property, Comma>>);

	struct Property: PopFrom {
		key: Key,
		colon: Colon,
		value: Value,
	}

	enum Key: PopFrom {
		Identifier(Identifier),
		String(String),
	} else "Expected key (plain identifier or string literal).";

	enum Number: PeekFrom, PopFrom {
		NaN(NaN),
		NotNaN(NotNaN),
	} else "Expected Number."; // Should be unreachable.

	struct NotNaN: PopFrom {
		sign: Option<Sign>,
		amount: Amount,
	}

	enum Sign: PeekFrom, PopFrom {
		Plus(Plus),
		Minus(Minus),
	} else "Expected Sign."; // Should be unreachable.

	enum Amount: PeekFrom, PopFrom {
		Finite(NumberLiteral), // This could be a structured variant too, but `grammar!` doesn't support that yet.
		Infinity(Infinity),
	} else "Expected number literal or `infinity`.";

	struct InlineRust: PeekFrom, PopFrom (Parentheses);
}

impl Value {
	/// Uses for narrowing the input range in which property errors are reported.
	fn span(&self) -> Span {
		match self {
			Value::String(s) => s.0.span(),
			Value::Number(number) => match number {
				Number::NaN(nan) => nan.0.span(),
				Number::NotNaN(not_nan) => not_nan.amount.span(),
			},
			Value::Object(object) => object.0.span.join(),
			Value::Array(array) => array.0.span.join(),
			Value::True(t) => t.0.span(),
			Value::False(f) => f.0.span(),
			Value::Null(n) => n.0.span(),
			Value::InlineRust(inline_rust) => inline_rust.0.span.join(),
		}
	}
}

/// [`NotNaN`] starts with an [`Option`], which isn't peekable since that's error-prone in [`grammar!`].
///
/// As such, this is implemented manually. (Maybe I'll add a way to have this automatically,
/// but it's unlikely since that might have an impact on compile time that's disproportionate to the work done.)
impl PeekFrom for NotNaN {
	fn peek_from(input: &Input) -> bool {
		Sign::peek_from(input) || Amount::peek_from(input)
	}
}

// Alternatively, you could `{#match …, … }` in a template for `Value`, but here it's convenient to split the branches.

impl IntoTokens for Object {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		quote_into_mixed_site!(self.0.span.join(), root, tokens, {
			{#root}::json::JsonValue::Object({
				let mut object = {#root}::json::object::Object::with_capacity(
					{#(Literal::usize_unsuffixed(self.0.contents.0.len()))}
				);
				{#for (property, comma) in self.0.contents.0 {
					{#let Property { key, colon, value } = property;}
					{#located_at(value.span()) {
						object.insert(
							{#(key)}
							{#located_at(colon.0.span()) { , }}
							{#(value)}
						)
					}}
					{#located_at(comma.map(|comma| comma.0.span()).unwrap_or(self.0.span.close())) { ; }}
				}}
				object
			})
		})
	}
}

impl IntoTokens for Key {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		match self {
			Key::Identifier(identifier) => {
				let mut s = Literal::string(&identifier.0.to_string());
				s.set_span(identifier.0.span());
				s.into_tokens(root, tokens)
			}
			Key::String(s) => s.0.into_tokens(root, tokens),
		}
	}
}

impl IntoTokens for Array {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		quote_into_mixed_site!(self.0.span.join(), root, tokens, {
			{#root}::json::JsonValue::Array({
				let mut vec = {#root}::std::vec::Vec::with_capacity(
					{#(Literal::usize_unsuffixed(self.0.contents.0.len()))}
				);
				{#for (item, comma) in self.0.contents.0 {
					{#located_at(item.span()) {
						vec.push({#(item)});
					}}
					{#located_at(comma.map(|comma| comma.0.span()).unwrap_or(self.0.span.close())) { ; }}
				}}
				vec
			})
		})
	}
}

impl IntoTokens for Number {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		let span = match &self {
			Number::NaN(nan) => nan.0.span(),
			Number::NotNaN(NotNaN {
				sign: Some(sign),
				amount,
			}) => sign.span().join(amount.span()).unwrap_or(amount.span()),
			Number::NotNaN(NotNaN { sign: None, amount }) => amount.span(),
		};
		quote_into_mixed_site!(span, root, tokens, {
			{#root}::json::JsonValue::Number(
				{#match self {
					Self::NaN(_) => { {#root}::json::number::NAN }
					Self::NotNaN(NotNaN { sign, amount }) => {
						{#root}::core::convert::From::from(
							{#match (sign, amount) {
								(Some(Sign::Minus(_)), Amount::Infinity(_)) => {
									{#root}::core::f64::NEG_INFINITY
								}
								(_, Amount::Infinity(_)) => {
									{#root}::core::f64::INFINITY
								}
								(Some(Sign::Minus(minus)), amount) => {
									{#(minus)} {#(amount)}
								}
								(_, amount) => { {#(amount)} }
							}}
						)
					}
				}}
			)
		})
	}
}

impl SimpleSpanned for Sign {
	fn span(&self) -> Span {
		match self {
			Sign::Plus(plus) => &plus.0,
			Sign::Minus(minus) => &minus.0,
		}
		.span()
	}

	fn set_span(&mut self, span: Span) {
		match self {
			Sign::Plus(plus) => &mut plus.0,
			Sign::Minus(minus) => &mut minus.0,
		}
		.set_span(span)
	}
}

impl Amount {
	fn span(&self) -> Span {
		match self {
			Amount::Finite(NumberLiteral(dot, literal)) => dot
				.as_ref()
				.and_then(|dot| dot.0.span().join(literal.span()))
				.unwrap_or(literal.span()),
			Amount::Infinity(infinity) => infinity.0.span(),
		}
	}
}

impl IntoTokens for Amount {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		let Self::Finite(finite) = self else {
			unreachable!("Handled by Number.")
		};
		finite.into_tokens(root, tokens)
	}
}

impl IntoTokens for True {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		quote_into_mixed_site!(self.0.span(), root, tokens, {
			{#root}::json::JsonValue::Boolean(true)
		});
	}
}

impl IntoTokens for False {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		quote_into_mixed_site!(self.0.span(), root, tokens, {
			{#root}::json::JsonValue::Boolean(false)
		});
	}
}

impl IntoTokens for Null {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		quote_into_mixed_site!(self.0.span(), root, tokens, {
			{#root}::json::JsonValue::Null
		});
	}
}

impl IntoTokens for String {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		quote_into_mixed_site!(self.0.span(), root, tokens, {
			{#root}::json::JsonValue::String(
				{#(self.0)}.to_string()
			)
		});
	}
}

impl IntoTokens for InlineRust {
	fn into_tokens(self, root: &TokenStream, tokens: &mut impl Extend<TokenTree>) {
		// Unwrap from parentheses.
		self.0.contents.into_tokens(root, tokens)
	}
}

/// This is just a convenience to get the source location when a panic occurs.
fn hook_panics() {
	std::panic::set_hook(Box::new(|panic_info| {
		let location = panic_info.location();

		let payload = if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
			s
		} else if let Some(s) = panic_info.payload().downcast_ref::<::std::string::String>() {
			s.as_str()
		} else {
			"(unknown panic type)"
		};
		eprintln!(
			"proc macro panic at {}:{}\n\n{}",
			location.map(|l| l.file()).unwrap_or("None"),
			location
				.map(|l| l.line().to_string())
				.unwrap_or_else(|| "None".to_string()),
			payload
		);
	}))
}
