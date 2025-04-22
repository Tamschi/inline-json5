// This is the example from <https://json5.org/>, roughly.

use std::f64::{INFINITY, NEG_INFINITY};

use inline_json5::json5;
use json::{JsonValue, number::NAN};

#[test]
pub fn example() {
	// Edit this to compare how each macro reacts to invalid input.

	let json5 = json5!({
		// comments
		unquoted: "and you can quote me on that",
		singleQuotes: "I can use \"double quotes\" here",
		lineBreaks: "Look, Mom! \
	No \\n's!",
		hexadecimal: 0xdecaf,
		leadingDecimalPoint: .8675309, andTrailing: 8675309.,
		positiveSign: +1,
		nan: NaN,
		infinity: infinity,
		negative_infinity: -infinity,
		json_value: (JsonValue::Null),
		trailingComma: "in objects", andIn: ["arrays",],
		"backwardsCompatible": "with JSON",
	});

	let object = json::object! {
		unquoted: "and you can quote me on that",
		singleQuotes: "I can use \"double quotes\" here",
		lineBreaks: "Look, Mom! \
	No \\n's!",
		hexadecimal: 0xdecaf,
		leadingDecimalPoint: 0.8675309, andTrailing: 8675309.,
		positiveSign: 1,
		nan: NAN,
		infinity: INFINITY,
		negative_infinity: NEG_INFINITY,
		json_value: JsonValue::Null,
		trailingComma: "in objects", andIn: ["arrays",],
		"backwardsCompatible": "with JSON",
	};

	assert_eq!(json5, object);
}
