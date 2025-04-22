# `inline-json5`

This is a small crate to write [`JsonValue`](https://docs.rs/json/latest/json/enum.JsonValue.html)s in Rust using JSON5 syntax.

It supports interpolation in parentheses and has great error highlighting.
(The macro recovers at each `,` or closing delimiter and can report further errors afterwards.)

```rust
use inline_json5::json5;
use json::JsonValue;

// Example taken from <https://json5.org/>, modified.
let _ = json5!({
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
```

The macro is fully hygienic even though it is proc macro backed,
and it compiles still quite quickly since Loess is a lightweight macro framework.
