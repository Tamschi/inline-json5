#[macro_export]
macro_rules! json5 {
    // Pass `$crate` to the procedural macro in a way that doesn't require parsing it.
    ($($tt:tt)*) => ( $crate::__::json5!([$crate::__] $($tt)*) );
}

#[doc(hidden)]
pub mod __ {
	pub use core; // Expected by `Errors`.
	pub use inline_json5_macro::json5;
	pub use json;
	pub use std; // Reexporting the entire crate makes it easier to update the macro without the wrapper.
}
