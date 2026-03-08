/// Enum that represents all possible runtime errors
#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum RuntimeError {
	/// Execution of a grug_script takes longer than allowed.
	ExceededTimeLimit = 0,
	/// Indicates potentially unbounded recursion
	StackOverflow,
	/// A game function called the `set_runtime_error` function on the state
	/// with the given `message`
	GameFunctionError{
		message: &'static str,
	},
}

impl RuntimeError {
	/// Return the code defined by grug.h for a runtime error kind
	pub fn code(self) -> u32 {
		match self {
			Self::StackOverflow         => 0,
			Self::ExceededTimeLimit     => 1,
			Self::GameFunctionError{..} => 2,
		}
	}
}

/// This is the maximum time allowed to execute an on function.
/// Backends are allowed to take longer than this time to throw an error.
pub const ON_FN_TIME_LIMIT: u64 = 100; // ms
// pub const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

/// This is the maximum allowed depth of function calls when executing an on function.
/// Backends are allowed to go further than this limit because of optimizations.
pub const MAX_RECURSION_LIMIT: usize = 100;

impl std::fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::ExceededTimeLimit => write!(f, "Took longer than {} milliseconds to run", ON_FN_TIME_LIMIT),
			Self::StackOverflow => write!(f, "Stack overflow, so check for accidental infinite recursion"),
			Self::GameFunctionError{message} => write!(f, "{}", message),
		}
	}
}
