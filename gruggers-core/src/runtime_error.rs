#[derive(Debug, Clone, Copy)]
#[repr(u32)]
pub enum RuntimeError {
	ExceededTimeLimit,
	StackOverflow,
	// GameFunctionError,
	GameFunctionError{
		message: &'static str,
	},
}

impl RuntimeError {
	pub fn code(self) -> u32 {
		match self {
			Self::StackOverflow         => 0,
			Self::ExceededTimeLimit     => 1,
			Self::GameFunctionError{..} => 2,
		}
	}
}

pub const ON_FN_TIME_LIMIT: u64 = 100; // ms
// pub const ON_FN_TIME_LIMIT: u64 = 2000000; // ms

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
