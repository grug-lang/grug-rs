//! Defines the types needed represent the ast of a grug file
//!
//! All the types defined here are layout compatible with the corresponding
//! types defined in `grug.h`.
//!
//!	Backends recieve a [`GrugAst`] through a call to [`Backend::insert_file`](crate::backend::Backend::insert_file).
//!
//! The current api assumes the ast structs do not own the memory they are
//! allocated in. The gruggers crate allocates these in an arena and
//! deallocates them automatically after the call to [`Backend::insert_file`](crate::backend::Backend::insert_file).
//! This may be changed in a later release
use crate::ntstring::NTStrPtr;
use crate::types::GameFnPtr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C, u32)]
pub enum GrugType<'a> {
	Void = 0,
	Bool,
	Number,
	String,
	Id{custom_name: Option<NTStrPtr<'a>>},
	Resource{extension: NTStrPtr<'a>},
	Entity{entity_type: Option<NTStrPtr<'a>>},
}

const _: () = const {
	struct Test {
		_value: u32,
		_data: *const u8,
	}

	assert!(std::mem::size_of::<Test>() == std::mem::size_of::<GrugType>())
};

impl<'a> std::fmt::Display for GrugType<'a> {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::Void => write!(f, "void"),
			Self::Bool => write!(f, "bool"),
			Self::Number => write!(f, "number"),
			Self::String => write!(f, "string"),
			Self::Id{
				custom_name: None,
			} => write!(f, "id"),
			Self::Id{
				custom_name: Some(custom_name),
			} => write!(f, "{}", custom_name),
			Self::Resource {
				extension: _,
			} => write!(f, "resource"),
			Self::Entity {
				entity_type: Some(name),
			} => write!(f, "{}", name),
			Self::Entity {
				entity_type: None,
			} => write!(f, "entity"),
		}
	}
}

impl<'a> GrugType<'a> {
	pub fn match_non_exact(&self, other: &Self) -> bool {
		use GrugType::*;
		match (self, other) {
			(Void, Void) => true,
			(Bool, Bool) => true,
			(Number, Number) => true,
			(String, String) => true,
			(Id{custom_name: custom_name_1}, Id{custom_name: custom_name_2}) => custom_name_1 == custom_name_2 || custom_name_1.is_none() || custom_name_2.is_none(),
			(
				Resource {
					extension: extension_1,
				}, 
				Resource {
					extension: extension_2,
				}, 
			) => extension_1 == extension_2 || extension_1.is_empty() || extension_2.is_empty(),
			(
				Entity {
					entity_type: ty_1,
				}, 
				Entity {
					entity_type: ty_2,
				}, 
			) => ty_1 == ty_2 || ty_1.is_none() || ty_2.is_none(),
			_ => false,
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum UnaryOperator {
	Not = 0,
	Minus, 
}

impl std::fmt::Display for UnaryOperator {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::Not => write!(f, "NOT_TOKEN"),
			Self::Minus => write!(f, "MINUS_TOKEN"),
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum BinaryOperator {
	Or = 0,
	And, 
	DoubleEquals,
	NotEquals,
	Greater,
	GreaterEquals,
	Less,
	LessEquals,
	Plus,
	Minus,
	Multiply,
	Division,
	Remainder,
}

impl std::fmt::Display for BinaryOperator {
	fn fmt (&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
		match self {
			Self::Or => write!(f, "OR_TOKEN"),
			Self::And => write!(f, "AND_TOKEN"),
			Self::DoubleEquals => write!(f, "EQUALS_TOKEN"),
			Self::NotEquals => write!(f, "NOT_EQUALS_TOKEN"),
			Self::Greater => write!(f, "GREATER_TOKEN"),
			Self::GreaterEquals => write!(f, "GREATER_OR_EQUAL_TOKEN"),
			Self::Less => write!(f, "LESS_TOKEN"),
			Self::LessEquals => write!(f, "LESS_OR_EQUAL_TOKEN"),
			Self::Plus => write!(f, "PLUS_TOKEN"),
			Self::Minus => write!(f, "MINUS_TOKEN"),
			Self::Multiply => write!(f, "MULTIPLICATION_TOKEN"),
			Self::Division => write!(f, "DIVISION_TOKEN"),
			Self::Remainder => write!(f, "REMAINDER_TOKEN"),
		}
	}
}

#[derive(Debug)]
#[repr(C, u32)]
pub enum ExprData<'a> {
	True,
	False,
	String(NTStrPtr<'a>),
	Resource(NTStrPtr<'a>),
	Entity(NTStrPtr<'a>),
	Identifier(NTStrPtr<'a>),
	Number(f64, NTStrPtr<'a>),
	Unary {
		op   : UnaryOperator,
		expr : &'a mut Expr<'a>,
	},
	Binary {
		op    : BinaryOperator,
		left  : &'a mut Expr<'a>,
		right : &'a mut Expr<'a>,
	},
	Call {
		name : NTStrPtr<'a>,
		args : &'a mut [Expr<'a>],
		ptr  : Option<GameFnPtr>,
	},
	Parenthesized(&'a mut Expr<'a>),
}
const _: () = const {assert!(std::mem::size_of::<Option<GameFnPtr>>() == std::mem::size_of::<GameFnPtr>())};

#[derive(Debug)]
#[repr(C)]
pub struct Expr<'a> {
	pub result_type : Option<&'a GrugType<'a>>,
	pub data        : ExprData<'a>,
}

/// Represents a single member variable declaration within a file
///
/// ```
/// x: number = 25
/// ```
///
/// Member variables must always define a type and an intializer.
///
/// The initializer of a member function cannot call any on functions or helper
/// fuctions. It is allowed to call game functions.
#[repr(C)]
#[derive(Debug)]
pub struct MemberVariable<'a> {
	/// Name of the variable
	pub name: NTStrPtr<'a>,
	/// Type of the variable
	pub ty  : GrugType<'a>,
	/// Initializer of the variable.
	/// It is not allowed to call an on function or helper function.
	pub assignment_expr: Expr<'a>,
}

/// Represents a statement within a function
#[derive(Debug)]
#[repr(C, u32)]
pub enum Statement<'a> {
	/// A variable declaration or a variable assignment.
	///
	/// If the type is provided, it is a declaration. 
	///
	/// ```text
	/// x: number = 25 # declaration
	/// x = 30 # assignment
	/// ``` 
	///
	/// A declaration indicates that there is no other variable with the same
	/// name accessible from the current scope.
	///
	/// An assignment indicates that there is definitely an existing variable
	/// with that name accessible from the current scope. The existing variable
	/// may either be a member variable or a local variable.
	Variable {
		/// Name of the variable
		name            : NTStrPtr<'a>,
		/// Type of the variable if the statement is a declaration
		ty              : Option<&'a GrugType<'a>>,
		/// Expression to assign to the variable
		assignment_expr : Expr<'a>,
	},
	/// A statement that only consists of a single function call.
	///
	/// ```
	/// my_position: number = 0
	/// on_tick() {
	/// 	my_position = my_position + 2
	/// 	set_position(my_position) # Call statement
	/// }
	/// ```
	///
	/// The [`Expr`] within this variant is guaranteed to be a [call expression](ExprData::Call)
	Call(Expr<'a>),
	/// An if statement
	///
	/// ```
	/// helper_fib(n: number) number {
	/// 	if n <= 0 {
	/// 		return 0
	/// 	} else if n <= 2 {
	/// 		return 1
	/// 	} else {
	/// 		...
	/// 	}
	/// }
	/// ```
	///
	/// Chained else if statements are represented as a nested if block within
	/// the else block with the `is_chained` field set to true. The else block
	/// contains a single If statement in that case. 
	If {
		/// The condition expression of the if block. The result_type of the
		/// expression must be a boolean
		condition: Expr<'a>,
		/// Indicates whether the statement is chained or not
		is_chained: bool,
		/// The statements within the if block
		if_block: &'a mut [Statement<'a>],
		/// The statements within the else block if it exists.
		/// If the `is_chained` field is true, the list contains a single If statement
		else_block: &'a mut [Statement<'a>],
	},
	/// A while loop
	///
	/// ```
	/// helper_fib(n: number) number {
	/// 	result: number = 0
	/// 	if number < 0 {
	/// 		result = 0
	/// 	} else if number == 1 {
	/// 		result = 1
	/// 	} else {
	/// 		a: number = 1
	/// 		b: number = 1
	/// 		i: number = 2
	/// 		while i < number {
	/// 			temp: number = a + b
	/// 			a = b
	/// 			b = temp
	/// 			i = i + 1
	/// 		}
	/// 		result = b
	/// 	}
	/// 	return result
	/// }
	/// ```
	///
	/// While loops are the only loop construct available in grug (except for recursion)
	While {
		/// The condition expression of the while block. The result_type of the
		/// expression must be a boolean
		condition: Expr<'a>,
		/// The list of statements within the while block
		block: &'a mut [Statement<'a>],
	},
	/// Returns a value from the current function
	/// ```
	/// result: number = 25
	/// return result
	/// ```
	Return {
		expr: Option<&'a mut Expr<'a>>,
	},
	/// A comment within a function
	Comment(NTStrPtr<'a>),
	/// A break statement.
	///
	/// ```
	/// helper_foo() number {
	///		i: number = 0
	///		while i < 30000 {
	///			if helper_bar(i) {
	///				break
	///			}
	///			game_fn_foo(i)
	///			i = i + 1
	///		} 
	/// }
	/// ```
	///
	/// This statement can only occur within a while loop
	Break,
	/// A continue statement.
	///
	/// ```
	/// helper_foo() number {
	///		i: number = 0
	///		while i < 30000 {
	///			if helper_bar(i) {
	///				continue
	///			}
	///			game_fn_foo(i)
	///			i = i + 1
	///		} 
	/// }
	/// ```
	///
	/// This statement can only occur within a while loop
	Continue,
	/// An empty line within a function.
	/// 
	/// It is a compile error to have multiple empty lines in a row
	EmptyLine,
}

// TODO: This should be renamed to Parameter
/// Represents the name and type of a function parameter
/// ```text
/// helper_color(n: number) Color {
///              ^^^^^^^^^ - Parameter
/// ```
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Argument<'a> {
	/// Name of the parameter
	/// `n` is the name in the example
	pub name: NTStrPtr<'a>,
	/// Type of the parameter
	/// `number` is the type in the example
	pub ty  : GrugType<'a>,
}

/// Represents a single on function declaration
///
/// ```
/// on_init(id: number) {
///     set_max_health(50)
///     set_unarmed_damage(2)
///     set_weapon("sword.json")
/// }
/// ```
/// 
/// On functions need to be defined after all member variables and before all helper functions
#[repr(C)]
#[derive(Debug)]
pub struct OnFunction<'a> {
	/// Name of the function as a [null terminated string](crate::ntstring::NTStrPtr)
	pub name: NTStrPtr<'a>,
	/// List of parameters to the function and their types 
	pub arguments: &'a [Argument<'a>],
	/// List of statements that make up the top level of the function. 
	pub body_statements: &'a mut [Statement<'a>],
}

/// Represents a single helper function declaration
///
/// ```
/// helper_color(n: number) Color {
///     if n == 0 {
///         return color("blue")
///     } else if n == 1 {
///         return color("red")
///     } else if n == 2 {
///         return color("green")
///     } else if n == 3 {
///         return color("yellow")
///     } else if n == 3 {
///         return color("black")
///     } 
///     return game_fn_error("invalid color id")
/// }
/// ```
///
/// Helper functions need to be defined after all member variables and all on
/// functions
#[repr(C)]
#[derive(Debug)]
pub struct HelperFunction<'a> {
	/// Name of the function as a [null terminated string](crate::ntstring::NTStrPtr)
	pub name: NTStrPtr<'a>,
	/// Return type of the function. 
	///
	/// Return type is [`GrugType::Void`] if there is no return type
	pub return_type: GrugType<'a>,
	/// List of parameters to the function and their types 
	pub arguments: &'a [Argument<'a>],
	/// List of statements that make up the top level of the function. 
	pub body_statements: &'a mut [Statement<'a>],
}

// TODO: This should not be defined here, it should be defined within gruggers
/// A top level statement in a grug file.
///
/// This is not passed through [`GrugAst`] but is instead supposed to be used
/// internally by a grug state implementation
#[derive(Debug)]
pub enum GlobalStatement<'a> {
	/// A member variable
	/// `x: number = 25`
	Variable(MemberVariable<'a>),
	/// An on function declaration
	/// ```
	/// on_init(id: number) {
	///     set_max_health(50)
	///     set_unarmed_damage(2)
	///     set_weapon("sword.json")
	/// }
	/// ```
	OnFunction(OnFunction<'a>),
	/// A helper function declaration
	/// ```
	/// helper_color(n: number) Color {
	///     if n == 0 {
	///         return color("blue")
	///     } else if n == 1 {
	///         return color("red")
	///     } else if n == 2 {
	///         return color("green")
	///     } else if n == 3 {
	///         return color("yellow")
	///     } else if n == 3 {
	///         return color("black")
	///     } 
	///     return game_fn_error("invalid color id")
	/// }
	/// ```
	HelperFunction(HelperFunction<'a>),
	/// A comment at the top level of a file
	/// ```
	/// ## This is a global comment
	/// shared_number: number = 0
	/// ```
	Comment{
		value: NTStrPtr<'a>,
	},
	/// An Empty line at the top level of a script
	EmptyLine,
}

// TODO: All the references here should be mut references
/// A full representation of the ast of a grug file
#[repr(C)]
#[derive(Debug)]
pub struct GrugAst<'a> {
	/// Represents the member functions declared at the top of the functions
	///
	/// ```
	/// x: number = 25
	/// ```
	///
	/// These variable declarations must define both a type and a initializer
	pub members: &'a [MemberVariable<'a>],
	/// Represents all the on function declarations in the file
	///
	/// ```
	/// on_init(id: number) {
	///     set_max_health(50)
	///     set_unarmed_damage(2)
	///     set_weapon("sword.json")
	/// }
	/// ```
	///
	/// This array contains an entry for every on function defined in the
	/// mod_api for the current entity in the order defined in the mod_api even
	/// if it is not present in the file.
	///
	/// If an on function is not present in the file, that entry will be `None`
	/// (or a null pointer on the c side)
	pub on_functions: &'a [Option<&'a OnFunction<'a>>],
	/// Represents all the helper function declarations in the file
	///
	/// ```
	/// helper_color(n: number) Color {
	///     if n == 0 {
	///         return color("blue")
	///     } else if n == 1 {
	///         return color("red")
	///     } else if n == 2 {
	///         return color("green")
	///     } else if n == 3 {
	///         return color("yellow")
	///     } else if n == 3 {
	///         return color("black")
	///     } 
	///     return game_fn_error("invalid color id")
	/// }
	/// ```
	pub helper_functions: &'a [HelperFunction<'a>],
}

const _: () = const{
	// The C interop defined above assumes that slice pointers have a layout like this
	// struct Slice<T> {
	// 		data: NonNull<T>,
	// 		len : usize,
	// }
	// 
	// The rust compiler currently does not guarantee the layout of slice pointer.
	// These assertions ensure that if the assumption is broken, we get a
	// compile error instead of random crashes
	let x: &[MemberVariable] = &[];
	unsafe{assert!(x.len() == (&x as *const _ as *const usize).add(1).read());}
	let x: &[OnFunction] = &[];
	unsafe{assert!(x.len() == (&x as *const _ as *const usize).add(1).read());}
	let x: &[HelperFunction] = &[];
	unsafe{assert!(x.len() == (&x as *const _ as *const usize).add(1).read());}
	let x: &[Argument] = &[];
	unsafe{assert!(x.len() == (&x as *const _ as *const usize).add(1).read());}
	let x: &[Statement] = &[];
	unsafe{assert!(x.len() == (&x as *const _ as *const usize).add(1).read());}
};
