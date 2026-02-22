use crate::ntstring::NTStrPtr;

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
	pub(crate) fn match_non_exact(&self, other: &Self) -> bool {
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
	},
	Parenthesized(&'a mut Expr<'a>),
}

#[derive(Debug)]
#[repr(C)]
pub struct Expr<'a> {
	pub result_type : Option<&'a GrugType<'a>>,
	pub data        : ExprData<'a>,
}

#[repr(C)]
#[derive(Debug)]
pub struct MemberVariable<'a> {
	pub name: NTStrPtr<'a>,
	pub ty  : GrugType<'a>,
	pub assignment_expr: Expr<'a>,
}

#[derive(Debug)]
#[repr(C, u32)]
pub enum Statement<'a> {
	Variable {
		name            : NTStrPtr<'a>,
		ty              : Option<&'a GrugType<'a>>,
		assignment_expr : Expr<'a>,
	},
	Call(Expr<'a>),
	If {
		condition: Expr<'a>,
		is_chained: bool,
		if_block: &'a mut [Statement<'a>],
		else_block: &'a mut [Statement<'a>],
	},
	While {
		condition: Expr<'a>,
		block: &'a mut [Statement<'a>],
	},
	Return {
		expr: Option<&'a mut Expr<'a>>,
	},
	Comment(NTStrPtr<'a>),
	Break,
	Continue,
	EmptyLine,
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Argument<'a> {
	pub name: NTStrPtr<'a>,
	pub ty  : GrugType<'a>,
}

#[repr(C)]
#[derive(Debug)]
pub struct OnFunction<'a> {
	pub name: NTStrPtr<'a>,
	pub arguments: &'a [Argument<'a>],
	pub body_statements: &'a mut [Statement<'a>],
}

#[repr(C)]
#[derive(Debug)]
pub struct HelperFunction<'a> {
	pub name: NTStrPtr<'a>,
	pub return_type: GrugType<'a>,
	pub arguments: &'a [Argument<'a>],
	pub body_statements: &'a mut [Statement<'a>],
}

#[repr(C)]
#[derive(Debug)]
pub struct GrugAst<'a> {
	pub members: &'a [MemberVariable<'a>],
	pub on_functions: &'a [Option<&'a OnFunction<'a>>],
	pub helper_functions: &'a [HelperFunction<'a>],
}

#[derive(Debug)]
pub enum GlobalStatement<'a> {
	Variable(MemberVariable<'a>),
	OnFunction(OnFunction<'a>),
	HelperFunction(HelperFunction<'a>),
	Comment{
		value: NTStrPtr<'a>,
	},
	EmptyLine,
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
