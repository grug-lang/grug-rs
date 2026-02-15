pub mod capi {
	#![allow(non_camel_case_types)]
	use crate::ntstring::NTStrPtr;

	use std::mem::MaybeUninit;

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct grug_type_enum(pub u32);
	impl grug_type_enum {
		pub const VOID     : Self = Self(0);
		pub const BOOL     : Self = Self(1);
		pub const NUMBER   : Self = Self(2);
		pub const STRING   : Self = Self(3);
		pub const ID       : Self = Self(4);
		pub const RESOURCE : Self = Self(5);
		pub const ENTITY   : Self = Self(6);
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct grug_type<'a> {
		pub ty: grug_type_enum,
		// optionally used if type is grug_type_enum::ID
		// used if type is grug_type_enum::RESOURCE
		// optionally used if type is grug_type_enum::ENTITY
		pub extra_data: Option<NTStrPtr<'a>>,
	}

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct unary_operator(pub u32);
	impl unary_operator {
		pub const NOT   : Self = Self(0);
		pub const MINUS : Self = Self(1);
	}

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct binary_operator(pub u32);
	impl binary_operator {
		pub const OR            : Self = Self(0 );
		pub const AND           : Self = Self(1 );
		pub const DOUBLEEQUALS  : Self = Self(2 );
		pub const NOTEQUALS     : Self = Self(3 );
		pub const GREATER       : Self = Self(4 );
		pub const GREATEREQUALS : Self = Self(5 );
		pub const LESS          : Self = Self(6 );
		pub const LESSEQUALS    : Self = Self(7 );
		pub const PLUS          : Self = Self(8 );
		pub const MINUS         : Self = Self(9 );
		pub const MULTIPLY      : Self = Self(10);
		pub const DIVISION      : Self = Self(11);
		pub const REMAINDER     : Self = Self(12);
	}

	#[repr(transparent)]
	#[derive(Clone, Copy)]
	pub struct expr_type(u32);
	impl expr_type {
		pub const TRUE          : Self = Self(0 );
		pub const FALSE         : Self = Self(1 );
		pub const STRING        : Self = Self(2 );
		pub const RESOURCE      : Self = Self(3 );
		pub const ENTITY        : Self = Self(4 );
		pub const IDENTIFIER    : Self = Self(5 );
		pub const NUMBER        : Self = Self(6 );
		pub const UNARY         : Self = Self(7 );
		pub const BINARY        : Self = Self(8 );
		pub const CALL          : Self = Self(9 );
		pub const PARENTHESIZED : Self = Self(10);
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct unary_op_data<'a> {
		op   : unary_operator,
		inner: &'a expr<'a>,
	}
	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct binary_op_data<'a> {
		op   : binary_operator,
		left : &'a expr<'a>,
		right: &'a expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct call_data<'a> {
		function_name : NTStrPtr<'a>,
		args          : *const expr<'a>,
		args_count    : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub union expr_data<'a> {
		string          : NTStrPtr<'a>,
		resource        : NTStrPtr<'a>,
		entity          : NTStrPtr<'a>,
		identifier_name : NTStrPtr<'a>,
		number          : f64,
		unary           : unary_op_data<'a>,
		binary          : binary_op_data<'a>,
		call            : call_data<'a>,
		parenthesized   : &'a expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct expr<'a> {
		result_type : grug_type<'a>,
		expr_data   : expr_data<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct member_variable<'a> {
		name            : NTStrPtr<'a>,
		ty              : grug_type<'a>,
		assignment_expr : expr<'a>,
	}

	#[repr(transparent)]
	#[derive(Clone, Copy)]
	pub struct statement_type(u32);
	impl statement_type {
		const VARIABLE : Self = Self(0);
		const CALL     : Self = Self(1);
		const IF       : Self = Self(2);
		const WHILE    : Self = Self(3);
		const COMMENT  : Self = Self(4);
		const BREAK    : Self = Self(5);
		const CONTINUE : Self = Self(6);
		const EMPTY    : Self = Self(7);
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct local_variable_data<'a> {
		name            : NTStrPtr<'a>,
		has_type        : u8, /* bool */
		actual_type     : MaybeUninit<grug_type<'a>>,
		assignment_expr : expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct if_stmt_data<'a> {
		condition      : expr<'a>,
		chained        : u8 /* bool */,
		if_block       : *const statement<'a>,
		if_block_len   : usize,
		else_block     : *const statement<'a>,
		else_block_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct while_stmt_data<'a> {
		condition : expr<'a>,
		block     : *const statement<'a>,
		block_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct return_stmt_data<'a> {
		has_value : u8 /* bool */,
		expr      : MaybeUninit<expr<'a>>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub union statement_data<'a> {
		variable    : local_variable_data<'a>,
		call        : expr<'a>,
		if_stmt     : if_stmt_data<'a>,
		while_stmt  : while_stmt_data<'a>,
		return_stmt : return_stmt_data<'a>,
		comment     : NTStrPtr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct statement<'a> {
		ty  : statement_type,
		data: statement_data<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct argument<'a> {
		name : NTStrPtr<'a>,
		ty   : grug_type<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct on_function<'a> {
		name                : NTStrPtr<'a>,
		arguments           : *const argument<'a>,
		arguments_len       : usize,
		body_statements     : *const statement<'a>,
		body_statements_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct helper_function<'a> {
		name                : NTStrPtr<'a>,
		return_type         : grug_type<'a>,
		arguments           : *const argument<'a>,
		arguments_len       : usize,
		body_statements     : *const statement<'a>,
		body_statements_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct grug_ast<'a> {
		members                : *const member_variable<'a>,
		members_count          : usize,

		on_functions           : *const on_function<'a>,
		on_functions_count     : usize,

		helper_functions       : *const helper_function<'a>,
		helper_functions_count : usize,
	}
}
use capi::*;

pub mod rust_api {
	use super::*;
	use crate::ntstring::{NTStr, NTStrPtr};

	pub enum GrugType<'a> {
		Void,
		Bool,
		Number,
		String,
		ID{custom_name: Option<&'a NTStr>},
		Resource{extension: &'a NTStr},
		Entity{entity_type: Option<&'a NTStr>},
	}

	impl<'a> From<GrugType<'a>> for grug_type<'a> {
		fn from (other: GrugType<'a>) -> Self {
			let (discriminant, data) = match other {
				GrugType::Void => (grug_type_enum::VOID, None),
				GrugType::Bool => (grug_type_enum::BOOL, None),
				GrugType::Number => (grug_type_enum::NUMBER, None),
				GrugType::String => (grug_type_enum::STRING, None),
				GrugType::ID{custom_name} => (grug_type_enum::ID, custom_name),
				GrugType::Resource{extension} => (grug_type_enum::RESOURCE, Some(extension)),
				GrugType::Entity{entity_type} => (grug_type_enum::ENTITY, entity_type),
			};
			Self {
				ty: discriminant,
				extra_data: data.map(NTStr::as_ntstrptr),
			}
		}
	}

	impl<'a> From<grug_type<'a>> for GrugType<'a> {
		fn from (other: grug_type<'a>) -> Self {
			match other.ty {
				grug_type_enum::VOID     => Self::Void,
				grug_type_enum::BOOL     => Self::Bool,
				grug_type_enum::NUMBER   => Self::Number,
				grug_type_enum::STRING   => Self::String,
				grug_type_enum::ID       => Self::ID {custom_name: other.extra_data.map(NTStrPtr::to_ntstr)},
				grug_type_enum::RESOURCE => Self::Resource {extension: other.extra_data.expect("Resource extension must be non null").to_ntstr()},
				grug_type_enum::ENTITY   => Self::Entity {entity_type: other.extra_data.map(NTStrPtr::to_ntstr)},
				_                        => panic!("unexpected grug_type variant: {}", other.ty.0),
			}
		}
	}

	pub enum UnaryOperator {
		Not = 0,
		Minus, 
	}

	impl From<UnaryOperator> for unary_operator {
		fn from (other: UnaryOperator) -> Self {
			match other {
				UnaryOperator::Not   => Self::NOT,
				UnaryOperator::Minus => Self::MINUS,
			}
		}
	}

	impl From<unary_operator> for UnaryOperator {
		fn from (other: unary_operator) -> Self {
			match other {
				unary_operator::NOT   => Self::Not,
				unary_operator::MINUS => Self::Minus,
				_                     => panic!("unexpected unary_operator variant: {}", other.0),
			}
		}
	}

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

	impl From<BinaryOperator> for binary_operator {
		fn from (other: BinaryOperator) -> Self {
			match other {
				BinaryOperator::Or            => Self::OR,
				BinaryOperator::And           => Self::AND,
				BinaryOperator::DoubleEquals  => Self::DOUBLEEQUALS,
				BinaryOperator::NotEquals     => Self::NOTEQUALS,
				BinaryOperator::Greater       => Self::GREATER,
				BinaryOperator::GreaterEquals => Self::GREATEREQUALS,
				BinaryOperator::Less          => Self::LESS,
				BinaryOperator::LessEquals    => Self::LESSEQUALS,
				BinaryOperator::Plus          => Self::PLUS,
				BinaryOperator::Minus         => Self::MINUS,
				BinaryOperator::Multiply      => Self::MULTIPLY,
				BinaryOperator::Division      => Self::DIVISION,
				BinaryOperator::Remainder     => Self::REMAINDER,
			}
		}
	}

	impl From<binary_operator> for BinaryOperator {
		fn from (other: binary_operator) -> Self {
			match other {
				binary_operator::OR            => Self::Or,
				binary_operator::AND           => Self::And, 
				binary_operator::DOUBLEEQUALS  => Self::DoubleEquals,
				binary_operator::NOTEQUALS     => Self::NotEquals,
				binary_operator::GREATER       => Self::Greater,
				binary_operator::GREATEREQUALS => Self::GreaterEquals,
				binary_operator::LESS          => Self::Less,
				binary_operator::LESSEQUALS    => Self::LessEquals,
				binary_operator::PLUS          => Self::Plus,
				binary_operator::MINUS         => Self::Minus,
				binary_operator::MULTIPLY      => Self::Multiply,
				binary_operator::DIVISION      => Self::Division,
				binary_operator::REMAINDER     => Self::Remainder,
				_                              => panic!("unexpected binary_operator variant: {}", other.0),
			}
		}
	}
}


