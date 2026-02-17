pub mod capi {
	#![allow(non_camel_case_types)]
	use crate::ntstring::NTStrPtr;

	use std::mem::MaybeUninit;

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct c_grug_type_enum(pub u32);
	impl c_grug_type_enum {
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
	pub struct c_grug_type<'a> {
		pub ty: c_grug_type_enum,
		// optionally used if type is grug_type_enum::ID
		// used if type is grug_type_enum::RESOURCE
		// optionally used if type is grug_type_enum::ENTITY
		pub extra_data: Option<NTStrPtr<'a>>,
	}

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct c_unary_operator(pub u32);
	impl c_unary_operator {
		pub const NOT   : Self = Self(0);
		pub const MINUS : Self = Self(1);
	}

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct c_binary_operator(pub u32);
	impl c_binary_operator {
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
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct c_expr_type(pub u32);
	impl c_expr_type {
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
	pub struct c_unary_op_data<'a> {
		pub op   : c_unary_operator,
		pub expr : &'a c_expr<'a>,
	}
	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_binary_op_data<'a> {
		pub op   : c_binary_operator,
		pub left : &'a c_expr<'a>,
		pub right: &'a c_expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_call_data<'a> {
		pub name       : NTStrPtr<'a>,
		pub args       : *const c_expr<'a>,
		pub args_count : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub union c_expr_data<'a> {
		pub bool          : (),
		pub string        : NTStrPtr<'a>,
		pub resource      : NTStrPtr<'a>,
		pub entity        : NTStrPtr<'a>,
		pub identifier    : NTStrPtr<'a>,
		pub number        : f64,
		pub unary         : c_unary_op_data<'a>,
		pub binary        : c_binary_op_data<'a>,
		pub call          : c_call_data<'a>,
		pub parenthesized : &'a c_expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_expr<'a> {
		pub result_filled : u8, /* bool */
		pub result_type   : MaybeUninit::<c_grug_type<'a>>,
		pub expr_type     : c_expr_type,
		pub expr_data     : c_expr_data<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_member_variable<'a> {
		name            : NTStrPtr<'a>,
		ty              : c_grug_type<'a>,
		assignment_expr : c_expr<'a>,
	}

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct c_statement_type(pub u32);
	impl c_statement_type {
		pub const VARIABLE  : Self = Self(0);
		pub const CALL      : Self = Self(1);
		pub const IF        : Self = Self(2);
		pub const WHILE     : Self = Self(3);
		pub const RETURN    : Self = Self(4);
		pub const COMMENT   : Self = Self(5);
		pub const BREAK     : Self = Self(6);
		pub const CONTINUE  : Self = Self(7);
		pub const EMPTYLINE : Self = Self(8);
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_local_variable_data<'a> {
		pub name            : NTStrPtr<'a>,
		pub has_type        : u8, /* bool */
		pub actual_type     : MaybeUninit<c_grug_type<'a>>,
		pub assignment_expr : c_expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_if_stmt_data<'a> {
		pub condition      : c_expr<'a>,
		pub is_chained     : u8 /* bool */,
		pub if_block       : *const c_statement<'a>,
		pub if_block_len   : usize,
		pub else_block     : *const c_statement<'a>,
		pub else_block_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_while_stmt_data<'a> {
		pub condition : c_expr<'a>,
		pub block     : *const c_statement<'a>,
		pub block_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_return_stmt_data<'a> {
		pub has_value : u8 /* bool */,
		pub expr      : MaybeUninit<c_expr<'a>>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub union c_statement_data<'a> {
		pub variable    : c_local_variable_data<'a>,
		pub call        : c_expr<'a>,
		pub if_stmt     : c_if_stmt_data<'a>,
		pub while_stmt  : c_while_stmt_data<'a>,
		pub return_stmt : c_return_stmt_data<'a>,
		pub comment     : NTStrPtr<'a>,
		pub empty       : (),
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_statement<'a> {
		pub ty  : c_statement_type,
		pub data: c_statement_data<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_argument<'a> {
		pub name : NTStrPtr<'a>,
		pub ty   : c_grug_type<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_on_function<'a> {
		pub name                : NTStrPtr<'a>,
		pub arguments           : *const c_argument<'a>,
		pub arguments_len       : usize,
		pub body_statements     : *const c_statement<'a>,
		pub body_statements_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_helper_function<'a> {
		pub name                : NTStrPtr<'a>,
		pub return_type         : c_grug_type<'a>,
		pub arguments           : *const c_argument<'a>,
		pub arguments_len       : usize,
		pub body_statements     : *const c_statement<'a>,
		pub body_statements_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct c_grug_ast<'a> {
		pub members                : *const c_member_variable<'a>,
		pub members_count          : usize,

		pub on_functions           : *const c_on_function<'a>,
		pub on_functions_count     : usize,

		pub helper_functions       : *const c_helper_function<'a>,
		pub helper_functions_count : usize,
	}
}
use capi::*;

pub mod rust_api {
	use super::*;
	use crate::ntstring::{NTStr, NTStrPtr};

	use std::mem::MaybeUninit;

	#[derive(Clone, Copy)]
	pub enum GrugType<'a> {
		Void,
		Bool,
		Number,
		String,
		ID{custom_name: Option<&'a NTStr>},
		Resource{extension: &'a NTStr},
		Entity{entity_type: Option<&'a NTStr>},
	}

	impl<'a> From<GrugType<'a>> for c_grug_type<'a> {
		fn from (other: GrugType<'a>) -> Self {
			let (discriminant, data) = match other {
				GrugType::Void => (c_grug_type_enum::VOID, None),
				GrugType::Bool => (c_grug_type_enum::BOOL, None),
				GrugType::Number => (c_grug_type_enum::NUMBER, None),
				GrugType::String => (c_grug_type_enum::STRING, None),
				GrugType::ID{custom_name} => (c_grug_type_enum::ID, custom_name),
				GrugType::Resource{extension} => (c_grug_type_enum::RESOURCE, Some(extension)),
				GrugType::Entity{entity_type} => (c_grug_type_enum::ENTITY, entity_type),
			};
			Self {
				ty: discriminant,
				extra_data: data.map(NTStr::as_ntstrptr),
			}
		}
	}

	impl<'a> From<c_grug_type<'a>> for GrugType<'a> {
		fn from (other: c_grug_type<'a>) -> Self {
			match other.ty {
				c_grug_type_enum::VOID     => Self::Void,
				c_grug_type_enum::BOOL     => Self::Bool,
				c_grug_type_enum::NUMBER   => Self::Number,
				c_grug_type_enum::STRING   => Self::String,
				c_grug_type_enum::ID       => Self::ID {custom_name: other.extra_data.map(NTStrPtr::to_ntstr)},
				c_grug_type_enum::RESOURCE => Self::Resource {extension: other.extra_data.expect("Resource extension must be non null").to_ntstr()},
				c_grug_type_enum::ENTITY   => Self::Entity {entity_type: other.extra_data.map(NTStrPtr::to_ntstr)},
				_                        => panic!("unexpected grug_type variant: {}", other.ty.0),
			}
		}
	}

	#[derive(Clone, Copy)]
	pub enum UnaryOperator {
		Not = 0,
		Minus, 
	}

	impl From<UnaryOperator> for c_unary_operator {
		fn from (other: UnaryOperator) -> Self {
			match other {
				UnaryOperator::Not   => Self::NOT,
				UnaryOperator::Minus => Self::MINUS,
			}
		}
	}

	impl From<c_unary_operator> for UnaryOperator {
		fn from (other: c_unary_operator) -> Self {
			match other {
				c_unary_operator::NOT   => Self::Not,
				c_unary_operator::MINUS => Self::Minus,
				_                     => panic!("unexpected unary_operator variant: {}", other.0),
			}
		}
	}

	#[derive(Clone, Copy)]
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

	impl From<BinaryOperator> for c_binary_operator {
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

	impl From<c_binary_operator> for BinaryOperator {
		fn from (other: c_binary_operator) -> Self {
			match other {
				c_binary_operator::OR            => Self::Or,
				c_binary_operator::AND           => Self::And, 
				c_binary_operator::DOUBLEEQUALS  => Self::DoubleEquals,
				c_binary_operator::NOTEQUALS     => Self::NotEquals,
				c_binary_operator::GREATER       => Self::Greater,
				c_binary_operator::GREATEREQUALS => Self::GreaterEquals,
				c_binary_operator::LESS          => Self::Less,
				c_binary_operator::LESSEQUALS    => Self::LessEquals,
				c_binary_operator::PLUS          => Self::Plus,
				c_binary_operator::MINUS         => Self::Minus,
				c_binary_operator::MULTIPLY      => Self::Multiply,
				c_binary_operator::DIVISION      => Self::Division,
				c_binary_operator::REMAINDER     => Self::Remainder,
				_                              => panic!("unexpected binary_operator variant: {}", other.0),
			}
		}
	}

	#[derive(Clone, Copy)]
	pub enum LiteralExprData<'a> {
		True,
		False,
		String(&'a NTStr),
		Resource(&'a NTStr),
		Entity(&'a NTStr),
		Identifier(&'a NTStr),
		Number(f64),
	}

	#[derive(Clone, Copy)]
	pub enum ExprData<'a> {
		Literal(LiteralExprData<'a>),
		Unary {
			op   : UnaryOperator,
			expr : &'a c_expr<'a>,
		},
		Binary {
			op    : BinaryOperator,
			left  : &'a c_expr<'a>,
			right : &'a c_expr<'a>,
		},
		Call {
			name : &'a NTStr,
			args : &'a [c_expr<'a>],
		},
		Parenthesized(&'a c_expr<'a>),
	}

	#[derive(Clone, Copy)]
	pub struct Expr<'a> {
		result_type : Option<GrugType<'a>>,
		data        : ExprData<'a>,
	}

	impl<'a> From<c_expr<'a>> for Expr<'a> {
		fn from(other: c_expr<'a>) -> Self {
			unsafe {
				let result_type = (other.result_filled != 0).then(|| other.result_type.assume_init().into());
				let data = match other.expr_type {
					c_expr_type::TRUE          => ExprData::Literal(LiteralExprData::True),
					c_expr_type::FALSE         => ExprData::Literal(LiteralExprData::False),
					c_expr_type::STRING        => ExprData::Literal(LiteralExprData::String(other.expr_data.string.to_ntstr())),
					c_expr_type::RESOURCE      => ExprData::Literal(LiteralExprData::Resource(other.expr_data.resource.to_ntstr())),
					c_expr_type::ENTITY        => ExprData::Literal(LiteralExprData::Entity(other.expr_data.entity.to_ntstr())),
					c_expr_type::IDENTIFIER    => ExprData::Literal(LiteralExprData::Identifier(other.expr_data.identifier.to_ntstr())),
					c_expr_type::NUMBER        => ExprData::Literal(LiteralExprData::Number(other.expr_data.number)),
					c_expr_type::UNARY         => ExprData::Unary{
						op   : other.expr_data.unary.op.into(), 
						expr : other.expr_data.unary.expr,
					},
					c_expr_type::BINARY        => ExprData::Binary{
						op    : other.expr_data.binary.op.into(), 
						left  : other.expr_data.binary.left,
						right : other.expr_data.binary.right
					},
					c_expr_type::CALL          => ExprData::Call{
						name : other.expr_data.call.name.to_ntstr(), 
						args : std::slice::from_raw_parts(other.expr_data.call.args, other.expr_data.call.args_count),
					},
					c_expr_type::PARENTHESIZED => ExprData::Parenthesized(other.expr_data.parenthesized),
					_                        => panic!("unexpected expression variant: {}", other.expr_type.0),
				};
				Expr {
					result_type,
					data,
				}
			}
		}
	}

	impl<'a> From<Expr<'a>> for c_expr<'a> {
		fn from(other: Expr<'a>) -> Self {
			let (result_filled, result_type) = other.result_type.map(|x| (1, MaybeUninit::new(x.into()))).unwrap_or((0, MaybeUninit::uninit()));
			let (ty, data) = match other.data {
				ExprData::Literal(literal) => {
					match literal {
						LiteralExprData::True                  => (c_expr_type::TRUE, c_expr_data{bool: ()}),
						LiteralExprData::False                 => (c_expr_type::FALSE, c_expr_data{bool: ()}),
						LiteralExprData::String(string)        => (c_expr_type::STRING, c_expr_data{string: string.as_ntstrptr()}),
						LiteralExprData::Resource(string)      => (c_expr_type::RESOURCE, c_expr_data{resource: string.as_ntstrptr()}),
						LiteralExprData::Entity(string)        => (c_expr_type::ENTITY, c_expr_data{entity: string.as_ntstrptr()}),
						LiteralExprData::Identifier(string)    => (c_expr_type::IDENTIFIER, c_expr_data{identifier: string.as_ntstrptr()}),
						LiteralExprData::Number(number)        => (c_expr_type::NUMBER, c_expr_data{number}),
					}
				}
				ExprData::Unary{op, expr}  => {
					(c_expr_type::UNARY, c_expr_data {
						unary: c_unary_op_data {
							op: op.into(),
							expr,
						}
					})
				}
				ExprData::Binary{op, left, right}  => {
					(c_expr_type::BINARY, c_expr_data {
						binary: c_binary_op_data {
							op: op.into(),
							left,
							right,
						}
					})
				}
				ExprData::Call{name, args}  => {
					(c_expr_type::CALL, c_expr_data {
						call: c_call_data {
							name       : name.as_ntstrptr(),
							args       : args.as_ptr(),
							args_count : args.len(),
						}
					})
				}
				ExprData::Parenthesized(expr) => {
					(c_expr_type::PARENTHESIZED, c_expr_data {
						parenthesized: expr,
					})
				}
			};
			Self {
				result_filled,
				result_type,
				expr_type: ty,
				expr_data: data,
			}
		}
	}

	#[derive(Clone, Copy)]
	pub enum Statement<'a> {
		Variable {
			name            : &'a NTStr,
			ty              : Option<GrugType<'a>>,
			assignment_expr : Expr<'a>,
		},
		Call(Expr<'a>),
		If {
			condition: Expr<'a>,
			is_chained: bool,
			if_block: &'a [c_statement<'a>],
			else_block: &'a [c_statement<'a>],
		},
		While {
			condition: Expr<'a>,
			block: &'a [c_statement<'a>],
		},
		Return {
			expr: Option<Expr<'a>>,
		},
		Comment(&'a NTStr),
		Break,
		Continue,
		EmptyLine,
	}

	impl<'a> From<c_statement<'a>> for Statement<'a> {
		fn from (other: c_statement<'a>) -> Self {
			unsafe {
				match other.ty {
					c_statement_type::VARIABLE  => {
						let c_local_variable_data{name, has_type, actual_type, assignment_expr} = other.data.variable;
						Statement::Variable {
							name: name.to_ntstr(),
							ty: (has_type != 0).then(|| actual_type.assume_init().into()),
							assignment_expr: assignment_expr.into(),
						}
					}
					c_statement_type::CALL      => {
						Statement::Call(other.data.call.into())
					}
					c_statement_type::IF        => {
						let c_if_stmt_data{condition, is_chained, if_block, if_block_len, else_block, else_block_len} = other.data.if_stmt;
						Statement::If {
							condition: condition.into(),
							is_chained: is_chained != 0,
							if_block: std::slice::from_raw_parts(if_block, if_block_len),
							else_block: std::slice::from_raw_parts(else_block, else_block_len),
						}
					}
					c_statement_type::WHILE     => {
						let c_while_stmt_data{condition, block, block_len} = other.data.while_stmt;
						Statement::While {
							condition: condition.into(),
							block: std::slice::from_raw_parts(block, block_len),
						}
					}
					c_statement_type::RETURN    => {
						let c_return_stmt_data{has_value, expr} = other.data.return_stmt;
						Statement::Return {
							expr: (has_value != 0).then(|| expr.assume_init().into()),
						}
					}
					c_statement_type::COMMENT   => {
						Statement::Comment(other.data.comment.to_ntstr())
					}
					c_statement_type::BREAK     => Statement::Break,
					c_statement_type::CONTINUE  => Statement::Continue,
					c_statement_type::EMPTYLINE => Statement::EmptyLine,
					_                    => panic!("unexpected statement variant: {}", other.ty.0),
				}
			}
		}
	}

	impl<'a> From<Statement<'a>> for c_statement<'a> {
		fn from(other: Statement<'a>) -> Self {
			let (ty, data) = match other {
				Statement::Variable{name, ty, assignment_expr}  => {
					let (has_type, actual_type) = ty.map(|x| (1, MaybeUninit::new(x.into()))).unwrap_or((0, MaybeUninit::uninit()));
					(
						c_statement_type::VARIABLE,
						c_statement_data {
							variable: c_local_variable_data {
								name: name.as_ntstrptr(),
								has_type,
								actual_type,
								assignment_expr: assignment_expr.into(),
							}
						}
					)
				}
				Statement::Call(expr) => {
					(
						c_statement_type::CALL,
						c_statement_data {
							call: expr.into(),
						}
					)
				}
				Statement::If{condition, is_chained, if_block, else_block} => {
					(
						c_statement_type::IF,
						c_statement_data {
							if_stmt: c_if_stmt_data {
								condition: condition.into(),
								is_chained: is_chained as u8,
								if_block: if_block.as_ptr(),
								if_block_len: if_block.len(),
								else_block: else_block.as_ptr(),
								else_block_len: else_block.len(),
							}
						}
					)
				}
				Statement::While{condition, block} => {
					(
						c_statement_type::WHILE,
						c_statement_data {
							while_stmt: c_while_stmt_data {
								condition: condition.into(),
								block: block.as_ptr(),
								block_len: block.len(),
							}
						}
					)
				}
				Statement::Return{expr} => {
					let (has_value, expr) = expr.map(|x| (1, MaybeUninit::new(x.into()))).unwrap_or((0, MaybeUninit::uninit()));
					(
						c_statement_type::RETURN,
						c_statement_data {
							return_stmt: c_return_stmt_data {
								has_value,
								expr: expr.into()
							}
						}
					)
				}
				Statement::Comment(comment) => {
					(
						c_statement_type::COMMENT,
						c_statement_data {
							comment: comment.as_ntstrptr(),
						}
					)
				}
				Statement::Break     => {
					(
						c_statement_type::BREAK,
						c_statement_data {
							empty: (),
						}
					)
				}
				Statement::Continue  => {
					(
						c_statement_type::CONTINUE,
						c_statement_data {
							empty: (),
						}
					)
				}
				Statement::EmptyLine => {
					(
						c_statement_type::EMPTYLINE,
						c_statement_data {
							empty: (),
						}
					)
				}
			};
			c_statement {
				ty,
				data,
			}
		}
	}

	struct Argument<'a> {
		name: &'a NTStr,
		ty  : GrugType<'a>,
	}

	impl<'a> From<c_argument<'a>> for Argument<'a> {
		fn from(other: c_argument<'a>) -> Self {
			Self {
				name: other.name.to_ntstr(),
				ty: other.ty.into(),
			}
		}
	}

	impl<'a> From<Argument<'a>> for c_argument<'a> {
		fn from(other: Argument<'a>) -> Self {
			Self {
				name: other.name.as_ntstrptr(),
				ty: other.ty.into(),
			}
		}
	}

	struct OnFunction<'a> {
		name: &'a NTStr,
		arguments: &'a [c_argument<'a>],
		body_statements: &'a [c_statement<'a>],
	}

	impl<'a> From<c_on_function<'a>> for OnFunction<'a> {
		fn from(other: c_on_function<'a>) -> Self {
			unsafe {
				Self {
					name: other.name.to_ntstr(),
					arguments: std::slice::from_raw_parts(other.arguments, other.arguments_len),
					body_statements: std::slice::from_raw_parts(other.body_statements, other.body_statements_len),
				}
			}
		}
	}

	impl<'a> From<OnFunction<'a>> for c_on_function<'a> {
		fn from(other: OnFunction<'a>) -> Self {
			Self {
				name: other.name.as_ntstrptr(),
				arguments: other.arguments.as_ptr(),
				arguments_len: other.arguments.len(),
				body_statements: other.body_statements.as_ptr(),
				body_statements_len: other.body_statements.len(),
			}
		}
	}

	struct HelperFunction<'a> {
		name: &'a NTStr,
		return_type: GrugType<'a>,
		arguments: &'a [c_argument<'a>],
		body_statements: &'a [c_statement<'a>],
	}

	impl<'a> From<c_helper_function<'a>> for HelperFunction<'a> {
		fn from(other: c_helper_function<'a>) -> Self {
			unsafe {
				Self {
					name: other.name.to_ntstr(),
					return_type: other.return_type.into(),
					arguments: std::slice::from_raw_parts(other.arguments, other.arguments_len),
					body_statements: std::slice::from_raw_parts(other.body_statements, other.body_statements_len),
				}
			}
		}
	}

	impl<'a> From<HelperFunction<'a>> for c_helper_function<'a> {
		fn from(other: HelperFunction<'a>) -> Self {
			Self {
				name: other.name.as_ntstrptr(),
				return_type: other.return_type.into(),
				arguments: other.arguments.as_ptr(),
				arguments_len: other.arguments.len(),
				body_statements: other.body_statements.as_ptr(),
				body_statements_len: other.body_statements.len(),
			}
		}
	}

	#[derive(Clone, Copy)]
	pub struct GrugAst<'a> {
		members: &'a [c_member_variable<'a>],
		on_functions: &'a [c_on_function<'a>],
		helper_functions: &'a [c_helper_function<'a>],
	}

	impl<'a> From<c_grug_ast<'a>> for GrugAst<'a> {
		fn from(other: c_grug_ast<'a>) -> Self {
			unsafe {
				Self {
					members: std::slice::from_raw_parts(other.members, other.members_count),
					on_functions: std::slice::from_raw_parts(other.on_functions, other.on_functions_count),
					helper_functions: std::slice::from_raw_parts(other.helper_functions, other.helper_functions_count),
				}
			}
		}
	}

	impl<'a> From<GrugAst<'a>> for c_grug_ast<'a> {
		fn from(other: GrugAst<'a>) -> Self {
			Self {
				members: other.members.as_ptr(),
				members_count: other.members.len(),
				on_functions: other.on_functions.as_ptr(),
				on_functions_count: other.on_functions.len(),
				helper_functions: other.helper_functions.as_ptr(),
				helper_functions_count: other.helper_functions.len(),
			}
		}
	}
}
