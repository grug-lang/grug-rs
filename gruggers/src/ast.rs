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
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct expr_type(pub u32);
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
		pub op   : unary_operator,
		pub expr : &'a expr<'a>,
	}
	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct binary_op_data<'a> {
		pub op   : binary_operator,
		pub left : &'a expr<'a>,
		pub right: &'a expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct call_data<'a> {
		pub name       : NTStrPtr<'a>,
		pub args       : *const expr<'a>,
		pub args_count : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub union expr_data<'a> {
		pub bool          : (),
		pub string        : NTStrPtr<'a>,
		pub resource      : NTStrPtr<'a>,
		pub entity        : NTStrPtr<'a>,
		pub identifier    : NTStrPtr<'a>,
		pub number        : f64,
		pub unary         : unary_op_data<'a>,
		pub binary        : binary_op_data<'a>,
		pub call          : call_data<'a>,
		pub parenthesized : &'a expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct expr<'a> {
		pub result_filled : u8, /* bool */
		pub result_type   : MaybeUninit::<grug_type<'a>>,
		pub expr_type     : expr_type,
		pub expr_data     : expr_data<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct member_variable<'a> {
		name            : NTStrPtr<'a>,
		ty              : grug_type<'a>,
		assignment_expr : expr<'a>,
	}

	#[repr(transparent)]
	#[derive(Clone, Copy, PartialEq, Eq)]
	pub struct statement_type(pub u32);
	impl statement_type {
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
	pub struct local_variable_data<'a> {
		pub name            : NTStrPtr<'a>,
		pub has_type        : u8, /* bool */
		pub actual_type     : MaybeUninit<grug_type<'a>>,
		pub assignment_expr : expr<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct if_stmt_data<'a> {
		pub condition      : expr<'a>,
		pub is_chained     : u8 /* bool */,
		pub if_block       : *const statement<'a>,
		pub if_block_len   : usize,
		pub else_block     : *const statement<'a>,
		pub else_block_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct while_stmt_data<'a> {
		pub condition : expr<'a>,
		pub block     : *const statement<'a>,
		pub block_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct return_stmt_data<'a> {
		pub has_value : u8 /* bool */,
		pub expr      : MaybeUninit<expr<'a>>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub union statement_data<'a> {
		pub variable    : local_variable_data<'a>,
		pub call        : expr<'a>,
		pub if_stmt     : if_stmt_data<'a>,
		pub while_stmt  : while_stmt_data<'a>,
		pub return_stmt : return_stmt_data<'a>,
		pub comment     : NTStrPtr<'a>,
		pub empty       : (),
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct statement<'a> {
		pub ty  : statement_type,
		pub data: statement_data<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct argument<'a> {
		pub name : NTStrPtr<'a>,
		pub ty   : grug_type<'a>,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct on_function<'a> {
		pub name                : NTStrPtr<'a>,
		pub arguments           : *const argument<'a>,
		pub arguments_len       : usize,
		pub body_statements     : *const statement<'a>,
		pub body_statements_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct helper_function<'a> {
		pub name                : NTStrPtr<'a>,
		pub return_type         : grug_type<'a>,
		pub arguments           : *const argument<'a>,
		pub arguments_len       : usize,
		pub body_statements     : *const statement<'a>,
		pub body_statements_len : usize,
	}

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub struct grug_ast<'a> {
		pub members                : *const member_variable<'a>,
		pub members_count          : usize,

		pub on_functions           : *const on_function<'a>,
		pub on_functions_count     : usize,

		pub helper_functions       : *const helper_function<'a>,
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

	#[derive(Clone, Copy)]
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
			expr : &'a expr<'a>,
		},
		Binary {
			op    : BinaryOperator,
			left  : &'a expr<'a>,
			right : &'a expr<'a>,
		},
		Call {
			name : &'a NTStr,
			args : &'a [expr<'a>],
		},
		Parenthesized(&'a expr<'a>),
	}

	#[derive(Clone, Copy)]
	pub struct Expr<'a> {
		result_type : Option<GrugType<'a>>,
		data        : ExprData<'a>,
	}

	impl<'a> From<expr<'a>> for Expr<'a> {
		fn from(other: expr<'a>) -> Self {
			unsafe {
				let result_type = (other.result_filled != 0).then(|| other.result_type.assume_init().into());
				let data = match other.expr_type {
					expr_type::TRUE          => ExprData::Literal(LiteralExprData::True),
					expr_type::FALSE         => ExprData::Literal(LiteralExprData::False),
					expr_type::STRING        => ExprData::Literal(LiteralExprData::String(other.expr_data.string.to_ntstr())),
					expr_type::RESOURCE      => ExprData::Literal(LiteralExprData::Resource(other.expr_data.resource.to_ntstr())),
					expr_type::ENTITY        => ExprData::Literal(LiteralExprData::Entity(other.expr_data.entity.to_ntstr())),
					expr_type::IDENTIFIER    => ExprData::Literal(LiteralExprData::Identifier(other.expr_data.identifier.to_ntstr())),
					expr_type::NUMBER        => ExprData::Literal(LiteralExprData::Number(other.expr_data.number)),
					expr_type::UNARY         => ExprData::Unary{
						op   : other.expr_data.unary.op.into(), 
						expr : other.expr_data.unary.expr,
					},
					expr_type::BINARY        => ExprData::Binary{
						op    : other.expr_data.binary.op.into(), 
						left  : other.expr_data.binary.left,
						right : other.expr_data.binary.right
					},
					expr_type::CALL          => ExprData::Call{
						name : other.expr_data.call.name.to_ntstr(), 
						args : std::slice::from_raw_parts(other.expr_data.call.args, other.expr_data.call.args_count),
					},
					expr_type::PARENTHESIZED => ExprData::Parenthesized(other.expr_data.parenthesized),
					_                        => panic!("unexpected expression variant: {}", other.expr_type.0),
				};
				Expr {
					result_type,
					data,
				}
			}
		}
	}

	impl<'a> From<Expr<'a>> for expr<'a> {
		fn from(other: Expr<'a>) -> Self {
			let (result_filled, result_type) = other.result_type.map(|x| (1, MaybeUninit::new(x.into()))).unwrap_or((0, MaybeUninit::uninit()));
			let (ty, data) = match other.data {
				ExprData::Literal(literal) => {
					match literal {
						LiteralExprData::True                  => (expr_type::TRUE, expr_data{bool: ()}),
						LiteralExprData::False                 => (expr_type::FALSE, expr_data{bool: ()}),
						LiteralExprData::String(string)        => (expr_type::STRING, expr_data{string: string.as_ntstrptr()}),
						LiteralExprData::Resource(string)      => (expr_type::RESOURCE, expr_data{resource: string.as_ntstrptr()}),
						LiteralExprData::Entity(string)        => (expr_type::ENTITY, expr_data{entity: string.as_ntstrptr()}),
						LiteralExprData::Identifier(string)    => (expr_type::IDENTIFIER, expr_data{identifier: string.as_ntstrptr()}),
						LiteralExprData::Number(number)        => (expr_type::NUMBER, expr_data{number}),
					}
				}
				ExprData::Unary{op, expr}  => {
					(expr_type::UNARY, expr_data {
						unary: unary_op_data {
							op: op.into(),
							expr,
						}
					})
				}
				ExprData::Binary{op, left, right}  => {
					(expr_type::BINARY, expr_data {
						binary: binary_op_data {
							op: op.into(),
							left,
							right,
						}
					})
				}
				ExprData::Call{name, args}  => {
					(expr_type::CALL, expr_data {
						call: call_data {
							name       : name.as_ntstrptr(),
							args       : args.as_ptr(),
							args_count : args.len(),
						}
					})
				}
				ExprData::Parenthesized(expr) => {
					(expr_type::PARENTHESIZED, expr_data {
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
			if_block: &'a [statement<'a>],
			else_block: &'a [statement<'a>],
		},
		While {
			condition: Expr<'a>,
			block: &'a [statement<'a>],
		},
		Return {
			expr: Option<Expr<'a>>,
		},
		Comment(&'a NTStr),
		Break,
		Continue,
		EmptyLine,
	}

	impl<'a> From<statement<'a>> for Statement<'a> {
		fn from (other: statement<'a>) -> Self {
			unsafe {
				match other.ty {
					statement_type::VARIABLE  => {
						let local_variable_data{name, has_type, actual_type, assignment_expr} = other.data.variable;
						Statement::Variable {
							name: name.to_ntstr(),
							ty: (has_type != 0).then(|| actual_type.assume_init().into()),
							assignment_expr: assignment_expr.into(),
						}
					}
					statement_type::CALL      => {
						Statement::Call(other.data.call.into())
					}
					statement_type::IF        => {
						let if_stmt_data{condition, is_chained, if_block, if_block_len, else_block, else_block_len} = other.data.if_stmt;
						Statement::If {
							condition: condition.into(),
							is_chained: is_chained != 0,
							if_block: std::slice::from_raw_parts(if_block, if_block_len),
							else_block: std::slice::from_raw_parts(else_block, else_block_len),
						}
					}
					statement_type::WHILE     => {
						let while_stmt_data{condition, block, block_len} = other.data.while_stmt;
						Statement::While {
							condition: condition.into(),
							block: std::slice::from_raw_parts(block, block_len),
						}
					}
					statement_type::RETURN    => {
						let return_stmt_data{has_value, expr} = other.data.return_stmt;
						Statement::Return {
							expr: (has_value != 0).then(|| expr.assume_init().into()),
						}
					}
					statement_type::COMMENT   => {
						Statement::Comment(other.data.comment.to_ntstr())
					}
					statement_type::BREAK     => Statement::Break,
					statement_type::CONTINUE  => Statement::Continue,
					statement_type::EMPTYLINE => Statement::EmptyLine,
					_                    => panic!("unexpected statement variant: {}", other.ty.0),
				}
			}
		}
	}

	impl<'a> From<Statement<'a>> for statement<'a> {
		fn from(other: Statement<'a>) -> Self {
			let (ty, data) = match other {
				Statement::Variable{name, ty, assignment_expr}  => {
					let (has_type, actual_type) = ty.map(|x| (1, MaybeUninit::new(x.into()))).unwrap_or((0, MaybeUninit::uninit()));
					(
						statement_type::VARIABLE,
						statement_data {
							variable: local_variable_data {
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
						statement_type::CALL,
						statement_data {
							call: expr.into(),
						}
					)
				}
				Statement::If{condition, is_chained, if_block, else_block} => {
					(
						statement_type::IF,
						statement_data {
							if_stmt: if_stmt_data {
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
						statement_type::WHILE,
						statement_data {
							while_stmt: while_stmt_data {
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
						statement_type::RETURN,
						statement_data {
							return_stmt: return_stmt_data {
								has_value,
								expr: expr.into()
							}
						}
					)
				}
				Statement::Comment(comment) => {
					(
						statement_type::COMMENT,
						statement_data {
							comment: comment.as_ntstrptr(),
						}
					)
				}
				Statement::Break     => {
					(
						statement_type::BREAK,
						statement_data {
							empty: (),
						}
					)
				}
				Statement::Continue  => {
					(
						statement_type::CONTINUE,
						statement_data {
							empty: (),
						}
					)
				}
				Statement::EmptyLine => {
					(
						statement_type::EMPTYLINE,
						statement_data {
							empty: (),
						}
					)
				}
			};
			statement {
				ty,
				data,
			}
		}
	}

	struct Argument<'a> {
		name: &'a NTStr,
		ty  : GrugType<'a>,
	}

	impl<'a> From<argument<'a>> for Argument<'a> {
		fn from(other: argument<'a>) -> Self {
			Self {
				name: other.name.to_ntstr(),
				ty: other.ty.into(),
			}
		}
	}

	impl<'a> From<Argument<'a>> for argument<'a> {
		fn from(other: Argument<'a>) -> Self {
			Self {
				name: other.name.as_ntstrptr(),
				ty: other.ty.into(),
			}
		}
	}

	struct OnFunction<'a> {
		name: &'a NTStr,
		arguments: &'a [argument<'a>],
		body_statements: &'a [statement<'a>],
	}

	impl<'a> From<on_function<'a>> for OnFunction<'a> {
		fn from(other: on_function<'a>) -> Self {
			unsafe {
				Self {
					name: other.name.to_ntstr(),
					arguments: std::slice::from_raw_parts(other.arguments, other.arguments_len),
					body_statements: std::slice::from_raw_parts(other.body_statements, other.body_statements_len),
				}
			}
		}
	}

	impl<'a> From<OnFunction<'a>> for on_function<'a> {
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
		arguments: &'a [argument<'a>],
		body_statements: &'a [statement<'a>],
	}

	impl<'a> From<helper_function<'a>> for HelperFunction<'a> {
		fn from(other: helper_function<'a>) -> Self {
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

	impl<'a> From<HelperFunction<'a>> for helper_function<'a> {
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
		members: &'a [member_variable<'a>],
		on_functions: &'a [on_function<'a>],
		helper_functions: &'a [helper_function<'a>],
	}

	impl<'a> From<grug_ast<'a>> for GrugAst<'a> {
		fn from(other: grug_ast<'a>) -> Self {
			unsafe {
				Self {
					members: std::slice::from_raw_parts(other.members, other.members_count),
					on_functions: std::slice::from_raw_parts(other.on_functions, other.on_functions_count),
					helper_functions: std::slice::from_raw_parts(other.helper_functions, other.helper_functions_count),
				}
			}
		}
	}

	impl<'a> From<GrugAst<'a>> for grug_ast<'a> {
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


