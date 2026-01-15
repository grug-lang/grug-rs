use std::sync::Arc;
use std::ffi::{CString, c_char, c_double};
use crate::ntstring::NTStr;
// TODO Unnest some of these enums

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(Clone, Copy)]
pub union GrugValue {
	pub number: c_double,
	pub bool: u8,
	pub id: u64,
	pub string: *const c_char,
	pub void: (),
}

/// SAFETY: GrugValue is !Send and !Sync because of the *mut c_char within it
/// This is just a pointer to a null terminated c string, which is thread safe
unsafe impl Send for GrugValue {}
unsafe impl Sync for GrugValue {}

#[derive(Debug, Clone, PartialEq)]
pub enum GrugType {
	Void,
	Bool,
	Number,
	String,
	Id{
		custom_name: Option<Arc<str>>,
	},
	Resource {
		extension: Arc<str>,
	},
	Entity {
		ty: Option<Arc<str>>,
	},
}
impl GrugType {
	pub(crate) fn match_non_exact(&self, other: &Self) -> bool {
		use GrugType::*;
		match (self, other) {
			(Void, Void) => true,
			(Bool, Bool) => true,
			(Number, Number) => true,
			(String, String) => true,
			(Id{custom_name: custom_name_1}, Id{custom_name: custom_name_2}) => custom_name_1 == custom_name_2 || *custom_name_1 == None || *custom_name_2 == None,
			(
				Resource {
					extension: extension_1,
				}, 
				Resource {
					extension: extension_2,
				}, 
			) => extension_1 == extension_2 || &**extension_1 == "" || &**extension_2 == "",
			(
				Entity {
					ty: ty_1,
				}, 
				Entity {
					ty: ty_2,
				}, 
			) => ty_1 == ty_2 || *ty_1 == None || *ty_2 == None,
			_ => false,
		}
	}
}

impl std::fmt::Display for GrugType {
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
				ty: Some(name),
			} => write!(f, "{}", name),
			Self::Entity {
				ty: None,
			} => write!(f, "entity"),
		}
	}
}

#[derive(Debug)]
pub enum LiteralExpr {
	TrueExpr,
	FalseExpr,
	StringExpr{
		value: Arc<NTStr>,
	},
	ResourceExpr{
		value: Arc<NTStr>,
	},
	EntityExpr{
		value: Arc<NTStr>,
	},
	IdentifierExpr{
		name: Arc<str>
	},
	NumberExpr {
		value: f64,
		string: Arc<str>,
	}
}

#[derive(Debug)]
pub enum ExprType {
	LiteralExpr{
		expr: LiteralExpr,
		line: usize,
		col: usize,
	},
	UnaryExpr{
		operator: UnaryOperator,
		expr: Box<Expr>,
	},
	BinaryExpr{
		operands: Box<(Expr, Expr)>,
		operator: BinaryOperator,
	},
	CallExpr{
		function_name: Arc<str>,
		arguments: Vec<Expr>,
		line: usize,
		col: usize,
	},
	ParenthesizedExpr{
		expr: Box<Expr>,
		line: usize,
		col: usize,
	},
}

impl ExprType {
	pub fn get_last_known_location(&self) -> (usize, usize) {
		let mut current = self;
		loop {
			match current {
				Self::LiteralExpr{
					line,
					col,
					..
				} => return (*line, *col),
				Self::UnaryExpr{
					expr,
					..
				} => current = &expr.ty,
				Self::BinaryExpr{
					operands,
					..
				} => current = &operands.1.ty,
				Self::CallExpr{
					line, col,
					..
				} => return (*line, *col),
				Self::ParenthesizedExpr {
					line, col, 
					..
				} => return (*line, *col),
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
	Not,
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

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
	Or,
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
pub struct Expr {
	pub(super) ty: ExprType,
	// Can be None before Type checking but MUST be Some after
	pub(super) result_ty: Option<GrugType>,
}

#[derive(Debug)]
pub enum GlobalStatement {
	Variable(GlobalVariable),
	OnFunction(OnFunction),
	HelperFunction(HelperFunction),
	Comment{
		value: Arc<str>,
	},
	EmptyLine,
}

#[derive(Debug)]
pub struct OnFunction {
	pub name: Arc<str>,
	pub arguments: Vec<Argument>,
	pub body_statements: Vec<Statement>,
	pub calls_helper_fn: bool,
	pub has_while_loop: bool,
}

#[derive(Debug)]
pub struct HelperFunction {
	pub name: Arc<str>,
	pub arguments: Vec<Argument>,
	pub body_statements: Vec<Statement>,
	pub calls_helper_fn: bool,
	pub has_while_loop: bool,
	pub return_ty: GrugType,
}

#[derive(Debug)]
pub struct GlobalVariable {
	pub name: Arc<str>,
	pub ty: GrugType,
	pub assignment_expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Argument {
	pub(super) name: Arc<str>,
	pub(super) ty: GrugType,
}

// TODO: remove Statement suffix from these variants
// TODO: Statements needs location information
#[derive(Debug)]
pub enum Statement {
	VariableStatement{
		name: Arc<str>,
		ty: Option<GrugType>,
		assignment_expr: Expr,
	},
	CallStatement {
		expr: Expr
	},
	IfStatement{
		condition: Expr,
		if_statements: Vec<Statement>,
		else_if_statements: Vec<(Expr, Vec<Statement>)>,
		else_statements: Option<Vec<Statement>>,
	},
	ReturnStatement{
		expr: Option<Expr>,
	},
	WhileStatement{
		condition: Expr,
		statements: Vec<Statement>,
	},
	Comment{
		value: Arc<str>,
	},
	BreakStatement,
	ContinueStatement,
	EmptyLineStatement,
}
