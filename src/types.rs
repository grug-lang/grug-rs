use std::sync::Arc;
// TODO Unnest some of these enums

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
		value: Arc<str>,
	},
	ResourceExpr{
		value: Arc<str>,
	},
	EntityExpr{
		value: Arc<str>,
	},
	IdentifierExpr{
		name: Arc<str>
	},
	NumberExpr {
		value: f64,
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
	GlobalVariableStatement{
		name: Arc<str>,
		ty: GrugType,
		assignment_expr: Expr,
	},
	GlobalOnFunction{
		name: Arc<str>,
		arguments: Vec<Argument>,
		body_statements: Vec<Statement>,
		calls_helper_fn: bool,
		has_while_loop: bool,
	},
	GlobalHelperFunction{
		name: Arc<str>,
		arguments: Vec<Argument>,
		body_statements: Vec<Statement>,
		calls_helper_fn: bool,
		has_while_loop: bool,
		return_ty: GrugType,
	},
	GlobalComment{
		value: Arc<str>,
	},
	GlobalEmptyLine,
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
		else_statements: Vec<Statement>,
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
