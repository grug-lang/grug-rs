use std::sync::Arc;
use std::ffi::c_double;
use std::cell::Cell;
use std::ptr::NonNull;
use crate::xar::XarHandle;
use crate::ntstring::{NTStr, NTStrPtr};
use crate::state::GrugState;
// TODO Unnest some of these enums

#[repr(C)]
#[derive(Clone, Copy)]
pub union GameFnPtr {
	pub void: GameFnPtrVoid,
	pub void_argless: GameFnPtrVoidArgless,
	pub value: GameFnPtrValue,
	pub value_argless: GameFnPtrValueArgless,
}

impl std::fmt::Debug for GameFnPtr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		unsafe{self.void.fmt(f)}
	}
}

impl PartialEq for GameFnPtr {
	fn eq(&self, other: &Self) -> bool {
		const _: () = const{assert!(size_of::<GameFnPtr>() == size_of::<usize>())};
		unsafe{std::ptr::fn_addr_eq(self.void, other.void)}
		// unsafe{std::mem::transmute::<Self, usize>(*self) == std::mem::transmute::<Self, usize>(*other)}
	}
}

mod from_impls {
	use super::*;
	impl From<GameFnPtrVoid> for GameFnPtr {
		fn from (value: GameFnPtrVoid) -> Self {
			Self {
				void: value,
			}
		}
	}

	impl From<GameFnPtrVoidArgless> for GameFnPtr {
		fn from (value: GameFnPtrVoidArgless) -> Self {
			Self {
				void_argless: value,
			}
		}
	}

	impl From<GameFnPtrValue> for GameFnPtr {
		fn from (value: GameFnPtrValue) -> Self {
			Self {
				value,
			}
		}
	}

	impl From<GameFnPtrValueArgless> for GameFnPtr {
		fn from (value: GameFnPtrValueArgless) -> Self {
			Self {
				value_argless: value,
			}
		}
	}
}

pub type GameFnPtrVoid = extern "C" fn (state: &GrugState, args: *const GrugValue);
pub type GameFnPtrVoidArgless = extern "C" fn (state: &GrugState);
pub type GameFnPtrValue = extern "C" fn (state: &GrugState, args: *const GrugValue) -> GrugValue;
pub type GameFnPtrValueArgless = extern "C" fn (state: &GrugState) -> GrugValue;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct GrugId(u64);
pub type GrugScriptId = GrugId;

impl std::fmt::Display for GrugId {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl GrugId {
	pub fn new(id: u64) -> Self {
		Self(id)
	}

	pub fn to_inner(self) -> u64 {
		self.0
	}
}

pub type GrugOnFnId = u64;

#[derive(Clone, Copy)]
#[repr(C)]
pub union GrugValue {
	pub number: c_double,
	pub bool: u8,
	pub id: GrugId,
	pub string: NTStrPtr<'static>,
	pub void: (),
}

impl GrugValue {
	pub fn from_bytes(bytes: [u8;8]) -> Self {
		const _: () = const {assert!(std::mem::size_of::<GrugValue>() == std::mem::size_of::<[u8;8]>())};
		unsafe{std::mem::transmute::<[u8;8], Self>(bytes)}
	}
	pub fn as_bytes(self) -> [u8;8] {
		const _: () = const {assert!(std::mem::size_of::<GrugValue>() == std::mem::size_of::<[u8;8]>())};
		unsafe{std::mem::transmute::<Self, [u8;8]>(self)}
	}
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
					ty: ty_1,
				}, 
				Entity {
					ty: ty_2,
				}, 
			) => ty_1 == ty_2 || ty_1.is_none() || ty_2.is_none(),
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

/// A pointer to a grug entity. Only allows shared access to the data and does
/// not allow copying or cloning. Lifetime of shared borrows are limited to the lifetime of self
#[repr(transparent)]
pub struct GrugEntityHandle<'a>(XarHandle<'a, GrugEntity>);

impl<'a> GrugEntityHandle<'a> {
	/// SAFETY: inner can only be deleted by deleting the returned value
	/// The returned value is allowed to create a shared reference to the data at any time 
	pub(crate) unsafe fn new(inner: XarHandle<'a, GrugEntity>) -> Self {
		Self(inner)
	}

	pub(crate) fn into_inner(self) -> XarHandle<'a, GrugEntity> {
		self.0
	}
}

impl<'a> AsRef<GrugEntity> for GrugEntityHandle<'a> {
	fn as_ref(&self) -> &GrugEntity {
		unsafe{self.0.get_ref()}
	}
}

impl<'a> std::ops::Deref for GrugEntityHandle<'a> {
	type Target = GrugEntity;
	fn deref(&self) -> &Self::Target {
		unsafe{self.0.get_ref()}
	}
}

#[derive(Debug)]
pub struct GrugEntity {
	pub id: GrugId,
	pub file_id: GrugScriptId,
	pub members: Cell<NonNull<()>>,
}

impl GrugEntity {
	/// SAFETY: The members of the returned entity are uninitialized
	/// This data must be initialized before it is actually used as an entity
	pub unsafe fn new_uninit(id: GrugId, file_id: GrugScriptId) -> Self {
		Self {
			id,
			file_id,
			members: Cell::new(NonNull::dangling())
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

#[derive(Debug)]
pub struct Variable {
	pub name: Arc<str>,
	pub ty: Option<GrugType>,
	pub assignment_expr: Expr,
}

// TODO: remove Statement suffix from these variants
// TODO: Statements needs location information
#[derive(Debug)]
pub enum Statement {
	Variable(Variable),
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
