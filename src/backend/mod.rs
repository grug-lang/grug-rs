use crate::types::{GlobalStatement, GrugValue, GlobalVariable, OnFunction, HelperFunction};
use std::cell::Cell;
use std::sync::Arc;
use std::collections::HashMap;
#[derive(Debug)]
pub struct GrugFile {
	pub(crate) global_variables: Vec<GlobalVariable>,
	pub(crate) on_functions: Vec<OnFunction>,
	pub(crate) helper_functions: Vec<HelperFunction>,
}

pub struct UninitGrugEntity {
	pub(crate) id: u64,
	pub(crate) global_variables: HashMap<Arc<str>, Cell<GrugValue>>,
	pub(crate) file: Arc<GrugFile>,
}

pub struct GrugEntity {
	pub(crate) id: u64,
	pub(crate) global_variables: HashMap<Arc<str>, Cell<GrugValue>>,
	pub(crate) file: Arc<GrugFile>,
}

impl GrugEntity {
	pub(crate) fn get_global_variable(&self, name: &str) -> Option<&Cell<GrugValue>> {
		self.global_variables.get(name)
	}
}
