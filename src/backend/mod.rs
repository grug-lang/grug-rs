use crate::types::{GlobalStatement, GrugValue};
use std::sync::Arc;
use std::collections::HashMap;
#[derive(Debug)]
pub struct GrugFile {
	pub(crate) global_variables: Vec<GlobalStatement>,
	pub(crate) on_functions: Vec<GlobalStatement>,
	pub(crate) helper_functions: Vec<GlobalStatement>,
}

pub struct UninitGrugEntity {
	pub(crate) id: u64,
	pub(crate) global_variables: HashMap<Arc<str>, GrugValue>,
	pub(crate) file: Arc<GrugFile>,
}

pub struct GrugEntity {
	pub(crate) id: u64,
	pub(crate) global_variables: HashMap<Arc<str>, GrugValue>,
	pub(crate) file: Arc<GrugFile>,
}

impl GrugEntity {
	pub(crate) fn get_global_variable(&mut self, name: &str) -> Option<&mut GrugValue> {
		self.global_variables.get_mut(name)
	}
}
