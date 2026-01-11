use crate::types::{GlobalStatement, GrugValue};
use std::sync::{Arc, LazyLock};
use std::collections::HashMap;
#[derive(Debug)]
pub struct GrugFile {
	pub(crate) global_variables: Arc<[GlobalStatement]>,
	on_fns: Arc<[GlobalStatement]>,
	helper_fns: Arc<[GlobalStatement]>,
}

pub struct GrugEntity {
	global_variables: HashMap<Arc<str>, GrugValue>,
}

impl GrugEntity {
	pub(crate) fn get_global_variable(&mut self, name: &str) -> Option<&mut GrugValue> {
		self.global_variables.get_mut(name)
	}

	pub fn dummy() -> Self {
		Self {
			global_variables: HashMap::new(),
		}
	}
}
