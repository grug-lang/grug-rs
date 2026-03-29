use crate::backend::Backend;
use crate::types::{GrugFileId, GrugEntity, GrugValue};
use crate::ast::GrugAst;

use std::pin::Pin;

use gruggers_core::state::State;

/// An empty backend that simply throws away any file given to it.
/// Cannot be used to create entities or call functions.
/// 
/// Will throw if another backend is swapped into this
pub struct StubBackend;

impl Backend for StubBackend {
	fn insert_file<GrugState: State>(&self, _state: &GrugState, _id: GrugFileId, _file: GrugAst) { }
	fn init_entity<GrugState: State>(&self, _state: &GrugState, _entity: Pin<&GrugEntity>) -> bool {
		panic!("Tried to initialize entity with stub backend");
	}
	fn clear_entities(&mut self) { }
	fn destroy_entity_data(&self, _entity: &GrugEntity) { }
	unsafe fn call_on_function_raw<GrugState: State>(&self, _state: &GrugState, _entity: &GrugEntity, _on_fn_index: usize, _values: *const GrugValue) -> bool {
		panic!("Tried to call export function with stub backend");
	}
	fn call_on_function<GrugState: State>(&self, _state: &GrugState, _entity: &GrugEntity, _on_fn_index: usize, _values: &[GrugValue]) -> bool {
		panic!("Tried to call export function with stub backend");
	}	
}
