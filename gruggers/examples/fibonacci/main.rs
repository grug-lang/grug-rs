#![deny(warnings)]
#![allow(static_mut_refs)]
use gruggers::state::{GrugInitSettings, GrugState};
use gruggers::types::Value;

mod game_fns {
	use super::*;
	use super::GrugState;
	pub extern "C" fn print_number<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let number = (*arguments).number;
			println!("{}", number);
		}
		Value{void: ()}
	}
	pub extern "C" fn print_string<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let string = (*arguments).string.to_str();
			println!("{}", string);
		}
		Value{void: ()}
	}
	pub extern "C" fn list_number<'a>(_state: &'a GrugState, _arguments: *const Value) -> Value {
		println!("creating list");
		unsafe {
			let id = _state.get_next_entity_id();
			let x = Vec::<f64>::new();
			OBJECTS.insert(id, Box::new(x));
			Value{id}
		}
	}
	pub extern "C" fn list_number_insert<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let list = (*arguments).id;
			let value = (*arguments.add(1)).number;
			let location = (*arguments.add(2)).number;
			OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().insert(location as usize, value);
		}
		Value{void: ()}
	}
	pub extern "C" fn list_number_remove<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let list = (*arguments).id;
			let location = (*arguments.add(2)).number;
			let ret_val = OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().remove(location as usize);
			Value{number: ret_val}
		}
	}
	pub extern "C" fn list_number_push<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let list = (*arguments).id;
			let value = (*arguments.add(1)).number;
			OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().push(value);
		}
		Value{void: ()}
	}
	pub extern "C" fn list_number_pop<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let list = (*arguments).id;
			let ret_val = OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().pop().unwrap();
			Value{number: ret_val}
		}
	}
	pub extern "C" fn list_number_len<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let list = (*arguments).id;
			let ret_val = OBJECTS.get(&list).unwrap().downcast_ref::<Vec<f64>>().unwrap().len();
			Value{number: ret_val as f64}
		}
	}
	pub extern "C" fn list_number_get<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let list = (*arguments).id;
			let location = (*arguments.add(1)).number;
			let ret_val = *OBJECTS.get(&list).unwrap().downcast_ref::<Vec<f64>>().unwrap().get(location as usize).unwrap();
			Value{number: ret_val}
		}
	}
	pub extern "C" fn list_number_set<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let list = (*arguments).id;
			let value = (*arguments.add(1)).number;
			let location = (*arguments.add(2)).number;
			*OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().get_mut(location as usize).unwrap() = value;
		}
		Value{void: ()}
	}
	pub extern "C" fn print_list_number<'a>(_state: &'a GrugState, arguments: *const Value) -> Value {
		unsafe {
			let id = (*arguments).id;
			let vec = OBJECTS.get(&id).unwrap().downcast_ref::<Vec<f64>>().unwrap();
			println!("{:2.0?}", vec);
		}
		Value{void: ()}
	}
}
use game_fns::*;

use gruggers::types::Id;
use std::any::Any;
pub type GameObjects = HashMap<Id, Box<dyn Any>>;

use std::mem::MaybeUninit;
use std::collections::HashMap;

static mut STATE: UnsafeStatic<GrugState> = unsafe{UnsafeStatic::new()};
static mut OBJECTS: UnsafeStatic<GameObjects> = unsafe{UnsafeStatic::new()};

struct UnsafeStatic<T>(MaybeUninit<T>);
impl<T> UnsafeStatic<T> {
	pub const unsafe fn new() -> Self {
		Self(MaybeUninit::uninit())
	}

	pub unsafe fn write(&mut self, value: T) {
		unsafe {
			self.0.as_mut_ptr().write(value)
		}
	}
}

impl<T> std::ops::Deref for UnsafeStatic<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		unsafe {
			self.0.assume_init_ref()
		}
	}
}
impl<T> std::ops::DerefMut for UnsafeStatic<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		unsafe {
			self.0.assume_init_mut()
		}
	}
}

fn main () {
	unsafe {
		let mut state = GrugInitSettings::new()
			.set_mod_api_path("gruggers/examples/fibonacci/mod_api.json")
			.set_mods_dir("gruggers/examples/fibonacci/mods")
			.build_state().unwrap();

		state.register_host_fn("print_string",       print_string      ).unwrap();
		state.register_host_fn("print_number",       print_number      ).unwrap();
		state.register_host_fn("list_number",        list_number       ).unwrap();
		state.register_host_fn("print_list_number",  print_list_number ).unwrap();
		state.register_host_fn("list_number_insert", list_number_insert).unwrap();
		state.register_host_fn("list_number_remove", list_number_remove).unwrap();
		state.register_host_fn("list_number_push",   list_number_push  ).unwrap();
		state.register_host_fn("list_number_pop",    list_number_pop   ).unwrap();
		state.register_host_fn("list_number_len",    list_number_len   ).unwrap();
		state.register_host_fn("list_number_get",    list_number_get   ).unwrap();
		state.register_host_fn("list_number_set",    list_number_set   ).unwrap();
		
		state.all_host_fns_registered().unwrap();
		STATE.write(state);
		OBJECTS.write(HashMap::new());

		let script_id = STATE.compile_grug_file("fib_script/entity-Fib.grug").unwrap();
		let script    = STATE.create_entity(script_id).unwrap();

		let naive_id      = STATE.get_export_fn_id("Fib", "fib_naive").unwrap();
		let iterative_id  = STATE.get_export_fn_id("Fib", "fib_iterative").unwrap();
		let memo_id       = STATE.get_export_fn_id("Fib", "fib_memoized").unwrap();
		let memo_print_id = STATE.get_export_fn_id("Fib", "print_list").unwrap();

		println!("Naive implementation");
		for i in 0..10 {
			if !STATE.call_export_fn(&*script, naive_id, &[Value{number:i as f64}]) {
				break
			};
		}
		
		println!("iterative implementation");
		for i in 0..10 {
			print!("{i} : ");
			if !STATE.call_export_fn(&*script, iterative_id, &[Value{number:i as f64}]) {
				break
			};
		}

		println!("memoized implementation");
		for i in 0..10 {
			// print!("{i} : ");
			if !STATE.call_export_fn(&*script, memo_id, &[Value{number:i as f64}]) {
				break
			};
		}
		
		if !STATE.call_export_fn(&*script, memo_print_id, &[]) {panic!()};
	}
}
