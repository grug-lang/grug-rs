#![deny(warnings)]
#![allow(static_mut_refs)]
use gruggers::state::GrugState;
use gruggers::types::GrugValue;

use std::ffi::CStr;

mod game_fns {
	use super::*;
	pub extern "C" fn print_number(arguments: *const GrugValue) {
		unsafe {
			let number = (*arguments).number;
			println!("{}", number);
		}
	}
	pub extern "C" fn print_string(arguments: *const GrugValue) {
		unsafe {
			let string = CStr::from_ptr((*arguments).string).to_str().unwrap();
			println!("{}", string);
		}
	}
	pub extern "C" fn list_number() -> GrugValue {
		println!("creating list");
		unsafe {
			let id = STATE.get_id();
			let x = Vec::<f64>::new();
			OBJECTS.insert(id, Box::new(x));
			GrugValue{id}
		}
	}
	pub extern "C" fn list_number_insert(arguments: *const GrugValue) {
		unsafe {
			let list = (*arguments).id;
			let value = (*arguments.add(1)).number;
			let location = (*arguments.add(2)).number;
			OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().insert(location as usize, value);
		}
	}
	pub extern "C" fn list_number_remove(arguments: *const GrugValue) -> GrugValue {
		unsafe {
			let list = (*arguments).id;
			let location = (*arguments.add(2)).number;
			let ret_val = OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().remove(location as usize);
			GrugValue{number: ret_val}
		}
	}
	pub extern "C" fn list_number_push(arguments: *const GrugValue) {
		unsafe {
			let list = (*arguments).id;
			let value = (*arguments.add(1)).number;
			OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().push(value);
		}
	}
	pub extern "C" fn list_number_pop(arguments: *const GrugValue) -> GrugValue {
		unsafe {
			let list = (*arguments).id;
			let ret_val = OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().pop().unwrap();
			GrugValue{number: ret_val}
		}
	}
	pub extern "C" fn list_number_len(arguments: *const GrugValue) -> GrugValue {
		unsafe {
			let list = (*arguments).id;
			let ret_val = OBJECTS.get(&list).unwrap().downcast_ref::<Vec<f64>>().unwrap().len();
			GrugValue{number: ret_val as f64}
		}
	}
	pub extern "C" fn list_number_get(arguments: *const GrugValue) -> GrugValue {
		unsafe {
			let list = (*arguments).id;
			let location = (*arguments.add(1)).number;
			let ret_val = *OBJECTS.get(&list).unwrap().downcast_ref::<Vec<f64>>().unwrap().get(location as usize).unwrap();
			GrugValue{number: ret_val}
		}
	}
	pub extern "C" fn list_number_set(arguments: *const GrugValue) {
		unsafe {
			let list = (*arguments).id;
			let value = (*arguments.add(1)).number;
			let location = (*arguments.add(2)).number;
			*OBJECTS.get_mut(&list).unwrap().downcast_mut::<Vec<f64>>().unwrap().get_mut(location as usize).unwrap() = value;
		}
	}
	pub extern "C" fn print_list_number(arguments: *const GrugValue) {
		unsafe {
			let id = (*arguments).id;
			let vec = OBJECTS.get(&id).unwrap().downcast_ref::<Vec<f64>>().unwrap();
			println!("{:2.0?}", vec);
		}
	}
}
use game_fns::*;

use gruggers::types::GrugId;
use std::any::Any;
pub type GameObjects = HashMap<GrugId, Box<dyn Any>>;

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
		let mut state = GrugState::new("examples/fibonacci/mod_api.json", "examples/fibonacci/mods").unwrap();
		state.register_game_fn("print_string", print_string as extern "C" fn(_)).unwrap();
		state.register_game_fn("print_number", print_number as extern "C" fn(_)).unwrap();
		state.register_game_fn("list_number", list_number as extern "C" fn() -> _).unwrap();
		state.register_game_fn("print_list_number", print_list_number as extern "C" fn(_)).unwrap();
		state.register_game_fn("list_number_insert", list_number_insert as extern "C" fn(_)).unwrap();
		state.register_game_fn("list_number_remove", list_number_remove as extern "C" fn(_) -> _).unwrap();
		state.register_game_fn("list_number_push", list_number_push as extern "C" fn(_)).unwrap();
		state.register_game_fn("list_number_pop", list_number_pop as extern "C" fn(_) -> _).unwrap();
		state.register_game_fn("list_number_len", list_number_len as extern "C" fn(_) -> _).unwrap();
		state.register_game_fn("list_number_get", list_number_get as extern "C" fn(_) -> _).unwrap();
		state.register_game_fn("list_number_set", list_number_set as extern "C" fn(_)).unwrap();
		assert!(state.all_game_fns_registered());
		 STATE.write(state);
		 OBJECTS.write(HashMap::new());

		let script_id = STATE.compile_grug_file("fib_script/entity-Fib.grug").unwrap();
		let script = STATE.create_entity(script_id).unwrap();

		let naive_id = STATE.get_on_fn_id("Fib", "on_fib_naive").unwrap();
		let iterative_id = STATE.get_on_fn_id("Fib", "on_fib_iterative").unwrap();
		let memo_id = STATE.get_on_fn_id("Fib", "on_fib_memoized").unwrap();
		let memo_print_id = STATE.get_on_fn_id("Fib", "on_print_list").unwrap();

		println!("Naive implementation");
		for i in 0..10 {
			let Ok(_) = STATE.call_on_function(script, naive_id, &[GrugValue{number:i as f64}]) else {
				break
			};
		}
		
		println!("iterative implementation");
		for i in 0..10 {
			print!("{i} : ");
			let Ok(_) = STATE.call_on_function(script, iterative_id, &[GrugValue{number:i as f64}]) else {
				break
			};
		}

		println!("memoized implementation");
		for i in 0..10 {
			// print!("{i} : ");
			let Ok(_) = STATE.call_on_function(script, memo_id, &[GrugValue{number:i as f64}]) else {
				break
			};
		}
		
		STATE.call_on_function(script, memo_print_id, &[]).unwrap();
	}
}
