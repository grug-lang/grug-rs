# Grug

Make your mods Immortal

Grug is a minimal strongly typed, embeddable, designed for long term compatibility. 
It is hot reloadable with extremely fast compile times and allows users to
fully override backends even in closed source applications. You can read more
about grug
(here)[https://mynameistrez.github.io/blog/creating-the-perfect-modding-language]
or watch the video about it
(here)[https://www.youtube.com/watch?v=4oUToVXR2Vo].

Gruggers provides an implementation of the grug language in rust. It comes with
rust bindings, the frontend, two different backends, and even c bindings.

# Basic Usage 

In order to begin using grug, you need to create a `GrugState`. It encapsulates
all the data needed by grug. The `GrugInitSettings` struct provides a builder
API to construct a `GrugState`. 

`./main.rs`
```rs
let mut state = GrugInitSettings::new()
    .set_mods_dir("./mods")
    .set_mod_api_path("./mod_api.json")
    .build_state().unwrap();
``` 


The `GrugState` looks for mods within the mods directory. All mod file paths
are expressed relative to this directory

The API between a grug host and a grug mod is expressed within the
`mod_api.json`. It contains all the kinds of entities allowed in grug mods and
the functions exported by the host for use in mods.

`./mod_api.json`
```json
{
	"entities": {
		"Dog": {
			"description": "An actor that can fight.",
			"on_functions": {
				"on_bark": {
					"description": "Called when the entity is supposed to bark",
					"arguments": [
						{
							"name": "message",
							"type": "string"
						}
					]
				}
			}
		}
	},
	"game_functions": {
		"print_string": {
			"description": "Prints a string",
			"arguments": [
				{
					"name": "msg",
					"type": "string"
				}
			]
		}
	}
}
```

All host functions mentioned in the `mod_api.json` need to be registered in the
state before any file is compiled. Attempting to compile a grug mod that uses a
function that hasn't been registered will cause a panic.

`./main.rs`
```rs
// print_string takes a single string and returns nothing
extern "C" print_string(state: &GrugState, arguments: *const GrugValue) -> GrugValue {
    // SAFETY: Argument types are checked by grug
    let string = unsafe{arguments.read().string}.to_str();
    println!("{}", string);
    GrugValue{void: ()}
}
```

Register the host functions and verify that all expected functions are present
`./main.rs`
```rs
// SAFETY: Expected signature matches signature in mod_api.json
unsafe{state.register_game_fn("print_string", print_string).unwrap()};

state.all_game_fns_registered().unwrap(); // Assert that all functions are registered
```

Write a simple grug mod in the mods directory. Each sub-directory in the mods
directory is a mod. Grug files must be placed within one of these sub directories to be compiled.
The file extension must be `.grug` and the file name must end with the kind of entity expected.
`./mods/first/goldie-Dog.grug`
```grug
on_bark(message: string) {
    print_string(message)
    if message == "arf" {
        print_string(message)
    }
}
```

`state.compile_all_files()` returns a list of all `.grug` files found within
the mods directory. The results member of each file entry contains the file id
or the compile error.

```rs
let files = state.compile_all_files(); 

let ids = file.into_iter().map(|info| info.result.unwrap()).collect::<Vec<_>>();
```

Each file id uniquely represents a particular file path. This ensures that
files maintain an identity even after hot reloading. This file id can be used
to create an entity. 

```rs
let dogs = ids.iter().map(|id| state.create_entity(id).unwrap());
```

In order to call an `on_` functions declared in an entity, you first need to
get the id of that function. Each id uniquely represents a particular `on_`
function a particular entity. The `get_on_fn_id` method can be used if you need
to get a single id, and the `get_on_functions` method can be used to get all
available ids.

```rs
let on_bark_id = state.get_on_fn_id("Dog", "on_bark").unwrap();
```

the `update_files` method reloads all mods that have been modified since the
state was created or since the last call to `udpate_files`. It returns a list
of files that have been updated. Any entity created from any updated file is
automatically recreated. This means that any data stored in the members of
those entities are lost. 

```rs
loop {
    _ = state.update_files();
    for dog in dogs {
        if !state.call_on_function(&*dog, on_bark_id, &[GrugValue{string: nt!("woof").as_ntstrptr()}]) {panic!()};
    }
    std::thread::sleep(Duration::from_secs(1));
}
```

# Usage from C

Gruggers exports a c compatible api compatible with the `grug.h` header from
(grug-for-c)[https://github.com/grug-lang/grug-bench]. 

In order to use gruggers as a static library, link against it alongside its
required libraries. 

for MSVC
```
kernel32.lib ntdll.lib userenv.lib ws2_32.lib dbghelp.lib /defaultlib:msvcrt kernel32.lib
```

for clang
```
-lntdll -luserenv -lws2_32 -ldbghelp -nostdlib -lmsvcrt 
```

for gcc
```
-lgcc_s -lutil -lrt -lpthread -lm -ldl -lc
```
