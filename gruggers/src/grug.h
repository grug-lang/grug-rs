#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef uint64_t grug_id;

typedef grug_id grug_export_fn_id;
#define INVALID_GRUG_ON_FN_ID UINT64_MAX

typedef grug_id grug_file_id;
#define INVALID_GRUG_FILE_ID UINT64_MAX

typedef grug_id grug_entity_id;
#define INVALID_GRUG_ENTITY_ID UINT64_MAX

typedef grug_id grug_object_id;
#define INVALID_GRUG_OBJECT_ID UINT64_MAX

// Doesn't matter what fields are there as long as the size doesn't change
union grug_value {
	double _number;
	bool _bool;
	// Null terminated, this doesn't use the grug_string type because benchmarks showed adding the extra 8 bytes per value halved argument passing performance even for non-string types.
	char const* _string;
	grug_object_id _id;
};

struct grug_state;

// Information about an entity. 
// These fields should be treated as readonly by the game
// Backends can modify `data` when initialing or deinitializing data
struct grug_entity {
	grug_entity_id id;
	grug_file_id file_id;
	void* data;
};

// A handle to an entity refers to the storage location of a grug_entity.
// These may be reused if an entity is destroyed.
//
// The id field within the entity is unique per entity and will not be reused even after an entity is destroyed
//
// This may either be just an integer or a pointer to the actual storage
typedef struct grug_entity* grug_entity_handle;

// Indicates a location within an (implicit) source file
struct grug_source_span {
	size_t offset;
	size_t line;
};

typedef union grug_value (*host_fn)(struct grug_state* gst, const union grug_value[]);
typedef host_fn (*registration_fn)(struct grug_type_info*);

typedef struct {
	uint8_t tag[4];
} grug_error_kind;

// More specific constants are in error.rs in gruggers core, move them over if they are needed
#define grug_error_kind_none    ((struct grug_error_kind) {{0x0, 0x0, 0x0, 0x0}})
#define grug_error_kind_init    ((struct grug_error_kind) {{0x1, 0x0, 0x0, 0x0}})
#define grug_error_kind_compile ((struct grug_error_kind) {{0x2, 0x0, 0x0, 0x0}})
#define grug_error_kind_runtime ((struct grug_error_kind) {{0x3, 0x0, 0x0, 0x0}})

static inline bool grug_error_kind_matches(grug_error_kind left, grug_error_kind right) {
	size_t i = 0; 
	while (i < sizeof(grug_error_kind) / sizeof(uint8_t)) {
		if left.tag[i] == 0 || right.tag[i] == 0 {
			return true;
		} else if left.tag[i] != right.tag[i] {
			return false;
		}
		i += 1;
	}
	return true;
}

struct grug_error {
	// Represents the kind of error that occurred
	grug_error_kind kind;
	// name of the function the error happened in, if such a function exists
	char* function_name; 
	// path to the file the error occurred in, if there is such a file
	char* file_path;
	// Source line where the error occurred, if there is such a line.
	//
	// Source line may contain a null character so we need to make it a length
	// based string
	struct {
		char* line;
		size_t len;
	} source;
	// Location of the error within a file, if the error occurred in a file
	struct grug_source_span span;
	// Single line error message
	char* error_message;
	// Formatted error message that can be directly printed
	char* error_string;
};

struct grug_file_info {
	// Full path to the file relative to the mods directory
	char* path;
	/// Filename component of the path
	char* file_name;
	/// first level directory within the mods directory
	// TODO: Check that mods directly within the mods directory (i.e, mods with an empty mod_name) don't
	// cause problems. This is technically disallowed by grug but grugc
	// uses this behavior
	char* mod_name;
	/// Portion of the filename between the '-' and '.'
	char* entity_type;
	/// Portion of the filename before the '-'
	char* entity_name;
	
	/// File id of the resultant file
	/// file_id == INVALID_GRUG_FILE_ID indicates that an error occurred
	grug_file_id file_id;
	
	// the error that occurred
	// only valid if file_id == INVALID_GRUG_FILE_ID
	grug_error error;
	pub(crate) error: MaybeUninit<GrugError<'a>>,
}

struct grug_files {
	struct grug_file_info* files;
	size_t count;
}

struct grug_runtime_error_handler {
	void* user_data;
	void (*drop_fn)(void*);
	void (*handler_fn)(
		void* data,
		uint32_t err_kind,
		char* reason_str,
		size_t reason_len,
		char* export_fn_name,
		size_t export_fn_name_len,
		char* script_path,
		size_t script_path_len,
	);
};

struct grug_export_fn_entry {
	struct grug_string entity_name;
	struct grug_string on_fn_name;
	// Counts up from zero for each export_fn
	grug_export_fn_id id;
};

struct grug_export_fns {
	struct grug_export_fn_entry* entries;
	size_t count;
};

struct grug_file {
	/// fill name of the mod file (ex: ak47-Gun.grug)
	struct grug_string name;
	/// what entity type this file implements (ex: Gun)
	struct grug_string entity_type;
	/// the name of the entity
	struct grug_string entity_name;

	/// file id
	grug_file_id id;

	/// Null if there is no error in this file
	struct grug_error* error;
};

enum grug_type

struct grug_type_info {
	
}

// Free all resource owned by the backend
typedef void (*grug_backend_vtable_drop)(void* backend_data);
/// The AST of a typechecked grug file is provided to let the backend do
/// further transforms and lower to bytecode or even machine code
/// `ast` owns allocations that are freed once this function returns. Ensure
/// all resources (including strings) are copied out before it returns;
/// 
/// The script ids are guaranteed to be in contiguous ascending order.
///
/// If the same script id is returned again, then it means the old script
/// associated with the id should be destroyed and replaced with this one. 
///
/// The entity data of all entities created from the old script should be
/// regenerated
typedef void (*grug_backend_vtable_compile_script)(void* backend_data, grug_file_id file_id, struct grug_ast ast);
/// Initialize the member data of the newly created entity. When this
/// function is called, the member field of `entity` points to garbage and
/// must not be deinitialized. The GrugScriptId to be used is obtained from
/// the file_id member of `entity`. 
///
/// `entity` is pinned until it is deinitialized by a call to
/// `destroy_entity_data` or `insert_file` with the same path as its
/// current GrugScriptId. The reference must be stored as a raw pointer
/// within self so that it can be used during `destroy_entity_data` to
/// check for pointer equality. 
/// It is safe to use that pointer as a &GrugEntity in the meantime.
///
/// Returns false if there was a runtime error during execution
typedef bool (*grug_backend_vtable_init_entity)(void* backend_data, struct grug_state* gst, struct grug_entity* entity); 
/// Deinitialize all the data associated with all entities. The pointers
/// stored during `init_entity` must be used to get access to the entity data.
/// The entities can only be accessed as a &GrugEntity even self is available with an exclusive reference
typedef bool (*grug_backend_vtable_clear_entities)(void* backend_data); 
/// Deinitialize the data associated with `entity`. 
typedef void (*grug_backend_vtable_destroy_entity_data)(void* backend_data, struct grug_entity* entity);
/// Run the on function at index `on_fn_index` of the script associated
/// with `entity`.
///
/// # SAFETY: `values` must point to an array of GrugValues of at least as
/// many elements as the number of arguments to the on_ function
///
/// If the number of arguments is 0, then `values` is allowed to be null
typedef bool (*grug_backend_vtable_call_on_function_raw)(void* backend_data, struct grug_state* gst, struct grug_entity* entity, uint64_t on_fn_index, union grug_value* args); 
/// Run the on function at index `on_fn_index` of the script associated
/// with `entity`.
///
/// # Panics: The length of `values` must exactly match the number of
/// expected arguments to the on_ function
typedef bool (*grug_backend_vtable_call_on_function)(void* backend_data, struct grug_state* gst, struct grug_entity* entity, uint64_t on_fn_index, union grug_value* args, size_t args_len); 

struct grug_backend_vtable {
	grug_backend_vtable_compile_script compile_script;
	grug_backend_vtable_init_entity init_entity;
	grug_backend_vtable_clear_entities clear_entities;
	grug_backend_vtable_destroy_entity_data entity_data;
	grug_backend_vtable_call_on_function_raw call_on_function_raw;
	grug_backend_vtable_call_on_function call_on_function;
    grug_backend_vtable_drop drop;
};

struct grug_backend {
	void* obj;
	struct grug_backend_vtable* vtable;
};

// TODO: This should probably be implementation specific
struct grug_init_settings {
	// TODO: We probably want a way to define the mod_api as a string (at least for prototyping)
	char const* mod_api_path;
	char const* mods_dir_path;
	struct grug_runtime_error_handler runtime_error_handler;
	struct grug_backend backend;
};

struct grug_init_settings grug_default_settings(void);

/// Returns null upon an error and writes to out_error
/// Leaks the data associated with the error
struct grug_state* grug_init(struct grug_init_settings settings, struct grug_error* out_error);

struct grug_error* grug_get_error(struct grug_state* gst);

// returns true if registration is successful
// returns false if not.
//
// Reasons for failure include but are not limited to 
// 	- function was not defined in `mod_api.json`. 
// 	- function has already been registered
//
// This function should be able to provide a user data pointer, but grug-rs
// does not handle that in its main branch yet
struct grug_error* grug_register_host_fn       (struct grug_state* gst, char* fn_name, host_fn func);
struct grug_error* grug_register_method        (struct grug_state* gst, char* class_name, char* fn_name, host_fn func);
struct grug_error* grug_register_generic_fn    (struct grug_state* gst, char* fn_name, registration_fn func);
struct grug_error* grug_register_generic_method(struct grug_state* gst, char* class_name, char* fn_name, registration_fn func);

// Returns true if all game functions defined in mod_api.json are registered
struct grug_error* grug_all_host_functions_registered(struct grug_state* gst);

// Get the on_fn_id for a particular on_ function for a particular entity
// returns INVALID_GRUG_EXPORT_FN_ID if an error occurred. 
// call grug_get_error to get the error in that case
grug_export_fn_id grug_get_export_fn_id(struct grug_state* gst, const char* entity_type, const char* on_fn_name);

// Returns a list of all the fn ids for the mod_api.json
struct grug_export_fns grug_get_fn_ids(struct grug_state* gst);

// Compiles a single file from the mods directory
grug_file_id grug_compile_file(struct grug_state* gst, const char* path);

// compiles all files in the mods directory and returns the file info for them
// the returned grug_files only exists until the next call to grug_update
struct grug_files grug_compile_all_files(struct grug_state* gst, const char* path);

// recompiles all files that have been updated since the last call to grug_update
// the returned grug_files only exists until the next call to grug_update
struct grug_files grug_update(struct grug_state* gst);

// returns a list of resource paths that have been updated
// Only considers resources that are actually used by grug files
void grug_get_updated_resources_TODO(struct grug_state* gst);

// Compile a file from a string. Useful for prototypeing or for built in scripts
// If it overlaps with a path on the actual filesystem, it is given the same id as that path
grug_file_id grug_compile_file_from_str_TODO(struct grug_state* gst, const char* path, char* file_text);

// Instantiate an entity from a script
grug_entity_handle grug_create_entity(struct grug_state* gst, grug_file_id script);

// Gets the entity data of an entity. It is UB to pass in an invalid handle
struct grug_entity* grug_entity_get_data(struct grug_state* gst, grug_entity_handle entity);
// Destroy the data associated with an entity
// It is UB to pass in an invalid handle
void grug_deinit_entity(struct grug_state* gst, grug_entity_handle entity);

// Destroy a grug state and free all its resources
void grug_deinit(struct grug_state* gst);

void grug_swap_backend_TODO(struct grug_state* gst, struct grug_backend backend);

// The game may call this at any point, even within an on_fn. However, a backend is entirely free to ignore this call if it happens within an on fn, so beware.
void grug_set_fast_mode_TODO(struct grug_state* gst, bool fast);

// returns false if on function could not be executed, if the id given isn't an entity, or if there was a runtime error
// `args` can be NULL if there are no arguments
bool grug_call_export_function_raw_TODO(struct grug_state* gst, grug_entity_handle entity, grug_export_fn_id on_fn_id, union grug_value* args);
bool grug_call_export_function(struct grug_state* gst, grug_entity_handle entity, grug_export_fn_id on_fn_id, union grug_value* args, size_t args_len);

void grug_set_runtime_error(struct grug_state* gst, char const* message);

#define GRUG_CALL_ARGLESS(_state, _entity, _on_fn_id) \
		grug_call_on_function(_state, _entity, _on_fn_id, NULL, 0); \

#define GRUG_CALL(_state, _entity, _on_fn_id, _args_count, ...) \
		grug_call_on_function(_state, _entity, _on_fn_id, (union grug_value[]) {__VA_ARGS__}, _args_count); \

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
static inline union grug_value GRUG_ARG_NUMBER(double v)      {union grug_value r; r._number = v; return r;}
static inline union grug_value GRUG_ARG_BOOL(bool v)          {union grug_value r; r._bool = v  ; return r;}
static inline union grug_value GRUG_ARG_STRING(char const* v) {union grug_value r; r._string = v; return r;}
static inline union grug_value GRUG_ARG_ID(grug_object_id v)  {union grug_value r; r._id = v    ; return r;}
#pragma GCC diagnostic pop
#ifdef __cplusplus
}
#endif
