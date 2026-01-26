
#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef uint64_t grug_id;

typedef grug_id grug_on_fn_id;

typedef grug_id grug_file_id;

typedef grug_id grug_entity_id;

union grug_value {
    double _number;
    bool _bool;
    char const* _string;
    grug_id _id;
};

struct grug_state;

typedef void (*game_fn_void)(struct grug_state* gst, grug_id me, const union grug_value[]);
typedef void (*game_fn_void_argless)(struct grug_state* gst, grug_id me);
typedef union grug_value (*game_fn_value)(struct grug_state* gst, grug_id me, const union grug_value[]);
typedef union grug_value (*game_fn_value_argless)(struct grug_state* gst, grug_id me);

enum grug_error_type_enum {
    grug_error_type_stack_overflow = 0,
    grug_error_type_time_limit_exceeded,
    grug_error_type_game_fn_error,
};

typedef uint32_t grug_error_type;

struct grug_update {
    grug_file_id file;
};

struct grug_updates_list {
    size_t count;
    struct grug_update updates[];
};

typedef void* (*grug_user_alloc_fn)(void* me, size_t size);
typedef void (*grug_user_free_fn)(void* me, void* ptr, size_t size);
// TODO: use strings or give the user the actual ids to the script + function?
typedef void (*runtime_error_handler)(char const* reason, grug_error_type type, char const* on_fn_name, char const* on_fn_path);

struct grug_init_settings {
    void* user_alloc_obj;
    // When null, grug simply calls malloc() / free() instead.
    grug_user_alloc_fn alloc_fn;
    grug_user_free_fn free_fn;
    runtime_error_handler runtime_error_handler;

    // When null, grug assumes "[cwd]/mods"
    char const* mods_folder;
};

struct grug_init_settings grug_default_settings(void);

struct grug_state* grug_init(struct grug_init_settings settings);

void grug_register_game_fn_void_argless(struct grug_state* gst, char const* game_fn_name, game_fn_void_argless fn);
void grug_register_game_fn_value_argless(struct grug_state* gst, char const* game_fn_name, game_fn_value_argless fn);
void grug_register_game_fn_void(struct grug_state* gst, char const* game_fn_name, game_fn_void fn);
void grug_register_game_fn_value(struct grug_state* gst, char const* game_fn_name, game_fn_value fn);

grug_on_fn_id grug_get_fn_id(struct grug_state* gst, char const* type, char const* on_fn_name);

grug_file_id grug_get_script(struct grug_state* gst, char const* script_name);

grug_entity_id grug_create_entity(struct grug_state* gst, grug_file_id script, grug_id id);

void grug_deinit_entity(struct grug_state* gst, grug_entity_id entity);

struct grug_updates_list grug_update(struct grug_state* gst);

void grug_deinit(struct grug_state* gst);

void grug_backend_call_argless(struct grug_state* gst, grug_on_fn_id fn, grug_entity_id entity);
void grug_backend_call(struct grug_state* gst, grug_on_fn_id fn, grug_entity_id entity, const union grug_value args[]);

#define GRUG_CALL_ARGLESS(_state, _on_fn, _entity) grug_backend_call_argless(_state, _on_fn, _entity)

#define GRUG_CALL(_state, _on_fn, _entity, ...) \
    do { \
        const union grug_value _grug_args[] = {__VA_ARGS__}; \
        grug_backend_call(_state, _on_fn, _entity, _grug_args); \
    } while(0);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
static inline union grug_value GRUG_ARG_NUMBER(double v) { union grug_value r; r._number = v; return r; }
static inline union grug_value GRUG_ARG_BOOL(bool v) { union grug_value r; r._bool = v; return r; }
static inline union grug_value GRUG_ARG_STRING(char const* v) { union grug_value r; r._string = v; return r; }
static inline union grug_value GRUG_ARG_ID(grug_id v) { union grug_value r; r._id = v; return r; }
#pragma GCC diagnostic pop


#ifdef __cplusplus
}
#endif
