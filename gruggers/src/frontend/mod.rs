use crate::arena::Arena;
use crate::ast::*;
use crate::error::{Error, ErrorKind, SourceSpan};
use crate::mod_api::ModApi;
use crate::ntstring::{NTOsStrPtr, NTStr, NTStrPtr};
use crate::own_ptr::OwnPtr;
use crate::state::{FileInfo, Files, GrugState, ResourcePaths};
use crate::type_storage::TypeStorage;
use crate::types::{FileId, Mode};
use crate::watcher::vanished_while_listing;

use allocator_api2::vec::Vec;

use std::ffi::OsStr;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::mpsc::{Receiver, Sender};
// use std::path::Path;

const MAX_FILE_ENTITY_TYPE_LENGTH: usize = 420;
pub(crate) const SPACES_PER_INDENT: usize = 4;

pub mod parser;
pub mod tokenizer;
pub mod type_propagation;
use type_propagation::TypePropagator;

pub type CompileRequest = (Arena, &'static [&'static OsStr]);
pub type CompileResponse = (
	Arena,
	OwnPtr<
		'static,
		[(
			Result<GrugAst<'static>, Error>,
			// derived from input paths
			// These paths are relative
			&'static OsStr,
		)],
	>,
);

/// The mode of every file one call compiles, given by the host.
///
/// grug's mode is per file and fixed when the file compiles (see [`Mode`]), so
/// a call that compiles many files cannot take one mode: it takes the whole
/// decision. [`Mode`] is designed around a mod, which is the first directory
/// component of a file's path relative to the mods directory (what
/// [`FileInfo::mod_name`] reports), so the host answers per mod and every file
/// of that mod compiles in the mode the host gave it.
///
/// Answering per mod rather than per file is what keeps a file's mode equal to
/// its mod's however the mod's files change: a file that is hot reloaded or
/// added later asks the same question and gets the same answer, without the
/// host having to name that new file. A host that grants a mod unsafe mode
/// therefore keeps the grant until it answers differently, and the way to take
/// a mode back is to pass different settings and let the mod's files recompile.
pub struct CompileSettings<'a> {
    /// Answers unsafe for a mod's name, or is `None`, which is safe for every
    /// file and lets a host that grants nothing pass no decision at all.
    is_unsafe_mod: Option<&'a dyn Fn(&OsStr) -> bool>,
}

impl<'a> CompileSettings<'a> {
    /// Every file compiles in [`Mode::Safe`], which is what a host that grants
    /// nothing passes.
    pub const fn safe() -> Self {
        Self {
            is_unsafe_mod: None,
        }
    }

    /// Asks `is_unsafe_mod` for the mod of each file it compiles, which
    /// compiles in [`Mode::Unsafe`] when it answers true and in [`Mode::Safe`]
    /// when it answers false.
    ///
    /// It is asked once per file a call installs, on the thread that made the
    /// call, with the file's mod: the first directory component of the file's
    /// path, or the empty string for a file directly within the mods directory.
    /// It must give the same answer for the same name for the length of that
    /// call, since two files of one mod must not end up in different modes. It
    /// also has to outlive the settings, so a closure that is not a plain
    /// function is bound to a variable first.
    pub const fn new(is_unsafe_mod: &'a dyn Fn(&OsStr) -> bool) -> Self {
        Self {
            is_unsafe_mod: Some(is_unsafe_mod),
        }
    }

    /// The mode `path` compiles in, where `path` is relative to the mods
    /// directory, like every other path in this module.
    pub fn mode(&self, path: &OsStr) -> Mode {
        match self.is_unsafe_mod {
            None => Mode::Safe,
            Some(is_unsafe_mod) if is_unsafe_mod(get_mod_name(path)) => Mode::Unsafe,
            Some(_) => Mode::Safe,
        }
    }
}

// Compilation functions
impl GrugState {
	
    /// Send at most this many files to a thread for compilation
    const FILES_PER_THREAD: usize = 8;
    // All 'static fields are actually allocated within the arena
    /// Uses async file system apis on windows
    pub(crate) fn compiler_thread_fn(
        // these paths are relative
        receiver: Receiver<CompileRequest>,
        sender: Sender<CompileResponse>,
        // path is absolute
        mods_dir_path: PathBuf,
        mod_api: Arc<ModApi>,
    ) -> impl FnOnce() {
        use crate::async_fs::{open_file_async_for_read, read_files_async};
        move || {
            let mut temp_arena = Arena::new();
            let mut type_storage = TypeStorage::new();

            for (arena, files) in receiver.iter() {
                temp_arena.clear();
                // This is the actual lifetime of the data but it has to be erased to send across the channel
                fn combine_lifetimes<'a>(
                    _: &'a Arena,
                    input: &'static [&'static OsStr],
                ) -> &'a [&'a OsStr] {
                    input
                }
                let files = combine_lifetimes(&arena, files);

                let mut results = Vec::new_in(&arena);
                // read all files
                // split into files that can be opened and files that can't
                let mut ok_files = Vec::new_in(&arena);
                for file_path in files.iter().copied() {
                    let mut abs_path = mods_dir_path.clone();
                    abs_path.push(file_path);
                    match open_file_async_for_read(&abs_path) {
                        Ok(file) => {
                            ok_files.push((file, file_path));
                        }
                        Err(err) => {
                            results.push((Err(err), file_path));
                        }
                    }
                }
                // read the contents of files that can be read
                let ok_files_data =
                    read_files_async(ok_files.iter().map(|(file, path)| (file, *path)), &arena);
                // compile files one by one and collect errors, asts and resources
                results.extend(ok_files_data.into_iter().zip(&ok_files).map(
                    |(data, (_, path))| {
                        let path = *path;
                        let file_text = match data {
                            Ok(data) => data,
                            // Only reachable when reading fails after opening succeeded
                            Err(err) => return (Err(err), path), // grcov-excl-line
                        };

                        // compile the file
                        let ast = match Self::compile_inner(
                            path,
                            file_text,
                            mods_dir_path.as_ref(),
                            &mod_api,
                            &arena,
                            &temp_arena,
                            &mut type_storage,
                        ) {
                            Ok(data) => data,
                            Err(err) => return (Err(err), path),
                        };
                        // collect errors and resources
                        (Ok(ast), path)
                    },
                ));
                drop(ok_files);

                let results = unsafe {
                    std::mem::transmute::<
                        OwnPtr<[(Result<GrugAst<'_>, Error>, &OsStr)]>,
                        OwnPtr<'static, [(Result<GrugAst<'static>, Error>, &'static OsStr)]>,
                    >(results.into_boxed_slice().into())
                };
                // The state stopped listening, because it is being dropped
                let Ok(()) = sender.send((arena, results)) else {
                    break; // grcov-excl-line
                };
            }
        }
    }

    /// Compile a grug file at a relative path within the mods directory. Once
    /// compiled directly once, the file will be automatically hot reloaded by
    /// the state.
    ///
    /// `settings` decides the mode the file runs in ([`CompileSettings`]).
    pub fn compile_grug_file(
        &self,
        path: impl AsRef<OsStr>,
        settings: CompileSettings<'_>,
    ) -> Result<FileId, Error> {
        let path = path.as_ref();
        let mut path_buf = self.mods_dir_path.clone();
        path_buf.push("/");
        path_buf.push(path);

        let file_text = match std::fs::read_to_string(path_buf) {
            Ok(file_text) => file_text,
            Err(err) => return Err(Error::from_io_error(err, path)),
        };

        self.compile_grug_file_from_str(path, &file_text, settings)
    }

    /// Compile a grug file directly from a string. The path is required to
    /// identify the entity type and to uniquely identify a particular script
    /// for hot reloading.
    ///
    /// If the path is the same as a path within the mods directory, when the
    /// actual file changes, the script will be hot reloaded.
    ///
    /// If a file has already been compiled with the same path, the script will
    /// be hot reloaded.
    ///
    /// `settings` decides the mode the file runs in from now on
    /// ([`CompileSettings`]).
    pub fn compile_grug_file_from_str(
        &self,
        path: impl AsRef<OsStr>,
        file_text: &str,
        settings: CompileSettings<'_>,
    ) -> Result<FileId, Error> {
        use crate::async_fs::verify_file_data;
        let path = path.as_ref();

        let mut arena = self.arenas.borrow_mut().pop().unwrap_or_default();
        let file_text = arena.copy_bytes_into_nt(file_text.as_bytes());
        let file_text = verify_file_data(file_text, path)?;
        // immediately invoked closure so we get try {} finally {}
        let id = (|| {
            let file = Self::compile_inner(
                path,
                file_text,
                &self.mods_dir_path,
                &self.mod_api,
                &arena,
                &arena,
                &mut self.type_storage.borrow_mut(),
            )?;
            let id = self
                .get_or_insert_script_id(path.as_ref())
                .map_err(|err| Error::from_io_error(err, path))?;
            self.backend.insert_file(id, &file, settings.mode(path));

            self.track_file_entities(id);

            Ok(id)
        })();
        arena.clear();
        self.arenas.borrow_mut().push(arena);

        id
    }

    // Starts tracking the entities of a file that was compiled for the first
    // time, or reinitializes the entities of a file that was compiled before
    fn track_file_entities(&self, id: FileId) {
        let mut script_entities = self.script_entities.borrow_mut();
        if (id.to_inner() as usize) < script_entities.len() {
            for entity in &script_entities[id.to_inner() as usize] {
                self.backend.init_entity(self, unsafe { &*entity.as_ptr() });
            }
        } else if id.to_inner() as usize == script_entities.len() {
            script_entities.push(std::vec::Vec::new());
        } else {
            // File ids are handed out in order, so a new id is always the next one
            unreachable!(); // grcov-excl-line
        }
    }

    /// Compile all the files within the mods directory. Uses asynchronous file
    /// system apis if available on the current platform
    ///
    /// `settings` decides the mode of each file ([`CompileSettings`]).
    pub fn compile_all_files(&self, settings: CompileSettings<'_>) -> Files {
        // iterate over all files and get all valid paths
        let arena = self.arenas.borrow_mut().pop().unwrap_or_default();

        let mut file_paths = Vec::new_in(&arena);
        let mut files = std::vec::Vec::new();

        let mods_dir_len = if self
            .mods_dir_path
            .as_encoded_bytes()
            .last()
            .is_some_and(|x| *x != b'\\' && *x != b'/')
        {
            self.mods_dir_path.len() + 1
        } else {
            self.mods_dir_path.len()
        };
        // Iterate through every directory within the mods directory and
        // collect relative paths to all grug scripts
        for mod_dir in
            std::fs::read_dir(&self.mods_dir_path).expect("Could not read mods directory")
        {
            let Ok(mod_dir) = mod_dir else {
                panic!("unable to read directory: {:?}", mod_dir); // grcov-excl-line
            };
            let mut entries_to_check = std::vec::Vec::from([mod_dir]);

            while let Some(next_entry) = entries_to_check.pop() {
                let metadata = match next_entry.metadata() {
                    Ok(metadata) => metadata,
                    // The entry was deleted after its directory was listed, which is a deletion
                    // rather than a failure to start (#908). Nothing in a test can delete it
                    // between the listing and this stat, so the rule is unproven here and is the
                    // one the poller's walk tests already force.
                    Err(err) if vanished_while_listing(&err) => continue, // grcov-excl-line
                    // grcov-excl-start: a stat that fails for any other reason is a real error
                    Err(err) => panic!(
                        "could not read metadata of '{}': {}",
                        next_entry.path().to_string_lossy(),
                        err
                    ),
                    // grcov-excl-stop
                };
                if metadata.is_dir() {
                    let next_entry_path = next_entry.path();
                    let entries = match std::fs::read_dir(&next_entry_path) {
                        Ok(entries) => entries,
                        // A subdirectory deleted after its parent listed it is a deletion too, and
                        // is unproven for the same reason.
                        Err(err) if vanished_while_listing(&err) => continue, // grcov-excl-line
                        // grcov-excl-start: an unreadable directory is a real error
                        Err(err) => panic!(
                            "could not read '{}': {}",
                            next_entry_path.to_string_lossy(),
                            err
                        ),
                        // grcov-excl-stop
                    };
                    for entry in entries {
                        let Ok(entry) = entry else {
                            panic!("unable to read entry: {:?}", entry); // grcov-excl-line
                        };
                        entries_to_check.push(entry);
                    }
                } else {
                    let entry_path = next_entry.path();
                    if let Some(extension) = entry_path.extension()
                        && extension == "grug"
                    {
                        // get path relative to mods dir
                        let rel_path = unsafe {
                            OsStr::from_encoded_bytes_unchecked(
                                &entry_path.as_os_str().as_encoded_bytes()[mods_dir_len..],
                            )
                        };
                        // copy path into arena
                        let rel_path = arena.copy_osstr_into(rel_path);
                        file_paths.push(rel_path);
                    };
                }
            }
        }

        let mut next_thread = self.compiler_senders.iter().cycle();
        let sent_count = file_paths.len();
        let mut recv_count = 0;
        // Chunk into FILES_PER_THREAD sized blocks
        for chunk in file_paths.chunks(Self::FILES_PER_THREAD) {
            let cur_arena = self.arenas.borrow_mut().pop().unwrap_or_default();

            // We need to allocate the paths into the new arena to ensure panic safety
            //
            // If these strings were allocated in the outer arena (or if
            // they are a reference to the existing PathBufs), then if this
            // thread panics for any reason, the strings will be freed and
            // the compiler threads will access freed memory.
            let chunk = cur_arena.slice_from_iter(
                chunk
                    .iter()
                    .map(|item| cur_arena.copy_osstr_into(item.as_ref())),
            );
            // SAFETY: make sure we never use this slice outside the current loop iteration
            let chunk =
                unsafe { std::mem::transmute::<&[&OsStr], &'static [&'static OsStr]>(chunk) };
            // Send to threads
            next_thread
                .next()
                .expect("at least one compiler thread")
                .send((cur_arena, chunk))
                .expect("send succeeds");
        }

        // Send to backend while recieving
        while recv_count < sent_count {
            let (mut current_arena, results) = self.compiler_receiver.recv().unwrap();
            recv_count += results.len();

            for (result, path) in results {
                // turn ok result into id, allocate error into outer arena
                let result = match result {
                    Ok(ast) => {
                        let id = match self.get_or_insert_script_id(path.as_ref()) {
                            Ok(id) => id,
                            // The compiler thread read the file and the file system no longer has
                            // it, which is a deletion like any other: the walk reports it as the
                            // change to the file's parent directory, and installing it would abort
                            // the host (#908).
                            Err(err) if vanished_while_listing(&err) => continue,
                            // grcov-excl-start: a canonicalize that fails for another reason is a
                            // real error
                            Err(err) => panic!(
                                "error while canonicalizing '{}': {}",
                                path.to_string_lossy(),
                                err
                            ),
                            // grcov-excl-stop
                        };
                        // Send to backend
                        self.backend.insert_file(id, &ast, settings.mode(path));

                        self.track_file_entities(id);
                        Ok(id)
                    }
                    Err(err) => {
                        let err = err.inner().copy_into(&arena);
                        Err(err)
                    }
                };
                // Create FileInfo from this result
                let path = <OsStr as AsRef<Path>>::as_ref(path);
                let mod_dir_path = path
                    .parent()
                    .expect("must have at least one component in path")
                    .components()
                    .next()
                    .unwrap()
                    .as_os_str();

                let entity_type = get_entity_type(path.as_os_str()).unwrap_or("".as_ref());
                let file_prefix = path
                    .file_prefix()
                    .unwrap()
                    .to_str()
                    .expect("non utf8 file name");
                let entity_name = file_prefix
                    .split_once('-')
                    .unwrap_or((file_prefix, "// ignored"))
                    .0;

                let info = FileInfo::new_in(
                    path.as_os_str(),
                    path.file_name().unwrap(),
                    mod_dir_path,
                    entity_type,
                    entity_name.as_ref(),
                    result,
                    &arena,
                );
                files.push(info);
            }
            // `results` and `resources` are allocated within current_arena, so it
            // is only safe to clear the current_arena now.
            current_arena.clear();
            self.arenas.borrow_mut().push(current_arena);
        }
        drop(file_paths);
        Files {
            inner: unsafe {
                std::mem::transmute::<OwnPtr<[FileInfo]>, OwnPtr<'static, [FileInfo]>>(
                    files.into_boxed_slice().into(),
                )
            },
            _arena: arena,
        }
    }

    /// Check if there are any files in the mods directory that need to be hot reloaded.
    ///
    /// Also returns the paths of every other (non-`.grug`) file within the
    /// mods directory that changed. The entire mods directory tree is
    /// watched recursively, so a path is reported here regardless of
    /// whether it's referenced by a `resource` string inside a `.grug`
    /// script: those strings are only used to validate that a resource
    /// exists at compile time, they no longer register a file watch.
    ///
    /// `settings` decides the mode of each file it compiles, so a host changes
    /// a mod's mode by passing different settings: the next change to any of
    /// that mod's files recompiles it in its new mode.
    pub fn update_files(&self, settings: CompileSettings<'_>) -> (ResourcePaths, Files) {
        let arena = self.arenas.borrow_mut().pop().unwrap_or_default();
        let mut file_paths = Vec::new_in(&arena);
        let mut updated_resources = std::vec::Vec::new();

        let mut grug_files = std::vec::Vec::new();
        for change in self.changes.try_iter() {
            let file_name = change.expect("File IO error");
            if let Some(extension) = <OsStr as AsRef<Path>>::as_ref(&file_name).extension()
                && extension == "grug"
            {
                if !file_paths.contains(&&*file_name) {
                    let rel_name = arena.copy_osstr_into(file_name.as_ref());
                    file_paths.push(rel_name);
                }
            } else if !updated_resources.contains(&file_name) {
                updated_resources.push(file_name);
            }
        }

        let mut next_thread = self.compiler_senders.iter().cycle();
        let sent_count = file_paths.len();
        let mut recv_count = 0;
        // Chunk into FILES_PER_THREAD sized blocks
        for chunk in file_paths.chunks(Self::FILES_PER_THREAD) {
            let cur_arena = self.arenas.borrow_mut().pop().unwrap_or_default();

            // We need to allocate the paths into the new arena to ensure panic safety
            //
            // If these strings were allocated in the outer arena (or if
            // they are a reference to the existing PathBufs), then if this
            // thread panics for any reason, the strings will be freed and
            // the compiler threads will access freed memory.
            let chunk = cur_arena.slice_from_iter(
                chunk
                    .iter()
                    .map(|item| cur_arena.copy_osstr_into(item.as_ref())),
            );
            // SAFETY: make sure we never use this slice outside the current loop iteration
            let chunk =
                unsafe { std::mem::transmute::<&[&OsStr], &'static [&'static OsStr]>(chunk) };
            // Send to threads
            next_thread
                .next()
                .expect("at least one compiler thread")
                .send((cur_arena, chunk))
                .expect("send succeeds");
        }

        // Send to backend while recieving
        while recv_count < sent_count {
            let (mut current_arena, results) = self.compiler_receiver.recv().unwrap();
            recv_count += results.len();

            for (result, path) in results {
                // turn ok result into id, copy err into current arena
                let result = match result {
                    Ok(ast) => {
                        let id = match self.get_or_insert_script_id(path.as_ref()) {
                            Ok(id) => id,
                            // The compiler thread read the file and the file system no longer has
                            // it, which is a deletion like any other: the walk reports it as the
                            // change to the file's parent directory, and installing it would abort
                            // the host (#908).
                            Err(err) if vanished_while_listing(&err) => continue,
                            // grcov-excl-start: a canonicalize that fails for another reason is a
                            // real error
                            Err(err) => panic!(
                                "error while canonicalizing '{}': {}",
                                path.to_string_lossy(),
                                err
                            ),
                            // grcov-excl-stop
                        };
                        // Send to backend
                        self.backend.insert_file(id, &ast, settings.mode(path));

                        self.track_file_entities(id);
                        Ok(id)
                    }
                    Err(err) => {
                        let err = err.inner().copy_into(&arena);
                        Err(err)
                    }
                };
                // Create FileInfo from this result
                let path = <OsStr as AsRef<Path>>::as_ref(path);
                let mod_dir_path = path
                    .parent()
                    .expect("must have at least one component in path")
                    .components()
                    .next()
                    .unwrap()
                    .as_os_str();

                let entity_type = get_entity_type(path.as_os_str()).unwrap_or("");
                let file_prefix = path.file_prefix().unwrap().to_str().unwrap_or("");
                let entity_name = file_prefix
                    .split_once('-')
                    .unwrap_or((file_prefix, "// ignored"))
                    .0;

                let info = FileInfo::new_in(
                    path.as_os_str(),
                    path.file_name().unwrap(),
                    mod_dir_path,
                    entity_type,
                    entity_name.as_ref(),
                    result,
                    &arena,
                );
                grug_files.push(info);
            }
            // `results` and `resources` are allocated within arena, so it
            // is only safe to clear the arena now.
            current_arena.clear();
            self.arenas.borrow_mut().push(current_arena);
        }
        drop(file_paths);
        let grug_files = Files {
            inner: unsafe {
                std::mem::transmute::<OwnPtr<[FileInfo]>, OwnPtr<'static, [FileInfo]>>(
                    grug_files.into_boxed_slice().into(),
                )
            },
            _arena: arena,
        };

        // Resource paths are allocated into their own arena, since they
        // don't need to live as long as (and aren't related to) the
        // arena backing `grug_files` above.
        let resource_arena = Arena::new();
        let resource_paths: std::vec::Vec<NTOsStrPtr<'_>> = updated_resources
            .iter()
            .map(|path| resource_arena.copy_osstr_into_nt(path).as_ntosstrptr())
            .collect();
        let resource_paths = ResourcePaths {
            inner: unsafe {
                std::mem::transmute::<OwnPtr<[NTOsStrPtr]>, OwnPtr<'static, [NTOsStrPtr<'static>]>>(
                    resource_paths.into_boxed_slice().into(),
                )
            },
            _arena: resource_arena,
        };

        (resource_paths, grug_files)
    }

    /// Merge threaded compilation and standalone compilation
    fn compile_inner<'arena>(
        path: &'arena OsStr,
        file_text: &'arena NTStr,
        mods_dir_path: &'arena OsStr,
        mod_api: &'arena ModApi,
        arena: &'arena Arena,
        temp_arena: &'_ Arena,
        type_storage: &mut TypeStorage,
    ) -> Result<GrugAst<'arena>, Error> {
        let mod_name = get_mod_name(path);
        let entity_type = get_entity_type(path)?;

        if file_text.is_empty() {
            return Err(Error::new(
                ErrorKind::EMPTY_FILE,
                "",
                path,
                "",
                SourceSpan { offset: 0, line: 1 },
                format_args!("File is empty"),
            ));
        }

        // tokenize
        let tokens = tokenizer::tokenize(file_text, arena, path)?;
        // parse
        let ast = parser::parse(tokens.leak(), arena, file_text, path)?;

        // get mod api entity declaration
        let entity = mod_api.classes().get(entity_type).ok_or_else(||
			// TODO: This is not handled by grug_tests
			Error::new(
				ErrorKind::FILE_NAME_ERROR,
				"",
				path,
				"",
				SourceSpan{offset: 0, line: 0},
				format_args!("Entity '{}' is not registered in the mod_api.json", entity_type),
			))?;
        let Some(export_fns) = &entity.export_fns else {
            return Err(Error::new(
                ErrorKind::FILE_NAME_ERROR,
                "",
                path,
                "",
                SourceSpan { offset: 0, line: 0 },
                format_args!("'{}' is a class, not an entity", entity_type),
            ));
        };

        // type check
        let ast = TypePropagator::fill_result_types(
            export_fns,
            mod_api,
            mod_name,
            mods_dir_path,
            file_text,
            path,
            entity_type,
            ast,
            arena,
            temp_arena,
            type_storage,
        )?;

        // convert into GrugAst
        let mut member_variables = Vec::new_in(arena);
        let mut on_functions = Vec::new_in(arena);
        on_functions.extend((0..export_fns.len()).map(|_| None));
        let mut helper_functions = Vec::new_in(arena);

        ast.global_statements
            .into_iter()
            .for_each(|statement| match statement {
                GlobalStatement::Variable(st) => member_variables.push(st),
                GlobalStatement::OnFunction(st) => {
                    let (i, _) = export_fns
                        .iter()
                        .enumerate()
                        .find(|(_, (name, _))| name.as_str() == st.name.to_str())
                        .expect("export function has already been verified to exist");
                    on_functions[i] = Some(&*arena.alloc_into(st));
                }
                GlobalStatement::HelperFunction(st) => helper_functions.push(st),
                _ => (),
            });

        // SAFETY: copy_bytes_into_nt ensures the slice has a single null byte
        // at the end of the string
        let file_path = arena.copy_osstr_into_nt(path).as_ntosstrptr();
        let file = GrugAst {
            members: member_variables.leak(),
            on_functions: on_functions.leak(),
            helper_functions: helper_functions.leak(),
            file_text: file_text.as_ntstrptr(),
            file_path,
        };
        Ok(file)
    }
}

/// A top level statement in a grug file.
///
/// This is not passed through [`GrugAst`] but is instead supposed to be used
/// internally by a grug state implementation
#[derive(Debug)]
pub(crate) enum GlobalStatement<'a> {
    /// A member variable
    /// `x: number = 25`
    Variable(MemberVariable<'a>),
    /// An on function declaration
    /// ```text
    /// on_init(id: number) {
    ///     set_max_health(50)
    ///     set_unarmed_damage(2)
    ///     set_weapon("sword.json")
    /// }
    /// ```
    OnFunction(OnFunction<'a>),
    /// A helper function declaration
    /// ```text
    /// helper_color(n: number) Color {
    ///     if n == 0 {
    ///         return color("blue")
    ///     } else if n == 1 {
    ///         return color("red")
    ///     } else if n == 2 {
    ///         return color("green")
    ///     } else if n == 3 {
    ///         return color("yellow")
    ///     } else if n == 3 {
    ///         return color("black")
    ///     }
    ///     return game_fn_error("invalid color id")
    /// }
    /// ```
    HelperFunction(HelperFunction<'a>),
    /// A comment at the top level of a file
    /// ```text
    /// ## This is a global comment
    /// shared_number: number = 0
    /// ```
    Comment { value: NTStrPtr<'a> },
    /// An Empty line at the top level of a script
    EmptyLine,
}

/// The mod of a path relative to the mods directory: its first directory
/// component, or the empty string for a path with no directory component.
///
/// This is what [`FileInfo::mod_name`] and [`crate::capi`]'s `grug_file_info`
/// report, and what a host names in [`CompileSettings`].
pub fn get_mod_name(path: &OsStr) -> &OsStr {
    let path = path.as_encoded_bytes();
    let mut slash_len = 0;
    for (i, ch) in path.iter().enumerate() {
        if *ch == b'/' || *ch == b'\\' {
            slash_len = i;
            break;
        }
    }
    // SAFETY: Next byte is b'/' which is valid utf8 or the length is 0.
    unsafe { OsStr::from_encoded_bytes_unchecked(&path[..slash_len]) }

    // This restrict isn't checked in grug_tests and it gets in the way of
    // implementing the compiler in the simplest way
    // path.split_once('/').map(|x| x.0).ok_or(GrugError::FileError(FileError::FilePathDoesNotContainForwardSlash{path: String::from(path)}))
}

/// The filename of a grug script should be as follows <name>-<entity>.grug
///
/// Where <name> can be any utf8 string, and <entity> refers to the entity type
/// the file contains.
///
/// This function returns <entity> for a given file path (if <entity> exists)
fn get_entity_type(path: &OsStr) -> Result<&str, Error> {
    let mut dot_pos = None;
    let mut dash_pos = None;
    let path_bytes = path.as_encoded_bytes();
    let file_name = <OsStr as AsRef<Path>>::as_ref(path)
        .file_name()
        .unwrap_or("".as_ref());
    for (i, ch) in path_bytes.iter().enumerate().rev() {
        match (ch, dot_pos, dash_pos) {
            (b'.', None, None) => dot_pos = Some(i),
            (b'-', None, None) => {
                return Err(Error::new(
                    ErrorKind::FILE_NAME_ERROR,
                    "",
                    path,
                    "",
                    SourceSpan { offset: 0, line: 0 },
                    format_args!("'{}' is missing a period in its name", file_name.display()),
                ));
            }
            (b'-', Some(_), None) => {
                dash_pos = Some(i);
                break;
            }
            _ => (),
        }
    }
    let (dot_pos, dash_pos) = match (dot_pos, dash_pos) {
        (Some(dot_pos), Some(dash_pos)) if dot_pos == dash_pos + 1 => {
            return Err(Error::new(
                ErrorKind::FILE_NAME_ERROR,
                "",
                path,
                "",
                SourceSpan { offset: 0, line: 0 },
                format_args!(
                    "'{}' is missing an entity type in its name",
                    file_name.display()
                ),
            ));
        }
        (Some(dot_pos), Some(dash_pos)) => (dot_pos, dash_pos),
        _ => {
            return Err(Error::new(
                ErrorKind::FILE_NAME_ERROR,
                "",
                path,
                "",
                SourceSpan { offset: 0, line: 0 },
                format_args!(
                    "'{}' is missing an entity type in its name",
                    file_name.display()
                ),
            ));
        }
    };
    // SAFETY: dash_pos is b'-' which is valid utf8, and dot_pos is b'.' which
    // is also utf8 so (dash_pos+1)..dot_pos will not truncate a utf8 codepoint
    let entity_type =
        unsafe { OsStr::from_encoded_bytes_unchecked(&path_bytes[(dash_pos + 1)..dot_pos]) };
    if entity_type.len() > MAX_FILE_ENTITY_TYPE_LENGTH {
        return Err(Error::new(
            ErrorKind::FILE_NAME_ERROR,
            "",
            path,
            "",
            SourceSpan { offset: 0, line: 0 },
            format_args!(
                "There are more than {} characters \n\
				in the entity type of '{}', exceeding MAX_FILE_ENTITY_TYPE_LENGTH",
                entity_type.len(),
                path.display()
            ),
        ));
    }
    check_custom_id_is_pascal(entity_type, path)
}

/// Entities and Id types in grug must be pascalCase.
fn check_custom_id_is_pascal<'a>(
    entity_type: &'a OsStr,
    path: &'_ OsStr,
) -> Result<&'a str, Error> {
    let entity_type = entity_type.to_str().ok_or_else(|| {
        Error::new(
            ErrorKind::FILE_NAME_ERROR,
            "",
            entity_type,
            "",
            SourceSpan { offset: 0, line: 0 },
            format_args!("'{}' is not valid utf8", entity_type.display()),
        )
    })?;
    let mut chars = entity_type.chars();
    if let Some(first) = chars.next()
        && !first.is_uppercase()
    {
        return Err(Error::new(
            ErrorKind::FILE_NAME_ERROR,
            "",
            path,
            "",
            SourceSpan { offset: 0, line: 0 },
            format_args!(
                "'{entity_type}' seems like a custom ID type, but it doesn't start in Uppercase"
            ),
        ));
    }
    for ch in chars {
        if !(ch.is_uppercase() || ch.is_lowercase() || ch.is_ascii_digit()) {
            return Err(Error::new(
                ErrorKind::FILE_NAME_ERROR,
                "",
                path,
                "",
                SourceSpan { offset: 0, line: 0 },
                format_args!(
                    "'{entity_type}' seems like a custom ID type, but it contains '{ch}', which isn't uppercase, lowercase, or a digit",
                ),
            ));
        }
    }
    Ok(entity_type)
}
