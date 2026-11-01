pub use inner::*;
use std::ffi::{OsStr, OsString};

/// Whether an IO error says the path is gone, which a walk racing with a deletion sees and has to
/// treat as the deletion rather than as a failure of the walk.
#[allow(unused)]
fn vanished_while_listing(err: &std::io::Error) -> bool {
    err.kind() == std::io::ErrorKind::NotFound
}

#[allow(unused)]
pub fn poll_watch_changes(
    mods_dir: impl AsRef<OsStr>,
    poll_interval: std::time::Duration,
    mut f: impl FnMut(Result<OsString, std::io::Error>) -> bool + Send + 'static,
) -> Result<(), std::io::Error> {
    use std::collections::HashMap;
    let mods_dir = OsString::from(mods_dir.as_ref());
    let mods_dir_len = if mods_dir
        .as_encoded_bytes()
        .last()
        .is_some_and(|x| *x != b'\\' && *x != b'/')
    {
        mods_dir.len() + 1
    } else {
        mods_dir.len()
    };

    let mut files = HashMap::new();

    let mut dirs_to_check = vec![mods_dir.clone()];

    while let Some(dir) = dirs_to_check.pop() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            let metadata = entry.metadata()?;
            if metadata.file_type().is_dir() {
                dirs_to_check.push(path.clone().into_os_string());
            }
            files.insert(path, metadata.modified()?);
        }
    }
    std::thread::spawn(move || {
        fn is_newer_than(first: std::time::SystemTime, second: std::time::SystemTime) -> bool {
            matches!(first.duration_since(second), Ok(diff) if diff != std::time::Duration::ZERO)
        }
        loop {
            // Replacement for try blocks
            match (|| -> Result<(), std::io::Error> {
                let mut dirs_to_check = vec![mods_dir.clone()];
                while let Some(dir) = dirs_to_check.pop() {
                    let entries = match std::fs::read_dir(&dir) {
                        Ok(entries) => entries,
                        // A subdirectory its owner deleted after this walk listed it is a deletion,
                        // which this walk reports as the change to the parent's mtime. The watched
                        // directory itself was listed by nobody, so failing to read it is an error.
                        Err(err) if dir != mods_dir && vanished_while_listing(&err) => continue,
                        Err(err) => return Err(err),
                    };
                    for entry in entries {
                        let entry = entry?;
                        let entry_path = entry.path();
                        let metadata = match entry.metadata() {
                            Ok(metadata) => metadata,
                            // The entry was deleted after this directory was listed. It is a
                            // deletion like any other, and reporting it as an error made the host
                            // abort the whole process (#675).
                            Err(err) if vanished_while_listing(&err) => continue,
                            Err(err) => return Err(err),
                        };
                        let m_time = metadata.modified()?;
                        let is_dir = metadata.file_type().is_dir();
                        if is_dir {
                            dirs_to_check.push(entry_path.clone().into_os_string());
                        }

                        match files.get_mut(&entry_path) {
                            Some(old_m_time) if is_newer_than(m_time, *old_m_time) => {
                                let rel_path =
                                    &entry_path.as_os_str().as_encoded_bytes()[mods_dir_len..];
                                let rel_path =
                                    unsafe { OsStr::from_encoded_bytes_unchecked(rel_path) };
                                if !f(Ok(OsString::from(rel_path))) {
                                    return Ok(());
                                };
                                *old_m_time = m_time;
                            }
                            None => {
                                // A brand new file that appears while we're
                                // already watching is reported directly,
                                // since this is the only way the host can
                                // discover it exists at all: unlike a
                                // modification, there's no prior baseline
                                // to compare against. Directories are
                                // exempt, since their new leaf descendants
                                // are already individually reported by this
                                // same recursive walk; reporting the
                                // directory too would just be redundant
                                // noise on top of that.
                                if !is_dir {
                                    let rel_path =
                                        &entry_path.as_os_str().as_encoded_bytes()[mods_dir_len..];
                                    let rel_path =
                                        unsafe { OsStr::from_encoded_bytes_unchecked(rel_path) };
                                    if !f(Ok(OsString::from(rel_path))) {
                                        return Ok(());
                                    };
                                }
                                files.insert(entry_path, m_time);
                            }
                            _ => (),
                        }
                    }
                }

                Ok(())
            })() {
                Ok(()) => (),
                Err(err) => {
                    if !f(Err(err)) {
                        return;
                    }
                }
            }

            std::thread::sleep(poll_interval);
        }
    });
    Ok(())
}

#[cfg(target_os = "windows")]
mod inner {
    use crate::pal::windows::*;
    use std::ffi::{OsStr, OsString};
    use std::mem::MaybeUninit;
    use std::os::windows::ffi::OsStringExt;

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn ReadDirectoryChangesW(
            directory: HANDLE,
            buffer: *mut MaybeUninit<u32>,
            buffer_len: DWORD,
            watch_subtree: BOOL,
            notify_filter: DWORD,
            bytes_returned: Option<&mut DWORD>,
            overlapped: Option<&mut Overlapped>,
            completion_routine: Option<OverlappedCompletionRoutine>,
        ) -> BOOL;
    }
    type OverlappedCompletionRoutine = extern "C" fn(DWORD, DWORD, &mut Overlapped);

    struct NotifyFilter;
    impl NotifyFilter {
        const CHANGE_FILE_NAME: DWORD = 0x00000001;
        const CHANGE_DIR_NAME: DWORD = 0x00000002;
        // const CHANGE_ATTRIBUTES : DWORD = 0x00000004;
        // const CHANGE_SIZE       : DWORD = 0x00000008;
        const CHANGE_LAST_WRITE: DWORD = 0x00000010;
        // const CHANGE_LAST_ACCESS: DWORD = 0x00000020;
        // const CHANGE_CREATION   : DWORD = 0x00000040;
        // const CHANGE_SECURITY   : DWORD = 0x00000100;
    }

    #[repr(C)]
    struct Overlapped {
        internal: ULONG_PTR,
        internal_high: ULONG_PTR,
        offset: [DWORD; 2],
        event: HANDLE,
    }

    #[repr(u32)]
    #[derive(Debug)]
    pub enum FileAction {
        Added = 0x1,
        Removed = 0x2,
        Modified = 0x3,
        RenamedOldName = 0x4,
        RenamedNewName = 0x5,
    }

    #[repr(C)]
    pub struct FileNotifyInformation {
        next_entry_offset: DWORD,
        pub action: FileAction,
        file_name_len: DWORD,
        file_name: [u16],
    }

    impl std::fmt::Debug for FileNotifyInformation {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.debug_struct("FileNotifyInformation")
                .field("action", &self.action)
                .field("file_name", &self.file_name())
                .finish()
        }
    }

    impl FileNotifyInformation {
        pub fn file_name(&self) -> OsString {
            OsString::from_wide(&self.file_name)
        }
    }

    pub struct DirChanges {
        buffer: Box<[MaybeUninit<DWORD>]>,
    }
    unsafe impl Send for DirChanges {}
    unsafe impl Sync for DirChanges {}

    impl DirChanges {
        /// SAFETY: buffer must be initialized with a call to ReadDirectoryChangesW
        unsafe fn new(buffer: Box<[MaybeUninit<u32>]>) -> Self {
            Self { buffer }
        }

        fn iter(&self) -> DirChangesIter<'_> {
            self.into_iter()
        }
    }

    impl<'a> IntoIterator for &'a DirChanges {
        type IntoIter = DirChangesIter<'a>;
        type Item = &'a FileNotifyInformation;
        fn into_iter(self) -> Self::IntoIter {
            DirChangesIter {
                _buffer: &self.buffer,
                current: self.buffer.as_ptr().cast::<u32>(),
            }
        }
    }

    pub struct DirChangesIter<'a> {
        _buffer: &'a [MaybeUninit<DWORD>],
        current: *const DWORD,
    }

    impl<'a> Iterator for DirChangesIter<'a> {
        type Item = &'a FileNotifyInformation;
        fn next(&mut self) -> Option<Self::Item> {
            use std::ptr;
            if self.current.is_null() {
                return None;
            }

            // SAFETY:
            // 	alignment: FileNotifyInformation is 4 byte aligned and
            // 	ReadDirectoryChangesW guarantees FileNotifyInformation is aligned
            // 	to 4 bytes
            // 	len: len of 0 is always valid for slices
            let current = unsafe {
                &*(ptr::slice_from_raw_parts(
                    self.current,
                    0,
                ) as *const FileNotifyInformation)
            };
            let next_offset = current.next_entry_offset as usize;
            // SAFETY:
            // 	alignment: FileNotifyInformation is 4 byte aligned and
            // 	ReadDirectoryChangesW guarantees FileNotifyInformation is aligned
            // 	to 4 bytes
            // 	len: len is guaranteed by ReadDirectoryChangesW
            let current = unsafe {
                &*(ptr::slice_from_raw_parts(
                    self.current,
                    current.file_name_len as usize / 2,
                ) as *const FileNotifyInformation)
            };

            self.current = if next_offset == 0 {
                std::ptr::null()
            } else {
                unsafe { self.current.byte_add(next_offset) }
            };
            Some(current)
        }
    }

    /// SAFETY: path should be null terminated
    unsafe fn open_dir(path: &[u8]) -> Result<OwnedHandle, std::io::Error> {
        let handle = unsafe {
            CreateFileA(
                path.as_ptr(),
                AccessMask::GENERIC_READ,
                ShareMode::FILE_SHARE_READ,
                None,
                CreateDisposition::OPEN_EXISTING,
                FlagsAndAttributes::FILE_ATTRIBUTE_NORMAL
                    | FlagsAndAttributes::FILE_FLAG_OVERLAPPED
                    | FlagsAndAttributes::FILE_FLAG_BACKUP_SEMANTICS,
                std::ptr::null_mut(),
            )
        };
        if handle == INVALID_HANDLE_VALUE {
            return Err(std::io::Error::last_os_error());
        }
        // SAFETY: CreateFileA with FILE_FLAG_BACKUP_SEMANTICS flag creates a directory handle
        Ok(unsafe { OwnedHandle::new(handle) })
    }

    fn read_changes(handle: &OwnedHandle) -> Result<DirChanges, std::io::Error> {
        use std::mem::size_of;
        let mut buffer = Vec::with_capacity(1024 * 16);
        loop {
            let mut bytes_returned = 0;
            let ret_val = unsafe {
                ReadDirectoryChangesW(
                    handle.0,
                    buffer.as_mut_ptr(),
                    (buffer.capacity() * size_of::<DWORD>()) as u32,
                    TRUE,
                    NotifyFilter::CHANGE_FILE_NAME
                        | NotifyFilter::CHANGE_DIR_NAME
                        | NotifyFilter::CHANGE_LAST_WRITE,
                    Some(&mut bytes_returned),
                    None,
                    None,
                )
            };
            if ret_val == 0 {
                return Err(std::io::Error::last_os_error());
            }
            if bytes_returned != 0 {
                unsafe { buffer.set_len((bytes_returned as usize - 1) / size_of::<DWORD>() + 1) };
                let buffer: Box<[MaybeUninit<DWORD>]> = buffer.into();
                return Ok(unsafe { DirChanges::new(buffer) });
            }
            buffer.reserve(buffer.len())
        }
    }

    pub fn watch_changes(
        path: impl AsRef<OsStr>,
        poll_interval: std::time::Duration,
        mut f: impl FnMut(Result<OsString, std::io::Error>) -> bool + Send + 'static,
    ) -> Result<(), std::io::Error> {
        // Windows uses ReadDirectoryChangesW, a native OS-level change
        // notification API, instead of polling, so there's no interval to
        // configure here.
        let _ = poll_interval;
        let mut path = Vec::from(path.as_ref().as_encoded_bytes());
        path.push(b'\0');
        let handle = unsafe { open_dir(&path)? };
        std::thread::spawn(move || {
            loop {
                match read_changes(&handle) {
                    Ok(changes) => {
                        for change in changes.iter() {
                            if !f(Ok(change.file_name())) {
                                return;
                            }
                        }
                    }
                    Err(err) => {
                        if !f(Err(err)) {
                            return;
                        }
                    }
                }
            }
        });
        Ok(())
    }
}

#[cfg(target_os = "linux")]
mod inner {
    use std::ffi::{OsStr, OsString};
    pub fn watch_changes(
        mods_dir: impl AsRef<OsStr>,
        poll_interval: std::time::Duration,
        f: impl FnMut(Result<OsString, std::io::Error>) -> bool + Send + 'static,
    ) -> Result<(), std::io::Error> {
        super::poll_watch_changes(mods_dir, poll_interval, f)
    }
}

#[cfg(all(test, target_os = "linux"))]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;
    use std::path::{Path, PathBuf};
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::mpsc::{Receiver, Sender, channel};
    use std::sync::{Arc, Mutex};
    use std::time::Duration;

    /// How long a test waits for the poller before deciding the change it needs never happened.
    const TIMEOUT: Duration = Duration::from_secs(10);

    /// How many files a round holds besides the victims. The whole round has to fit in one read of
    /// the directory, so that the walk lists the victims before it can be told to delete them.
    const ROUND_FILES: usize = 60;

    /// How many files the callback deletes while the walk inspects the round. A single victim is
    /// enough on a filesystem that lists a directory in the order its entries were created, like
    /// tmpfs, where the victim created first is listed last, but another order is free to list every
    /// victim before the walk reports a file, and four of the round's sixty-four entries makes that
    /// practically certain not to happen.
    const VICTIMS: usize = 4;

    /// A watched directory, and an unwatched one beside it where a test builds a subtree before
    /// moving it in with one rename, so a walk sees the subtree either not at all or all at once.
    struct Scratch {
        watched: PathBuf,
        staging: PathBuf,
    }

    impl Scratch {
        fn new(name: &str) -> Self {
            let root = std::env::temp_dir().join(format!(
                "grug-watcher-{}-{name}",
                std::process::id()
            ));
            let _ = std::fs::remove_dir_all(&root);
            let watched = root.join("watched");
            let staging = root.join("staging");
            std::fs::create_dir_all(&watched).unwrap();
            std::fs::create_dir_all(&staging).unwrap();
            Self { watched, staging }
        }

        /// Builds a round of [`ROUND_FILES`] files and [`VICTIMS`] victims, outside the watched
        /// directory.
        ///
        /// A filesystem that lists a directory in the order its entries were created, like tmpfs,
        /// lists the victims first and so last, which leaves the walk the whole round to report
        /// before it inspects one of them.
        fn build_round(&self, round: &str) {
            let built = self.staging.join(round);
            std::fs::create_dir(&built).unwrap();
            for victim in 0..VICTIMS {
                std::fs::write(built.join(victim_name(victim)), "").unwrap();
            }
            for file in 0..ROUND_FILES {
                std::fs::write(built.join(format!("file{file:03}")), "").unwrap();
            }
        }

        /// Moves the round built by [`Self::build_round`] into the watched directory.
        fn move_in(&self, round: &str) {
            std::fs::rename(self.staging.join(round), self.watched.join(round)).unwrap();
        }
    }

    impl Drop for Scratch {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(self.watched.parent().unwrap());
        }
    }

    /// The name of the `index`th file of a round that its test deletes during the walk.
    fn victim_name(index: usize) -> String {
        format!("victim{index}")
    }

    /// Whether `name` is one of a round's victims.
    fn is_victim(name: &OsStr) -> bool {
        (0..VICTIMS).any(|index| name == OsStr::new(&victim_name(index)))
    }

    /// What a walk reported to a test.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    enum Event {
        /// The walk listed and inspected every file of the round, so none of them was an error.
        RoundWalked,
        /// The walk stopped on an IO error.
        Errored(std::io::ErrorKind),
    }

    /// Starts a poller on the scratch directory, reporting each change to `on_change` and the
    /// round's completion to `events`.
    ///
    /// The callback answers a change with true, which keeps the poller walking: answering false only
    /// ends one walk, and only an error answered with false ends its thread. `stop` uses that.
    fn start(
        scratch: &Scratch,
        round: &'static str,
        events: &Sender<Event>,
        mut on_change: impl FnMut(&Path) + Send + 'static,
    ) -> (Arc<Mutex<Vec<std::io::ErrorKind>>>, Receiver<()>) {
        let errors = Arc::new(Mutex::new(Vec::new()));
        let (stopped, thread_stopped) = channel();
        let errors_for_poller = Arc::clone(&errors);
        let events = events.clone();
        let mut walked = 0;
        poll_watch_changes(
            scratch.watched.as_os_str(),
            Duration::from_millis(5),
            move |change| {
                match change {
                    Ok(path) => {
                        let path = PathBuf::from(path);
                        if path.parent() == Some(Path::new(round))
                            && path.file_name().is_some_and(|name| !is_victim(name))
                        {
                            walked += 1;
                            if walked == ROUND_FILES {
                                let _ = events.send(Event::RoundWalked);
                            }
                        }
                        on_change(&path);
                    }
                    Err(err) => {
                        errors_for_poller.lock().unwrap().push(err.kind());
                        let _ = events.send(Event::Errored(err.kind()));
                        let _ = stopped.send(());
                        return false;
                    }
                }
                true
            },
        )
        .unwrap();
        (errors, thread_stopped)
    }

    /// Makes the poller report an error it has to keep reporting, and waits for its thread to end.
    ///
    /// A directory the poller cannot read is an IO error like any other, so it tells a test that the
    /// walk still reports what it cannot read, and it is the one error that ends the thread.
    fn stop(
        scratch: &Scratch,
        errors: &Arc<Mutex<Vec<std::io::ErrorKind>>>,
        stopped: &Receiver<()>,
    ) -> Vec<std::io::ErrorKind> {
        let locked = scratch.watched.join("locked");
        std::fs::create_dir(&locked).unwrap();
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();
        stopped
            .recv_timeout(TIMEOUT)
            .expect("the poller never reported the unreadable directory");
        // Put the mode back, so the scratch directory can be removed.
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o700)).unwrap();
        errors.lock().unwrap().clone()
    }

    /// A file deleted between being listed and being inspected is a deletion, not an IO error.
    ///
    /// A walk reads a directory's entries in one go and inspects them one by one, so the file's owner
    /// can delete it before its turn comes. Reporting that race as an error is what aborted the JVM
    /// during coverage/tests/hot_reload_recipe_removed-Test.grug (#675); the deletion itself reaches
    /// the host as the change to the file's parent directory.
    #[test]
    fn a_file_deleted_during_a_walk_is_not_an_io_error() {
        let scratch = Scratch::new("deleted-file");
        scratch.build_round("round");
        let removed = Arc::new(AtomicBool::new(false));
        let removed_in_poller = Arc::clone(&removed);
        let round_in_poller = scratch.watched.join("round");
        let (events, event) = channel();
        let (errors, stopped) = start(&scratch, "round", &events, move |path| {
            // The first file the walk reports leaves the victims' inspection still to come, since
            // the walk listed the whole round before it inspected any of it.
            let Some(directory) = path.parent() else {
                return;
            };
            if directory != Path::new("round")
                || path.file_name().is_some_and(is_victim)
                || removed_in_poller.load(Ordering::Relaxed)
            {
                return;
            }
            for victim in 0..VICTIMS {
                if std::fs::remove_file(round_in_poller.join(victim_name(victim))).is_ok() {
                    removed_in_poller.store(true, Ordering::Relaxed);
                }
            }
        });
        scratch.move_in("round");

        match event.recv_timeout(TIMEOUT) {
            Ok(Event::RoundWalked) => {}
            Ok(Event::Errored(kind)) => panic!(
                "the walk reported {kind:?} for a file deleted while it was being listed"
            ),
            Err(_) => panic!("the walk never listed the round"),
        }
        assert!(
            removed.load(Ordering::Relaxed),
            "the walk never reported the round's first file, so nothing raced"
        );
        let reported = stop(&scratch, &errors, &stopped);
        assert_eq!(
            reported,
            vec![std::io::ErrorKind::PermissionDenied],
            "the walk reported an error other than the unreadable directory"
        );
    }

    /// A directory deleted between being listed and being walked is a deletion too, not an IO error.
    ///
    /// A walk lists a directory's subdirectories and walks each of them only once it has the whole
    /// listing, so the owner of a subtree can delete a directory that the walk has already seen.
    #[test]
    fn a_directory_deleted_during_a_walk_is_not_an_io_error() {
        let scratch = Scratch::new("deleted-directory");
        let built = scratch.staging.join("round");
        // Two directories hold a file, which the walk reports while it walks the first of them, and
        // the third stays empty. However the walk orders the three, the report comes from one of the
        // first two, so at least one of the directories the callback deletes is still unvisited.
        for name in ["alpha", "beta"] {
            std::fs::create_dir_all(built.join(name)).unwrap();
            std::fs::write(built.join(name).join("file"), "").unwrap();
        }
        std::fs::create_dir(built.join("gamma")).unwrap();

        let removed = Arc::new(AtomicBool::new(false));
        let removed_in_poller = Arc::clone(&removed);
        let round_in_poller = scratch.watched.join("round");
        let (deleted, deletion) = channel();
        let (events, _event) = channel();
        let (errors, stopped) = start(&scratch, "round", &events, move |path| {
            let Some(directory) = path.parent() else {
                return;
            };
            if directory.parent() != Some(Path::new("round"))
                || removed_in_poller.load(Ordering::Relaxed)
            {
                return;
            }
            for name in ["alpha", "beta", "gamma"] {
                if directory.file_name() == Some(OsStr::new(name)) {
                    continue;
                }
                if std::fs::remove_dir_all(round_in_poller.join(name)).is_ok() {
                    removed_in_poller.store(true, Ordering::Relaxed);
                    let _ = deleted.send(());
                }
            }
        });
        scratch.move_in("round");

        deletion
            .recv_timeout(TIMEOUT)
            .expect("the walk never reported a file in the round, so nothing was deleted");
        let reported = stop(&scratch, &errors, &stopped);
        assert_eq!(
            reported,
            vec![std::io::ErrorKind::PermissionDenied],
            "a walk reported an error for a directory deleted while it was being listed"
        );
    }
}
