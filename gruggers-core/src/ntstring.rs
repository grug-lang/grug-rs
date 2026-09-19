//! This module exports the [`NTStr`] and [`NTStrPtr`] types which represent
//! null terminated utf8 strings.
//!
//! This is allow simpler interfacing with c apis. [`NTStrPtr`] can be used in
//! FFI in place of a non-null `const char*` c string. It is also
//! `#[repr(transparent)]`, so [`Option<NTStrPtr>`] can be used in place of a
//! `const char*` c string.
//!
//! ## Why not just use [`CStr`]?
//!
//! [`CStr`] is a wide pointer so it cannot be used directly in FFI and it requires
//! unsafe parsing at the boundary. It also cannot be easily displayed without
//! converting into a string
//!
//! ## Won't that cause UB if you are given a string that's not null terminated.
//! Yeah, it will. Don't do that. But because there's no way to verify
//! that at runtime, parsing into a [`CStr`] will also cause UB
//!
//! On the other hand, it is also UB if the [`NTStrPtr`] points to non-utf8 data.
//! [`CStr`] will catch this when converting into a [prim@`str`]. This is a
//! limitation of these types to be aware of.

use std::borrow::Borrow;
use std::ffi::{CStr, c_char};
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::Arc;

use allocator_api2::alloc::Allocator;
use allocator_api2::boxed::Box;

pub use str::*;
mod str {
    use super::*;
    /// Represents a utf-8 string with a single null byte at the end.
    #[repr(transparent)]
    pub struct NTStr(str);

    /// Indicates an error that can occur when converting a [`prim@str`] into an [`NTStr`]
    #[derive(Debug, Clone, Copy)]
    pub enum NTStrError {
        /// A null byte was found in the middle of the string
        UnexpectedNullByte {
            /// The byte offset of the null byte
            location: usize,
        },
        /// The null terminator was missing from the input
        MissingNullTerminator,
    }

    /// # SAFETY
    ///
    /// Same as str
    unsafe impl Send for NTStr {}
    /// # SAFETY
    ///
    /// Same as str
    unsafe impl Sync for NTStr {}

    impl NTStr {
        /// Copy `value` into a `Arc<NTStr>`
        ///
        /// # Panics
        ///
        /// if `value` contains a null byte
        pub fn arc_from_str(value: &str) -> Arc<Self> {
            assert!(!value.contains('\0'));
            let arc = Arc::into_raw(Arc::<[u8]>::new_uninit_slice(value.len() + 1));

            unsafe {
                std::ptr::copy(value.as_ptr(), arc.cast_mut().cast(), value.len());
                arc.cast_mut().cast::<u8>().add(value.len()).write(b'\0');
                std::mem::transmute(Arc::from_raw(arc).assume_init())
            }
        }

        /// Copy `value` into a `Box<NTStr>` in allocator `a`.
        ///
        /// # Panics
        ///
        /// if `value` contains a null byte
        // TODO: remove this
        pub fn box_from_str_in<A: Allocator>(value: &str, a: A) -> Box<Self, A> {
            assert!(!value.contains('\0'));
            let (ptr, a) = Box::into_raw_with_allocator(Box::<[u8], _>::new_uninit_slice_in(
                value.len() + 1,
                a,
            ));

            unsafe {
                std::ptr::copy(value.as_ptr(), ptr.cast(), value.len());
                ptr.cast::<u8>().add(value.len()).write(b'\0');
                let ptr = std::mem::transmute::<*mut [MaybeUninit<u8>], *mut NTStr>(ptr);
                Box::from_raw_in(ptr, a)
            }
        }

        /// Equivalent to `self.is_len == 0`
        pub const fn is_empty(&self) -> bool {
            self.len() == 0
        }

        /// Returns the number of bytes until the null byte is encountered
        ///
        /// Equivalent of `strlen` in the c standard library
        pub const fn len(&self) -> usize {
            self.0.len() - 1
        }

        /// Converts self into a [`prim@str`] excluding the null terminator
        pub fn as_str(&self) -> &str {
            &self.0[..(self.0.len() - 1)]
        }

        /// Converts self into a [`prim@str`] including the null terminator
        pub const fn as_str_with_null(&self) -> &str {
            &self.0
        }

        /// # SAFETY
        ///
        /// The last byte of `value` MUST be a null byte and there must be no other null byte in between
        pub const unsafe fn from_str_unchecked(value: &str) -> &Self {
            unsafe { std::mem::transmute::<&str, &NTStr>(value) }
        }

        /// Tries to converts a str into an NTStr
        pub fn try_from_str(value: &str) -> Result<&NTStr, NTStrError> {
            if let Some(last) = value.as_bytes().last()
                && *last == b'\0'
            {
                for (i, byte) in value.as_bytes()[0..value.len() - 1].iter().enumerate() {
                    if *byte == b'\0' {
                        return Err(NTStrError::UnexpectedNullByte { location: i });
                    }
                }
                // SAFETY: last byte (if it exists) is null
                unsafe { Ok(Self::from_str_unchecked(value)) }
            } else {
                Err(NTStrError::MissingNullTerminator)
            }
        }

        /// Get the string as a [`NTStrPtr`]
        pub const fn as_ntstrptr(&self) -> NTStrPtr<'_> {
            // SAFETY There is a null byte at the self.len()
            unsafe { NTStrPtr::from_ptr(NonNull::from_ref(&self.0).cast::<i8>()) }
        }
    }

    impl std::fmt::Debug for NTStr {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            std::fmt::Debug::fmt(self.as_str(), f)
        }
    }

    impl std::fmt::Display for NTStr {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            self.deref().fmt(f)
        }
    }

    impl<'a> TryFrom<&'a str> for &'a NTStr {
        type Error = NTStrError;
        fn try_from(value: &str) -> Result<&NTStr, Self::Error> {
            NTStr::try_from_str(value)
        }
    }

    impl std::ops::Deref for NTStr {
        type Target = str;
        fn deref(&self) -> &Self::Target {
            self.as_str()
        }
    }

    impl AsRef<str> for NTStr {
        fn as_ref(&self) -> &str {
            &self.0[..(self.0.len() - 1)]
        }
    }

    impl AsRef<CStr> for NTStr {
        fn as_ref(&self) -> &CStr {
            // SAFETY: There is a single null byte at the end
            unsafe { CStr::from_bytes_with_nul_unchecked(self.0.as_bytes()) }
        }
    }

    impl std::hash::Hash for NTStr {
        fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
            self.as_str().hash(hasher)
        }
    }

    impl PartialEq for NTStr {
        fn eq(&self, other: &Self) -> bool {
            self.as_str() == other.as_str()
        }
    }
    impl Eq for NTStr {}

    impl<'a> From<&'a NTStr> for String {
        fn from(other: &'a NTStr) -> String {
            String::from(other.as_str_with_null())
        }
    }

    impl Borrow<str> for &NTStr {
        fn borrow(&self) -> &str {
            self.as_str()
        }
    }

    #[cfg(test)]
    mod test {
        use super::*;
        #[test]
        fn ntstr_test() {
            let str = "hello\0";
            let ntstr = <&NTStr>::try_from(str).unwrap();
            let str2 = &**ntstr;
            assert_eq!(str2, "hello");
        }
    }

    /// Represents a null terminated UTF-8 string as a single pointer (unlike CStr which uses two)
    /// Requires that there is atleast one null byte between the pointer and the end of the buffer it points to
    /// This pointer must be valid to read until the null byte
    #[repr(transparent)]
    #[derive(Clone, Copy)]
    pub struct NTStrPtr<'a>(NonNull<c_char>, PhantomData<&'a ()>);
    const _: () = const {
        assert!(std::mem::size_of::<NTStrPtr>() == std::mem::size_of::<Option<NTStrPtr>>())
    };

    /// # SAFETY
    ///
    /// Same as str
    unsafe impl Send for NTStrPtr<'_> {}
    /// # SAFETY
    ///
    /// Same as str
    unsafe impl Sync for NTStrPtr<'_> {}

    impl<'a> NTStrPtr<'a> {
        /// Returns a raw pointer to the string
        pub fn as_ptr(self) -> *const u8 {
            self.0.cast::<u8>().as_ptr().cast_const()
        }

        /// A version of the len function that can work in const
        pub const fn const_len(self) -> usize {
            let mut len = 0;
            while unsafe { self.0.add(len).read() as u8 } != b'\0' {
                len += 1;
            }
            len
        }

        /// get the length of the string using SIMD intrinsics if available on
        /// the target platform.
        ///
        /// The way the SIMD algorithm works will cause asan errors because
        /// there are out of bounds reads. However, there is no memory safety
        /// concern here because any bits are read from out of bounds are discarded.
        ///
        /// Additionally, the out of bounds reads will never straddle a page
        /// boundary, so there will never be a seg fault caused by this
        /// function
        pub fn len(self) -> usize {
            #[inline]
            fn len_default(val: NTStrPtr) -> usize {
                let mut len = 0;
                while unsafe { val.0.add(len).read() as u8 } != b'\0' {
                    len += 1;
                }
                len
            }
            #[cfg(target_arch = "x86_64")]
            {
                #[inline]
                #[target_feature(enable = "sse2")]
                fn len_sse(val: NTStrPtr) -> usize {
                    use std::arch::x86_64::*;
                    use std::mem::{align_of, size_of};

                    let ptr = val.0.cast::<u8>().as_ptr();
                    const _: () = const {
                        assert!(size_of::<__m128i>() == align_of::<__m128i>());
                    };
                    let diff = size_of::<__m128i>() - ptr.align_offset(align_of::<__m128i>());
                    let ptr = ptr.wrapping_sub(diff).cast::<__m128i>();

                    let zeros: __m128i = _mm_set1_epi8(0);

                    let movemask =
                        _mm_movemask_epi8(_mm_cmpeq_epi8(zeros, unsafe { ptr.read() })) >> diff;
                    if movemask != 0 {
                        return movemask.trailing_zeros() as usize;
                    };

                    let mut offset = 0;

                    loop {
                        offset += 1;
                        let movemask = _mm_movemask_epi8(_mm_cmpeq_epi8(zeros, unsafe {
                            ptr.add(offset).read()
                        }));
                        if movemask != 0 {
                            return offset * size_of::<__m128i>()
                                + movemask.trailing_zeros() as usize
                                - diff;
                        };
                    }
                }

                #[inline]
                #[target_feature(enable = "avx2")]
                fn len_avx2(val: NTStrPtr) -> usize {
                    use std::arch::x86_64::*;
                    use std::mem::{align_of, size_of};

                    let ptr = val.0.cast::<u8>().as_ptr();
                    const _: () = const {
                        assert!(size_of::<__m256i>() == align_of::<__m256i>());
                    };
                    let diff = (ptr.addr()) & (size_of::<__m256i>() - 1);
                    let ptr = ptr.wrapping_sub(diff).cast::<__m256i>();

                    let zeros: __m256i = _mm256_set1_epi8(0);

                    let movemask =
                        _mm256_movemask_epi8(_mm256_cmpeq_epi8(zeros, unsafe { ptr.read() }))
                            >> diff;
                    if movemask != 0 {
                        return movemask.trailing_zeros() as usize;
                    };

                    let mut offset = 0;

                    loop {
                        offset += 1;
                        let movemask = _mm256_movemask_epi8(_mm256_cmpeq_epi8(zeros, unsafe {
                            ptr.add(offset).read()
                        }));
                        if movemask != 0 {
                            return offset * size_of::<__m256i>()
                                + movemask.trailing_zeros() as usize
                                - diff;
                        };
                    }
                }
                // SAFETY: We choose whichever one is available
                if std::arch::is_x86_feature_detected!("avx2") {
                    unsafe { len_avx2(self) }
                } else if std::arch::is_x86_feature_detected!("sse2") {
                    unsafe { len_sse(self) }
                } else {
                    len_default(self)
                }
            }
            #[cfg(not(target_arch = "x86_64"))]
            {
                len_default(self)
            }
        }

        /// returns true if the first byte is null (indicating that there is no data behind the string)
        pub fn is_empty(self) -> bool {
            // SAFETY: NTStrPtr is guaranteed to point to allocated memory that ends in a null byte
            unsafe { self.0.cast::<u8>().read() == b'\0' }
        }

        /// # Safety
        /// `ptr` must point at a buffer that is valid to read until the next
        /// null byte
        /// The memory pointed to by `ptr` must not be modified as long as the
        /// returned string exists
        pub const unsafe fn from_ptr(ptr: NonNull<c_char>) -> Self {
            Self(ptr, PhantomData)
        }

        /// Returns a `&CStr` to the string
        pub fn to_cstr(self) -> &'a CStr {
            unsafe { CStr::from_ptr(self.0.as_ptr().cast_const()) }
        }

        /// Returns a `&NTStr` to the string excluding the null byte
        pub fn to_ntstr(self) -> &'a NTStr {
            let len = self.len();
            // buffer is okay to read upto len = i
            let slice =
                unsafe { std::slice::from_raw_parts(self.0.cast::<u8>().as_ptr(), len + 1) };
            // SAFETY: NTStrPtr points to a utf8 encoded buffer
            let slice = unsafe { std::str::from_utf8_unchecked(slice) };
            // SAFETY: Last byte is null
            unsafe { NTStr::from_str_unchecked(slice) }
        }

        /// Returns a `&str` to the string excluding the null byte
        pub fn to_str(self) -> &'a str {
            self.to_ntstr().as_str()
        }

        /// # SAFETY
        ///
        /// There must be at least one null byte within the str
        pub unsafe fn from_str_unchecked(value: &'a str) -> Self {
            unsafe { Self::from_ptr(NonNull::from_ref(value).cast::<c_char>()) }
        }

        /// Checks if the underlying pointers are equal
        pub fn ptr_eq(self, other: Self) -> bool {
            self.0 == other.0
        }

        /// Expects a single null byte at the end of the string and no null bytes
        /// in the rest of the string
        pub fn try_from_str(value: &'a str) -> Result<Self, NTStrError> {
            Ok(NTStr::try_from_str(value)?.as_ntstrptr())
        }

        /// Returns a pointer with a static lifetime.
        ///
        /// # SAFETY
        ///
        /// It is UB to use the returned pointer after the backing memory is
        /// deallocated or borrowed mutably
        pub unsafe fn detach_lifetime(self) -> NTStrPtr<'static> {
            unsafe { std::mem::transmute::<Self, NTStrPtr<'static>>(self) }
        }
    }

    impl<'a> std::hash::Hash for NTStrPtr<'a> {
        fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
            std::hash::Hash::hash(self.to_str(), hasher)
        }
    }

    impl<'a> std::fmt::Display for NTStrPtr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            self.to_str().fmt(f)
        }
    }

    impl<'a> PartialEq for NTStrPtr<'a> {
        fn eq(&self, other: &Self) -> bool {
            if self.0 == other.0 {
                true
            } else {
                let first = self.to_cstr();
                let other = other.to_cstr();
                first == other
            }
        }
    }

    impl<'a> Eq for NTStrPtr<'a> {}

    impl<'a> From<&'a NTStr> for NTStrPtr<'a> {
        fn from(other: &'a NTStr) -> Self {
            other.as_ntstrptr()
        }
    }

    impl<'a> std::fmt::Debug for NTStrPtr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            std::fmt::Debug::fmt(self.to_str(), f)
        }
    }

    /// Creates an NTStr from a literal
    #[macro_export]
    macro_rules! nt {
        ($lit: literal) => {{
            const {
                let bytes = $lit.as_bytes();
                let mut i = 0;
                while i < bytes.len() {
                    assert!(bytes[i] != b'\0');
                    i += 1;
                }
            };
            unsafe { $crate::ntstring::NTStr::from_str_unchecked(concat!($lit, "\0")) }
        }};
    }
}

pub use osstr::*;
mod osstr {
    use super::NTStrError;
    use super::*;

    use std::ffi::OsStr;

    /// Represents a utf-8 string with a single null byte at the end.
    #[repr(transparent)]
    pub struct NTOsStr(OsStr);

    /// # SAFETY
    ///
    /// Same as OsStr
    unsafe impl Send for NTOsStr {}
    /// # SAFETY
    ///
    /// Same as OsStr
    unsafe impl Sync for NTOsStr {}

    impl NTOsStr {
        /// Equivalent to `self.is_len == 0`
        pub const fn is_empty(&self) -> bool {
            self.len() == 0
        }

        /// Returns the number of bytes until the null byte is encountered
        ///
        /// Equivalent of `strlen` in the c standard library
        pub const fn len(&self) -> usize {
            // AHHHHH OsStr::len is not const
            unsafe { std::mem::transmute::<&OsStr, &[u8]>(&self.0) }.len() - 1
        }

        /// Converts self into a [`OsStr`] excluding the null terminator
        pub fn as_osstr(&self) -> &OsStr {
            // SAFETY: Removing a null byte does not change nutf8 validity
            unsafe {
                OsStr::from_encoded_bytes_unchecked(&self.0.as_encoded_bytes()[..self.0.len() - 1])
            }
        }

        /// Converts self into a [`OsStr`] including the null terminator
        pub const fn as_osstr_with_null(&self) -> &OsStr {
            &self.0
        }

        /// # SAFETY
        ///
        /// The last byte of `value` MUST be a null byte and there must be no other null byte in between
        pub const unsafe fn from_osstr_unchecked(value: &OsStr) -> &Self {
            unsafe { std::mem::transmute::<&OsStr, &NTOsStr>(value) }
        }

        /// Tries to converts a OsStr into an NTOsStr
        pub fn try_from_osstr(value: &OsStr) -> Result<&NTOsStr, NTStrError> {
            if let Some(last) = value.as_encoded_bytes().last()
                && *last == b'\0'
            {
                for (i, byte) in value.as_encoded_bytes()[0..value.len() - 1]
                    .iter()
                    .enumerate()
                {
                    if *byte == b'\0' {
                        return Err(NTStrError::UnexpectedNullByte { location: i });
                    }
                }
                // SAFETY: last byte (if it exists) is null
                unsafe { Ok(Self::from_osstr_unchecked(value)) }
            } else {
                Err(NTStrError::MissingNullTerminator)
            }
        }

        /// Get the string as a [`NTOsStrPtr`]
        pub const fn as_ntosstrptr(&self) -> NTOsStrPtr<'_> {
            // SAFETY There is a null byte at the self.len()
            unsafe { NTOsStrPtr::from_ptr(NonNull::from_ref(&self.0).cast::<i8>()) }
        }

        /// same as [`OsStr::display`]
        pub fn display(&self) -> std::ffi::os_str::Display<'_> {
            self.as_osstr().display()
        }
    }

    impl std::fmt::Debug for NTOsStr {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            std::fmt::Debug::fmt(self.as_osstr(), f)
        }
    }

    impl<'a> TryFrom<&'a OsStr> for &'a NTOsStr {
        type Error = NTStrError;
        fn try_from(value: &OsStr) -> Result<&NTOsStr, Self::Error> {
            NTOsStr::try_from_osstr(value)
        }
    }

    impl std::ops::Deref for NTOsStr {
        type Target = OsStr;
        fn deref(&self) -> &Self::Target {
            self.as_osstr()
        }
    }

    impl AsRef<OsStr> for NTOsStr {
        fn as_ref(&self) -> &OsStr {
            self.as_osstr()
        }
    }

    impl AsRef<CStr> for NTOsStr {
        fn as_ref(&self) -> &CStr {
            // SAFETY: There is a single null byte at the end
            unsafe { CStr::from_bytes_with_nul_unchecked(self.0.as_encoded_bytes()) }
        }
    }

    impl std::hash::Hash for NTOsStr {
        fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
            self.as_osstr().hash(hasher)
        }
    }

    impl PartialEq for NTOsStr {
        fn eq(&self, other: &Self) -> bool {
            self.as_osstr() == other.as_osstr()
        }
    }
    impl Eq for NTOsStr {}

    impl Borrow<OsStr> for &NTOsStr {
        fn borrow(&self) -> &OsStr {
            self.as_osstr()
        }
    }

    /// Represents a null terminated UTF-8 string as a single pointer (unlike CStr which uses two)
    /// Requires that there is atleast one null byte between the pointer and the end of the buffer it points to
    /// This pointer must be valid to read until the null byte
    #[repr(transparent)]
    #[derive(Clone, Copy)]
    pub struct NTOsStrPtr<'a>(NonNull<c_char>, PhantomData<&'a ()>);
    const _: () = const {
        assert!(std::mem::size_of::<NTOsStrPtr>() == std::mem::size_of::<Option<NTOsStrPtr>>())
    };

    /// # SAFETY
    ///
    /// Same as OsStr
    unsafe impl Send for NTOsStrPtr<'_> {}
    /// # SAFETY
    ///
    /// Same as OsStr
    unsafe impl Sync for NTOsStrPtr<'_> {}

    impl<'a> NTOsStrPtr<'a> {
        /// Returns a raw pointer to the string
        pub fn as_ptr(self) -> *const u8 {
            self.0.cast::<u8>().as_ptr().cast_const()
        }

        /// A version of the len function that can work in const
        pub const fn const_len(self) -> usize {
            let mut len = 0;
            while unsafe { self.0.add(len).read() as u8 } != b'\0' {
                len += 1;
            }
            len
        }

        /// get the length of the string using SIMD intrinsics if available on
        /// the target platform.
        ///
        /// The way the SIMD algorithm works will cause asan errors because
        /// there are out of bounds reads. However, there is no memory safety
        /// concern here because any bits are read from out of bounds are discarded.
        ///
        /// Additionally, the out of bounds reads will never straddle a page
        /// boundary, so there will never be a seg fault caused by this
        /// function
        pub fn len(self) -> usize {
            #[inline]
            fn len_default(val: NTOsStrPtr) -> usize {
                let mut len = 0;
                while unsafe { val.0.add(len).read() as u8 } != b'\0' {
                    len += 1;
                }
                len
            }
            #[cfg(target_arch = "x86_64")]
            {
                #[inline]
                #[target_feature(enable = "sse2")]
                fn len_sse(val: NTOsStrPtr) -> usize {
                    use std::arch::x86_64::*;
                    use std::mem::{align_of, size_of};

                    let ptr = val.0.cast::<u8>().as_ptr();
                    const _: () = const {
                        assert!(size_of::<__m128i>() == align_of::<__m128i>());
                    };
                    let diff = size_of::<__m128i>() - ptr.align_offset(align_of::<__m128i>());
                    let ptr = ptr.wrapping_sub(diff).cast::<__m128i>();

                    let zeros: __m128i = _mm_set1_epi8(0);

                    let movemask =
                        _mm_movemask_epi8(_mm_cmpeq_epi8(zeros, unsafe { ptr.read() })) >> diff;
                    if movemask != 0 {
                        return movemask.trailing_zeros() as usize;
                    };

                    let mut offset = 0;

                    loop {
                        offset += 1;
                        let movemask = _mm_movemask_epi8(_mm_cmpeq_epi8(zeros, unsafe {
                            ptr.add(offset).read()
                        }));
                        if movemask != 0 {
                            return offset * size_of::<__m128i>()
                                + movemask.trailing_zeros() as usize
                                - diff;
                        };
                    }
                }

                #[inline]
                #[target_feature(enable = "avx2")]
                fn len_avx2(val: NTOsStrPtr) -> usize {
                    use std::arch::x86_64::*;
                    use std::mem::{align_of, size_of};

                    let ptr = val.0.cast::<u8>().as_ptr();
                    const _: () = const {
                        assert!(size_of::<__m256i>() == align_of::<__m256i>());
                    };
                    let diff = (ptr.addr()) & (size_of::<__m256i>() - 1);
                    let ptr = ptr.wrapping_sub(diff).cast::<__m256i>();

                    let zeros: __m256i = _mm256_set1_epi8(0);

                    let movemask =
                        _mm256_movemask_epi8(_mm256_cmpeq_epi8(zeros, unsafe { ptr.read() }))
                            >> diff;
                    if movemask != 0 {
                        return movemask.trailing_zeros() as usize;
                    };

                    let mut offset = 0;

                    loop {
                        offset += 1;
                        let movemask = _mm256_movemask_epi8(_mm256_cmpeq_epi8(zeros, unsafe {
                            ptr.add(offset).read()
                        }));
                        if movemask != 0 {
                            return offset * size_of::<__m256i>()
                                + movemask.trailing_zeros() as usize
                                - diff;
                        };
                    }
                }
                // SAFETY: We choose whichever one is available
                if std::arch::is_x86_feature_detected!("avx2") {
                    unsafe { len_avx2(self) }
                } else if std::arch::is_x86_feature_detected!("sse2") {
                    unsafe { len_sse(self) }
                } else {
                    len_default(self)
                }
            }
            #[cfg(not(target_arch = "x86_64"))]
            {
                len_default(self)
            }
        }

        /// returns true if the first byte is null (indicating that there is no data behind the string)
        pub fn is_empty(self) -> bool {
            // SAFETY: NTOsStrPtr is guaranteed to point to allocated memory that ends in a null byte
            unsafe { self.0.cast::<u8>().read() == b'\0' }
        }

        /// # Safety
        /// `ptr` must point at a buffer that is valid to read until the next
        /// null byte
        ///
        /// buffer pointed to by `ptr` must be valid as an OsStr
        ///
        /// The memory pointed to by `ptr` must not be modified as long as the
        /// returned string exists
        pub const unsafe fn from_ptr(ptr: NonNull<c_char>) -> Self {
            Self(ptr, PhantomData)
        }

        /// Returns a `&CStr` to the string
        pub fn to_cstr(self) -> &'a CStr {
            unsafe { CStr::from_ptr(self.0.as_ptr().cast_const()) }
        }

        /// Returns a `&NTOsStr` to the string excluding the null byte
        pub fn to_ntosstr(self) -> &'a NTOsStr {
            let len = self.len();
            // buffer is okay to read upto len + 1
            let slice =
                unsafe { std::slice::from_raw_parts(self.0.cast::<u8>().as_ptr(), len + 1) };
            // SAFETY: NTOsStrPtr points to a valid OsStr
            let slice = unsafe { OsStr::from_encoded_bytes_unchecked(slice) };
            // SAFETY: Last byte is null
            unsafe { NTOsStr::from_osstr_unchecked(slice) }
        }

        /// Returns a [`&OsStr`] to the string excluding the null byte
        pub fn to_osstr(self) -> &'a OsStr {
            self.to_ntosstr().as_osstr()
        }

        /// # SAFETY
        ///
        /// There must be at least one null byte within the [`OsStr`]
        pub unsafe fn from_osstr_unchecked(value: &'a OsStr) -> Self {
            unsafe { Self::from_ptr(NonNull::from_ref(value).cast::<c_char>()) }
        }

        /// Checks if the underlying pointers are equal
        pub fn ptr_eq(self, other: Self) -> bool {
            self.0 == other.0
        }

        /// Expects a single null byte at the end of the string and no null bytes
        /// in the rest of the string
        pub fn try_from_osstr(value: &'a OsStr) -> Result<Self, NTStrError> {
            Ok(NTOsStr::try_from_osstr(value)?.as_ntosstrptr())
        }

        /// same as [`OsStr::display`]
        pub fn display(&self) -> std::ffi::os_str::Display<'a> {
            self.to_osstr().display()
        }
    }

    impl<'a> std::hash::Hash for NTOsStrPtr<'a> {
        fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
            std::hash::Hash::hash(self.to_osstr(), hasher)
        }
    }

    impl<'a> PartialEq for NTOsStrPtr<'a> {
        fn eq(&self, other: &Self) -> bool {
            if self.0 == other.0 {
                true
            } else {
                let first = self.to_cstr();
                let other = other.to_cstr();
                first == other
            }
        }
    }

    impl<'a> Eq for NTOsStrPtr<'a> {}

    impl<'a> From<&'a NTOsStr> for NTOsStrPtr<'a> {
        fn from(other: &'a NTOsStr) -> Self {
            other.as_ntosstrptr()
        }
    }

    impl<'a> std::fmt::Debug for NTOsStrPtr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            std::fmt::Debug::fmt(self.to_osstr(), f)
        }
    }
}
