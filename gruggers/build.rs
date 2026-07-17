use std::env::var;
use std::path::PathBuf;

fn main () {
	build_tests();
}

fn build_tests() {
	println!("{:?}", var("CARGO_MANIFEST_DIR").unwrap());

	let out_dir = PathBuf::from(var("OUT_DIR").unwrap());

	let test_source_path  = String::from("./src/grug-tests/build/");
	let bench_source_path = String::from("./src/grug-bench/build/");
	let archive_path = var("OUT_DIR").unwrap();

	println!("cargo::rustc-link-search={}", out_dir.display());

	#[cfg(target_os = "linux")]
	{
		if let Ok(true) = std::fs::exists(&test_source_path) {
			println!("cargo::rerun-if-changed={}", test_source_path);
			_ = std::fs::copy(test_source_path + "/libtests.so", archive_path.clone() + "/libtests.so");
		}
		if let Ok(true) = std::fs::exists(&bench_source_path) {
			println!("cargo::rerun-if-changed={}", bench_source_path);
			_ = std::fs::copy(bench_source_path + "/libbench.so", archive_path + "/libbench.so");
		}
	}
	#[cfg(target_os = "windows")]
	{
		if let Ok(true) = std::fs::exists(&test_source_path) {
			println!("cargo::rerun-if-changed={}", test_source_path);
			let tests_dll_out_path = archive_path.clone() + "/tests.dll";
			for optional_path in ["tests.dll"] {
				_ = std::fs::copy(test_source_path.clone() + optional_path, &tests_dll_out_path);
			}
			let tests_lib_out_path = archive_path.clone() + "/tests.lib";
			for optional_path in ["tests.lib", "libtests.dll.a"] {
				_ = std::fs::copy(test_source_path.clone() + optional_path, &tests_lib_out_path);
			}
		}
		if let Ok(true) = std::fs::exists(&bench_source_path) {
			println!("cargo::rerun-if-changed={}", bench_source_path);
			let bench_dll_out_path = archive_path.clone() + "/bench.dll";
			for optional_path in ["bench.dll", "libbench.dll"] {
				_ = std::fs::copy(bench_source_path.clone() + optional_path, &bench_dll_out_path);
			}
			let bench_lib_out_path = archive_path + "/bench.lib";
			for optional_path in ["bench.lib", "libbench.lib"] {
				_ = std::fs::copy(bench_source_path.clone() + optional_path, &bench_lib_out_path);
			}
		}
	}
}
