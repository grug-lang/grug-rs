use std::env::var;
use std::path::PathBuf;

fn main () {
	build_tests();
}

fn build_tests() {
	println!("{:?}", var("CARGO_MANIFEST_DIR").unwrap());

	let out_dir = PathBuf::from(var("OUT_DIR").unwrap());

	let mut source_path = PathBuf::from("./src/grug-tests/");
	let mut archive_path = PathBuf::from(var("OUT_DIR").unwrap());

	#[cfg(target_os = "linux")]
	{
		source_path.push("tests.so");
		archive_path.push("libtests.so");
	}
	#[cfg(target_os = "windows")]
	{
		source_path.push("tests.dll");
		archive_path.push("tests.dll");
	}

	println!("{:?}", archive_path);

	if std::fs::exists(&archive_path).ok().is_some_and(|x| !x) {
		match std::fs::copy(&source_path, &archive_path) {
			Ok(_) => (),
			Err(_) => println!(
				"grug-tests is not pulled yet\n\
				Run the following commands first\n\n\
				'git submodule update --init --force'\n\
				'cd src/grug-tests/'\n\
				'./build.sh'"
			),
		}
	}
	println!("cargo::rustc-link-search={}", out_dir.display());
}
