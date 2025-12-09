fn main () {
	build_tests();
}

fn build_tests() {
	println!("{:?}", std::env::var("CARGO_MANIFEST_DIR").unwrap());
	let path = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());

	let out_dir = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());

	let mut archive_path = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
	archive_path.push("libtests.so");
	println!("{:?}", archive_path);
	// panic!();

	if std::fs::exists(&archive_path).ok().is_some_and(|x| !x) {
		std::process::Command::new("./src/grug-tests/build.sh").output()
			.expect("grug-tests is not pulled yet\n\
					Run 'git submodule init' within the src directory of grug-rs
					");
		std::fs::copy("./src/grug-tests/tests.so", &archive_path).unwrap();
	}
	
	// println!("cargo:rustc-link-search={}", "../grug-tests/.");
	println!("cargo::rustc-link-search={}", out_dir.display());

	eprintln!("{:?}", path);
	// panic!();
}
