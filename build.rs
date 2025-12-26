use std::env::var;
use std::path::PathBuf;

fn main () {
	build_tests();
}

fn build_tests() {
	println!("{:?}", var("CARGO_MANIFEST_DIR").unwrap());

	let out_dir = PathBuf::from(var("OUT_DIR").unwrap());

	let source_path = String::from("./src/grug-tests");
	let archive_path = String::from(var("OUT_DIR").unwrap());

	println!("{:?}", archive_path);

	if std::fs::exists(&archive_path).ok().is_some_and(|x| !x) {
		#[cfg(target_os = "linux")]
		{
			match std::fs::copy(&source_path + "tests.so", &archive_path + "libtests.so") {
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
		#[cfg(target_os = "windows")]
		{
			match std::fs::copy(source_path.clone() + "/tests.dll", archive_path.clone() + "/tests.dll") {
				Ok(_) => (),
				Err(_) => println!(
					"grug-tests is not pulled yet\n\
					Run the following commands first\n\n\
					'git submodule update --init --force'\n\
					'cd src/grug-tests/'\n\
					'./build.sh'"
				),
			}
			match std::fs::copy(source_path + "/libtests.dll.a", archive_path + "/tests.lib") {
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
	}
	println!("cargo::rustc-link-search={}", out_dir.display());
}
