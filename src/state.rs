use crate::mod_api::{ModApi, get_mod_api};
use crate::error::GrugError;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct GrugState {
	pub(crate) mod_api: ModApi,
	pub(crate) mods_dir_path: PathBuf,
}

impl GrugState {
	pub fn new<'a, J: AsRef<Path>, D: AsRef<Path>> (mod_api_path: J, mods_dir_path: D) -> Result<Self, GrugError<'a>> {
		let mod_api_text = std::fs::read_to_string(mod_api_path).unwrap();
		let mod_api = get_mod_api(&mod_api_text)?;
		Ok(Self {
			mod_api,
			mods_dir_path: PathBuf::from(mods_dir_path.as_ref()),
		})
	}
}
