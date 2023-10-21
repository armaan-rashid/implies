#[cfg(features = "python")]
use pyo3_build_config;
fn main() {
    #[cfg(features = "python")]
    pyo3_build_config::add_extension_module_link_args();
}
