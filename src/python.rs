pub mod prop;

use prop::Proposition;
use pyo3::prelude::*;

#[pymodule]
fn implies(py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<Proposition>()?;
    Ok(())
}
