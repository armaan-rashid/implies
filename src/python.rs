use crate::parser::ParseError;
use pyo3::{IntoPy, PyErr};

impl pyo3::PyErrArguments for ParseError {
    fn arguments(self, py: pyo3::Python<'_>) -> pyo3::PyObject {
        self.to_string().into_py(py)
    }
}

impl From<ParseError> for PyErr {
    fn from(value: ParseError) -> Self {
        pyo3::exceptions::PyValueError::new_err(value)
    }
}
pub mod proposition;
