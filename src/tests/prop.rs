use crate::prop::*;
use cascade::cascade;
use pyo3::PyResult;

#[test]
fn parsing() -> PyResult<()> {
    let f = Proposition::new(Some(Atom(1)), None)?;
    assert_eq!(f, Proposition::from_str("a")?);
    Ok(())
}
