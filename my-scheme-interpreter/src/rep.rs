use std::error::Error;

pub trait DisplayRep {
    fn to_display(&self) -> Result<String, Box<dyn Error>>;
}

pub trait ExternalRep {
    fn to_external(&self) -> Result<String, Box<dyn Error>>;
}
