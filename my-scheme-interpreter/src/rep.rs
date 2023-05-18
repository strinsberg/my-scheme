pub trait DisplayRep {
    fn to_display(&self) -> String;
}

pub trait ExternalRep {
    fn to_external(&self) -> String;
}
