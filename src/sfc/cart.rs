use std::fmt;

pub struct Cartridge {
    rom: Box<[u8]>
}

impl Cartridge {
    pub fn new(cart_rom: Box<[u8]>) -> Cartridge {
        Cartridge { rom: cart_rom }
    }
}

impl fmt::Debug for Cartridge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Cartridge")
    }
}

