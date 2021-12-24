use super::cart::Cartridge;

#[derive(Debug)]
pub struct MemoryBus {
    cart: Cartridge
}

impl MemoryBus {
    pub fn new(cart_rom: Box<[u8]>) -> MemoryBus {
        MemoryBus {
            cart: Cartridge::new(cart_rom)
        }
    }
}

