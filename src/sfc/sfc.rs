use super::cpu::Core;
use super::mem::MemoryBus;

#[derive(Debug)]
pub struct SuperFamicom {
    cpu: Core,
    bus: MemoryBus
}

impl SuperFamicom {
    pub fn new(cart_rom: Box<[u8]>) -> SuperFamicom {
        SuperFamicom {
            bus: MemoryBus::new(cart_rom),
            cpu: Core::new()
        }
    }
}

