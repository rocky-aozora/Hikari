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

    pub fn initialize(&mut self) {
        self.cpu.initialize(&self.bus);
    }

    pub fn cpu(&self) -> &Core {
        &self.cpu
    }

    pub fn run_cycle(&mut self) -> u8 {
        self.cpu.run_cycle(&mut self.bus)
    }
}

