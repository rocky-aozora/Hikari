use std::fmt;

use super::cart::Cartridge;


#[derive(Default)]
struct IoPorts;

impl IoPorts {
    fn new() -> IoPorts {
        IoPorts::default()
    }

    fn write(&mut self, _bank: u8, _addr: u16, _val: u8) {
        // TODO: Implement
    }
}


const WRAM_SIZE: usize = 0x20000; // 128kb

pub struct MemoryBus {
    io: IoPorts,
    cart: Cartridge,
    wram: [u8; WRAM_SIZE] // 128kb
}

impl MemoryBus {
    pub fn new(cart_rom: Box<[u8]>) -> MemoryBus {
        MemoryBus {
            io: IoPorts::new(),
            cart: Cartridge::new(cart_rom),
            wram: [0; WRAM_SIZE]
        }
    }

    pub fn read(&self, bank: u8, addr: u16) -> u8 {
        match bank {
            0x00..=0x3F => match addr { // Q1
                0x0000..=0x1FFF => self.wram[(addr as usize) + ((bank as usize) * 0x2000)],
                0x8000..=0xFFFF => self.cart.read(bank, addr),
                _ => panic!("MemoryBus: read unknown address {:X} on bank {:X}", addr, bank)
            },
            0x40..=0x7D => self.cart.read(bank, addr), // Q2
            0x7E..=0x7F => self.wram[(addr as usize) + ((bank as usize) - 0x7E)], // Q2 WRAM
            0x80..=0xBF => match addr { // Q3
                0x0000..=0x1FFF => self.wram[(addr as usize) + ((bank - 0x80) as usize) * 0x2000],
                0x8000..=0xFFFF => self.cart.read(bank, addr),
                _ => panic!("MemoryBus: read unknown address {:X} on bank {:X}", addr, bank)
            },
            0xC0..=0xFF => self.cart.read(bank, addr) // Q4
        }
    }

    pub fn write(&mut self, bank: u8, addr: u16, val: u8) {
        println!("MemoryBus: write {:x} to {:x} on bank {:x}", val, addr, bank);
        match bank {
            0x00..=0x3F => match addr { // Q1
                0x0000..=0x1FFF => self.wram[(addr as usize) + ((bank as usize) * 0x2000)] = val,
                0x4200..=0x5FFF => self.io.write(bank, addr, val),
                0x8000..=0xFFFF => self.cart.write(bank, addr, val),
                _ => panic!("MemoryBus: write to unknown address {:X} on bank {:X}", addr, bank)
            },
            0x40..=0x7D => self.cart.write(bank, addr, val), // Q2
            0x7E..=0x7F => self.wram[(addr as usize) + ((bank as usize) - 0x7E)] = val, // Q2 WRAM
            0x80..=0xBF => match addr { // Q3
                0x0000..=0x1FFF => self.wram[(addr as usize) + ((bank - 0x80) as usize) * 0x2000] = val,
                0x8000..=0xFFFF => self.cart.write(bank, addr, val),
                _ => panic!("MemoryBus: write to unknown address {:X} on bank {:X}", addr, bank)
            },
            0xC0..=0xFF => self.cart.write(bank, addr, val) // Q4
        }
    }
}

impl fmt::Debug for MemoryBus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "MemoryBus")
    }
}

