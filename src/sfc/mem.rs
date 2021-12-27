use std::fmt;

use super::cart::Cartridge;


#[derive(Debug)]
pub enum Addr {
    RomSel(u8, u16),
    Wram(u32),
    Io // TODO: Unimplemented for now
}

const WRAM_SIZE: usize = 0x20000; // 128kb

pub struct MemoryBus {
    cart: Cartridge,
    wram: [u8; WRAM_SIZE]
}

impl MemoryBus {
    pub fn new(cart_rom: Box<[u8]>) -> MemoryBus {
        MemoryBus {
            cart: Cartridge::new(cart_rom),
            wram: [0; WRAM_SIZE]
        }
    }

    pub fn cart(&self) -> &Cartridge {
        &self.cart
    }

    pub fn read(&self, bank: u8, addr: u16) -> u8 {
        match self.resolve_addr(bank, addr) {
            Addr::Wram(offset) => self.wram[offset as usize],
            Addr::RomSel(bank, addr) => self.cart.read(bank, addr),
            Addr::Io => 0,
        }
    }

    pub fn write(&mut self, bank: u8, addr: u16, val: u8) {
        match self.resolve_addr(bank, addr) {
            Addr::Wram(offset) => self.wram[offset as usize] = val,
            Addr::RomSel(bank, addr) => self.cart.write(bank, addr, val),
            Addr::Io => { },
        }
    }

    pub fn resolve_addr(&self, bank: u8, addr: u16) -> Addr {
        match bank {
            0x00..=0x3F => match addr {
                0x0000..=0x1FFF =>
                    Addr::Wram((addr as u32) + ((bank as u32) * 0x2000)),
                0x2100..=0x21FF => Addr::Io,
                0x4200..=0x5FFF => Addr::Io,
                0x8000..=0xFFFF => Addr::RomSel(bank, addr),
                _ => panic!("MemoryBus: unknown address ${:X}{:X}", addr, bank)
            },
            0x40..=0x7D => Addr::RomSel(bank, addr),
            0x7E..=0x7F => Addr::Wram(addr as u32 + (bank as u32) - 0x7E),
            0x80..=0xBF => match addr {
                0x0000..=0x1FFF => Addr::Wram(addr as u32 + ((bank - 0x80) as u32) * 0x2000),
                0x8000..=0xFFFF => Addr::RomSel(bank, addr),
                _ => panic!("MemoryBus: unknown address ${:X}{:X}", addr, bank)
            },
            0xC0..=0xFF => Addr::RomSel(bank, addr)
        }
    }
}

impl fmt::Debug for MemoryBus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "MemoryBus")
    }
}

