pub struct Cartridge {
    rom: Box<[u8]>
}

impl Cartridge {
    pub fn new(cart_rom: Box<[u8]>) -> Cartridge {
        Cartridge { rom: cart_rom }
    }

    pub fn read(&self, bank: u8, addr: u16) -> u8 {
        // TODO: Different mappers. For now LoROM.
        match bank {
            0x00..=0x3F => match addr { // Q1
                0x8000..=0xFFFF => self.rom[((addr as usize) - 0x8000) | (bank as usize)],
                _ => panic!("Cartridge: read non-romsel address {:X} on bank {:X}", addr, bank)
            },
            0x40..=0x7D => 0, // Q2
            0x7E..=0x7F => unreachable!(),
            0x80..=0xBF => match addr { // Q3
                0x8000..=0xFFFF => self.rom[((addr as usize) - 0x8000) | ((bank - 0x80) as usize)],
                _ => panic!("Cartridge: read non-romsel address {:X} on bank {:X}", addr, bank)
            },
            0xC0..=0xFF => 0 // Q4
        }
    }

    pub fn write(&mut self, bank: u8, addr: u16, val: u8) {
        // TODO: Different mappers. For now LoROM.
        match bank {
            0x00..=0x3F => match addr { // Q1
                0x8000..=0xFFFF => self.rom[((addr as usize) - 0x8000) | (bank as usize)] = val,
                _ => panic!("Cartridge: write to non-romsel address {:X} on bank {:X}", addr, bank)
            },
            0x40..=0x7D => { }, // Q2
            0x7E..=0x7F => unreachable!(),
            0x80..=0xBF => match addr { // Q3
                0x8000..=0xFFFF => self.rom[((addr as usize) - 0x8000) | ((bank - 0x80) as usize)] = val,
                _ => panic!("Cartridge: write to non-romsel address {:X} on bank {:X}", addr, bank)
            },
            0xC0..=0xFF => { } // Q4
        }
    }
}

