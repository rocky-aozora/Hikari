use num_enum::TryFromPrimitive;
use std::convert::TryFrom;

#[derive(Debug)]
enum Mapper {
    LoRom,
    HiRom
}

#[derive(Debug, TryFromPrimitive)]
#[repr(u8)]
enum Type {
    RomOnly             = 0b0000,
    RomSram             = 0b0001,
    RomSramBattery      = 0b0010,
    RomCoCpu            = 0b0011,
    RomCoCpuSram        = 0b0100,
    RomCoCpuSramBattery = 0b0101,
    RomCoCpuBattery     = 0b0110
}

#[derive(Debug, TryFromPrimitive)]
#[repr(u8)]
enum Region {
    Japan         = 0x00,
    NorthAmerica  = 0x01,
    Europe        = 0x02,
    Sweden        = 0x03,
    Finland       = 0x04,
    Denmark       = 0x05,
    France        = 0x06,
    Netherlands   = 0x07,
    Spain         = 0x08,
    Germany       = 0x09,
    Italy         = 0x0A,
    China         = 0x0B,
    Indonesia     = 0x0C,
    SouthKorea    = 0x0D,
    International = 0x0E,
    Canada        = 0x0F,
    Brazil        = 0x10,
    Australia     = 0x11
}

enum Addr {
    Rom(u32),
    Sram(u32)
}

#[derive(Debug)]
struct Header {
    version: u8,
    name: String,
    region: Region,

    mapper: Mapper,
    cart_type: Type,
    rom_size: u32,
    ram_size: u32,

    checksum: u16,
    complement: u16
}

pub struct Cartridge {
    header: Header,
    rom: Box<[u8]>,
    ram: Vec<u8>
}

impl Cartridge {
    pub fn new(rom: Box<[u8]>) -> Cartridge {
        let header = parse_header(Mapper::LoRom, &rom);
        let ram = vec![0; header.ram_size as usize];
        Cartridge { header, rom, ram }
    }

    pub fn read(&self, bank: u8, addr: u16) -> u8 {
        match self.resolve_addr(bank, addr) {
            Addr::Rom(offset) => self.rom[offset as usize],
            Addr::Sram(offset) => self.ram[offset as usize]
        }
    }

    pub fn write(&mut self, bank: u8, addr: u16, val: u8) {
        match self.resolve_addr(bank, addr) {
            Addr::Rom(_) => panic!("Cartridge: Cannot write to ROM!"),
            Addr::Sram(offset) => self.ram[offset as usize] = val
        }
    }

    fn resolve_addr(&self, bank: u8, addr: u16) -> Addr {
        match self.header.mapper {
            Mapper::LoRom => self.resolve_lorom(bank, addr),
            Mapper::HiRom => unimplemented!()
        }
    }

    fn resolve_lorom(&self, bank: u8, addr: u16) -> Addr {
        match addr {
            0x8000..=0xFFFF => match bank {
                0x00..=0x7D => {
                    let bank_step = bank as u32 * 0x8000;
                    let offset = ((addr as u32) - 0x8000) + bank_step;
                    Addr::Rom(offset % self.header.rom_size)
                },
                0x80..=0xFF => {
                    let bank_step = (bank as u32 - 0x80) * 0x8000;
                    let offset = ((addr as u32) - 0x8000) + bank_step;
                    Addr::Rom(offset % self.header.rom_size)
                },
                _ => panic!("Cartridge: r/w to non-ROMSEL address {:#X}{:X}", bank, addr)
            },
            0x0000..=0x7FFF => match bank {
                0x40..=0x80 => {
                    let bank_step = (bank as u32 - 0x40) * 0x8000;
                    let offset = addr as u32 + bank_step;
                    Addr::Sram(offset % self.header.ram_size)
                },
                0xC0..=0xEF => {
                    let bank_step = (bank as u32 - 0xBF) * 0x8000;
                    let offset = addr as u32 + bank_step;
                    Addr::Rom(offset % self.header.rom_size)
                },
                0xF0..=0xFF => {
                    let bank_step = (bank as u32 - 0xEF) * 0x8000;
                    let offset = addr as u32 + bank_step;
                    Addr::Sram(offset % self.header.ram_size)
                },
                _ => panic!("Cartridge: r/w to non-ROMSEL address {:#X}{:X}", bank, addr)

            }
        }
    }
}

fn parse_header(expected_type: Mapper, rom: &Box<[u8]>) -> Header {
    let (header_data_start, header_data_end) = match expected_type {
        Mapper::LoRom => (0x7FB0, 0xFFFF),
        Mapper::HiRom => unimplemented!()
    };

    let header_data = &rom[header_data_start..=header_data_end];

    let name = title_from_bytes(&header_data[0x10..=0x24]);
    let mapper_mode = header_data[0x25];
    let cart_type = header_data[0x26];
    let cart_type = Type::try_from(cart_type).unwrap_or_else(|_| {
        panic!("Unrecognized Cart Type!")
    });
    let rom_size = header_data[0x27];
    let ram_size = header_data[0x28];

    let dev_id = header_data[0x2A];
    let version = header_data[0x2B];
    let region = header_data[0x29];
    let region = Region::try_from(region).unwrap_or_else(|_| {
        panic!("Unrecognized Region!")
    });

    let complement_lo = header_data[0x2C] as u16;
    let complement_hi = header_data[0x2D] as u16;
    let complement = (complement_hi << 8) | complement_lo;

    let checksum_lo = header_data[0x2E] as u16;
    let checksum_hi = header_data[0x2F] as u16;
    let checksum = (checksum_hi << 8) | checksum_lo;

    Header {
        version,
        name,
        region,

        mapper: expected_type,
        cart_type,
        rom_size: 0x400 << (rom_size as u32),
        ram_size: 0x400 << (ram_size as u32),

        checksum,
        complement
    }
}

fn title_from_bytes(data: &[u8]) -> String {
    let mut buffer = [0; 21]; // Maximum of 21 characters.
    for (index, character) in data.iter().enumerate() {
        match *character {
            0x20..=0x7E => buffer[index] = *character,
            _ => panic!("Invalid ASCII character in title!")
        }
    }

    let res = String::from_utf8(buffer.to_vec()).unwrap();
    res.trim_end().to_string()
}

