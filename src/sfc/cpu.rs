use super::mem::MemoryBus;


/// An `AddressingMode` determines what data an `Instruction` is operating on.
/// They are implemented as a function (`AddressingModeFn`) on the cpu that
/// fetches some data which the instruction can use to perform its logic with.
#[derive(PartialEq)]
enum AddressingMode {
    Absolute(u16),
    Immediate(u8),
    Implied
}

type AddressingModeFn = fn(&mut Core, &MemoryBus) -> AddressingMode;

fn implied(_cpu: &mut Core, _bus: &MemoryBus) -> AddressingMode {
    AddressingMode::Implied
}

fn immediate(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let value = bus.read(cpu.reg_db, cpu.reg_pc);
    cpu.reg_pc = cpu.reg_pc.wrapping_add(1);
    AddressingMode::Immediate(value)
}

fn absolute(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let addr_lo = bus.read(cpu.reg_db, cpu.reg_pc) as u16;
    let addr_hi = bus.read(cpu.reg_db, cpu.reg_pc + 1) as u16;
    cpu.reg_pc = cpu.reg_pc.wrapping_add(2);

    let addr = (addr_hi << 8) | addr_lo;
    AddressingMode::Absolute(addr)
}


/// An `Instruction` is the implementation of the 65816 CPU instruction related to
/// the opcode. An instruction performs its logic on the CPU and can use different
/// `AddressingMode`s to be able to work on different types of data.
type Instruction = fn(&mut Core, &mut MemoryBus, AddressingModeFn) -> ();

// Load Register from Memory

/// LDA (A9) - Load Accumulator
fn lda(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let value: u8 = match mode(cpu, bus) {
        AddressingMode::Immediate(value) => value,
        _ => panic!("LDA: Invalid AddressingMode")
    };

    cpu.reg_a = value as u16;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
        },
        EmulationMode::Native => {
            cpu.reg_psr.n = (value as u16) & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
        }
    }

    cpu.cycles += 2;
}

// Store Register in Memory

/// STZ (9C) - Store Zero
fn stz(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let addr: u16 = match mode(cpu, bus) {
        AddressingMode::Absolute(addr) => addr,
        _ => panic!("STZ: Invalid AddressingMode")
    };

    bus.write(cpu.reg_db, addr, 0);
    cpu.cycles += 4;
}

/// STA (8D) - Store Accumulator
fn sta(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let addr: u16 = match mode(cpu, bus) {
        AddressingMode::Absolute(addr) => addr,
        _ => panic!("STZ: Invalid AddressingMode")
    };

    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            bus.write(cpu.reg_db, addr, cpu.reg_a as u8);
        },
        EmulationMode::Native => {
            // TODO: Figure out if this is correct
            bus.write(cpu.reg_db, addr, cpu.reg_a as u8);
            bus.write(cpu.reg_db, addr + 1, (cpu.reg_a >> 8) as u8);
        }
    }

    cpu.cycles += 4;
}

// CPU Control

/// CLC (18) - Clear carry flag
fn clc(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    if mode(cpu, bus) != AddressingMode::Implied {
        panic!("CLC: Invalid AddressingMode");
    }

    cpu.reg_psr.c = false;
    cpu.cycles += 2;
}

/// SEI (78) - Set interrupt disable bit
fn sei(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    if mode(cpu, bus) != AddressingMode::Implied {
        panic!("SEI: Invalid AddressingMode");
    }

    cpu.reg_psr.i = true;
    cpu.cycles += 2;
}

fn xce(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    if mode(cpu, bus) != AddressingMode::Implied {
        panic!("SEI: Invalid AddressingMode");
    }

    use EmulationMode::*;
    let temp = cpu.reg_psr.c;
    cpu.reg_psr.c = cpu.reg_psr.e == Emulation;
    cpu.reg_psr.e = if temp { Emulation } else { Native };
    cpu.cycles += 2;
}

#[derive(PartialEq, Debug)]
enum EmulationMode {
    Emulation,
    Native
}

impl Default for EmulationMode {
    fn default() -> EmulationMode {
        EmulationMode::Emulation
    }
}

#[derive(Default, Debug)]
struct ProcessorStatusRegister {
    c: bool, // Carry
    z: bool, // Zero
    i: bool, // Interrupt disable
    n: bool, // Negative (Sign)
    e: EmulationMode, // 6502 Emulation mode
}

#[derive(Default, Debug)]
pub struct Core {
    reg_a: u16,
    reg_x: u16,
    reg_y: u16,

    reg_sp: u16,
    pub reg_pc: u16,
    pub reg_db: u8,

    reg_psr: ProcessorStatusRegister,
    cycles: usize
}

impl Core {
    pub fn new() -> Core {
        Core::default()
    }

    pub fn initialize(&mut self, bus: &MemoryBus) {
        let reset_vec_lo = bus.read(0, 0xFFFC) as u16;
        let reset_vec_hi = bus.read(0, 0xFFFD) as u16;
        self.reg_pc = (reset_vec_hi << 8) | reset_vec_lo;
    }

    pub fn run_cycle(&mut self, bus: &mut MemoryBus) -> u8 {
        let op = bus.read(self.reg_db, self.reg_pc);
        self.reg_pc += 1;

        match op {
            // Load Register from Memory
            0xA9 => lda(self, bus, immediate),

            // Store Register in Memory
            0x9C => stz(self, bus, absolute),
            0x8D => sta(self, bus, absolute),

            // CPU Control
            0x18 => clc(self, bus, implied),
            0x78 => sei(self, bus, implied),
            0xFB => xce(self, bus, implied),
            _ => panic!("Core: Unknown instruction {:X}", op)
        }

        op
    }
}

