use std::convert::TryFrom;
use num_enum::TryFromPrimitive;

use super::mem::MemoryBus;

#[derive(Debug, TryFromPrimitive)]
#[repr(u8)]
pub enum Opcode {
    Lda = 0xA9,
    Stz = 0x9C,
    Sta = 0x8D,
    Rti = 0x40,
    Brk = 0x00,
    Clc = 0x18,
    Sei = 0x78,
    Rep = 0xC2,
    Xce = 0xFB
}

pub struct Instruction(pub u8);

impl Instruction {
    #[inline(always)]
    pub fn op(&self) -> Opcode {
        Opcode::try_from(self.0).unwrap_or_else(|_| {
            panic!("Instruction: unrecognized opcode {:X}", self.0)
        })
    }
}


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
type InstructionFn = fn(&mut Core, &mut MemoryBus, AddressingModeFn) -> ();

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

// Normal Jumps

/// RTI (40) - Return from Interrupt
fn rti(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    if mode(cpu, bus) != AddressingMode::Implied {
        panic!("RTI: Invalid AddressingMode");
    }

    let reg_psr = cpu.pop_stack(bus);
    cpu.reg_psr.set_from_u8(reg_psr);


    cpu.reg_db = cpu.pop_stack(bus);
    let reg_pc_hi = cpu.pop_stack(bus) as u16;
    let reg_pc_lo = cpu.pop_stack(bus) as u16;
    cpu.reg_pc = (reg_pc_hi << 8) | reg_pc_lo;

    cpu.cycles += 6;
}

// Interrupts, Exceptions and Breakpoints

/// BRK (00) - Break
fn brk(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    if mode(cpu, bus) != AddressingMode::Implied {
        panic!("BRK: Invalid AddressingMode");
    }

    // TODO: Cycles?
    let new_reg_pc = cpu.reg_pc.wrapping_add(2);

    cpu.reg_psr.b = true;
    cpu.push_stack(bus, new_reg_pc as u8);
    cpu.push_stack(bus, (new_reg_pc >> 8) as u8);
    cpu.push_stack(bus, cpu.reg_db as u8);
    cpu.push_stack(bus, cpu.reg_psr.as_u8());
    cpu.reg_psr.d = false;
    cpu.reg_psr.i = true;
    cpu.reg_db = 0x00;

    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let reset_vector_lo = bus.read(cpu.reg_db, 0xFFFE) as u16;
            let reset_vector_hi = bus.read(cpu.reg_db, 0xFFFF) as u16;
            let reset_vector = (reset_vector_hi << 8) | reset_vector_lo;
            cpu.reg_pc = reset_vector;
        },
        EmulationMode::Native => {
            let reset_vector_lo = bus.read(cpu.reg_db, 0xFFF6) as u16;
            let reset_vector_hi = bus.read(cpu.reg_db, 0xFFF7) as u16;
            let reset_vector = (reset_vector_hi << 8) | reset_vector_lo;
            cpu.reg_pc = reset_vector;
        }
    }
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

/// REP (C2) - Reset Status Bits
fn rep(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let value: u8 = match mode(cpu, bus) {
        AddressingMode::Immediate(value) => value,
        _ => panic!("REP: Invalid AddressingMode")
    };

    let registers = cpu.reg_psr.as_u8() & !value;
    cpu.reg_psr.set_from_u8(registers);
    cpu.cycles += 3;
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

#[derive(Debug)]
struct ProcessorStatusRegister {
    c: bool, // Carry
    z: bool, // Zero
    i: bool, // Interrupt disable
    d: bool, // Decimal mode
    b: bool, // Break flag (also known as X)
    u: bool, // Unused (also known as M)
    v: bool, // Overflow
    n: bool, // Negative (Sign)
    e: EmulationMode, // 6502 Emulation mode
}

impl ProcessorStatusRegister {
    fn as_u8(&self) -> u8 {
        ((self.c as u8) << 0) +
        ((self.z as u8) << 1) +
        ((self.i as u8) << 2) +
        ((self.d as u8) << 3) +
        ((self.b as u8) << 4) +
        ((self.u as u8) << 5) +
        ((self.v as u8) << 6) +
        ((self.n as u8) << 7)
    }

    fn set_from_u8(&mut self, flags: u8) {
        // self.u is always 1, cannot be set
        self.c = flags & 0b0000_0001 != 0;
        self.z = flags & 0b0000_0010 != 0;
        self.i = flags & 0b0000_0100 != 0;
        self.d = flags & 0b0000_1000 != 0;
        self.b = flags & 0b0001_0000 != 0;
        self.v = flags & 0b0100_0000 != 0;
        self.n = flags & 0b1000_0000 != 0;
    }
}

impl Default for ProcessorStatusRegister {
    fn default() -> ProcessorStatusRegister {
        ProcessorStatusRegister {
            c: false,
            z: false,
            i: false,
            d: false,
            b: false,
            u: true,
            v: false,
            n: false,
            e: EmulationMode::Emulation
        }
    }
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

        self.reg_sp = 0x01FF;
    }

    pub fn run_cycle(&mut self, bus: &mut MemoryBus) {
        let instr = Instruction(bus.read(self.reg_db, self.reg_pc));
        self.reg_pc += 1;

        use Opcode::*;
        match instr.op() {
            // Load Register from Memory
            Lda => lda(self, bus, immediate),

            // Store Register in Memory
            Stz => stz(self, bus, absolute),
            Sta => sta(self, bus, absolute),

            // Normal Jumps
            Rti => rti(self, bus, implied),

            // Interrupts, Exceptions and Breakpoints
            Brk => brk(self, bus, implied),

            // CPU Control
            Clc => clc(self, bus, implied),
            Sei => sei(self, bus, implied),
            Rep => rep(self, bus, immediate),
            Xce => xce(self, bus, implied)
        }
    }

    fn push_stack(&mut self, bus: &mut MemoryBus, value: u8) {
        // TODO: Check hardcoded bank 0
        bus.write(0x00, self.reg_sp, value);
        self.reg_sp -= 1;
    }

    fn pop_stack(&mut self, bus: &mut MemoryBus) -> u8 {
        // TODO: Check hardcoded bank 0
        self.reg_sp += 1;
        let ret = bus.read(0x00, self.reg_sp);
        ret
    }
}

