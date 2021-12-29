use super::mem::MemoryBus;

#[derive(Debug)]
pub enum Opcode {
    Lda,
    Ldx,
    Ldy,
    Stz,
    Sta,
    Ora,
    Sbc,
    Dex,
    Tsb,
    Brl,
    Jsr,
    Rti,
    Bmi,
    Bne,
    Brk,
    Clc,
    Sei,
    Rep,
    Xce
}

pub struct Instruction {
    pub op: Opcode,
    mode: AddressingModeFn,
    func: InstructionFn
}

impl Instruction {
    pub fn new(op: u8) -> Self {
        match op {
            0xA9 => Instruction { op: Opcode::Lda, func: lda, mode: immediate },
            0xA2 => Instruction { op: Opcode::Ldx, func: ldx, mode: immediate },
            0xAC => Instruction { op: Opcode::Ldy, func: ldy, mode: absolute },
            0x9C => Instruction { op: Opcode::Stz, func: stz, mode: absolute },
            0x8D => Instruction { op: Opcode::Sta, func: sta, mode: absolute },
            0x03 => Instruction { op: Opcode::Ora, func: ora, mode: stack_relative },
            0x01 => Instruction { op: Opcode::Ora, func: ora, mode: indirect_x },
            0xFF => Instruction { op: Opcode::Sbc, func: sbc, mode: long },
            0xCA => Instruction { op: Opcode::Dex, func: dex, mode: implied },
            0x04 => Instruction { op: Opcode::Tsb, func: tsb, mode: direct_page },
            0x82 => Instruction { op: Opcode::Brl, func: brl, mode: absolute },
            0x20 => Instruction { op: Opcode::Jsr, func: jsr, mode: absolute },
            0xFC => Instruction { op: Opcode::Jsr, func: jsr, mode: absolute_x },
            0x40 => Instruction { op: Opcode::Rti, func: rti, mode: implied },
            0x30 => Instruction { op: Opcode::Bmi, func: bmi, mode: immediate },
            0xD0 => Instruction { op: Opcode::Bne, func: bne, mode: immediate },
            0x00 => Instruction { op: Opcode::Brk, func: brk, mode: implied },
            0x18 => Instruction { op: Opcode::Clc, func: clc, mode: implied },
            0x78 => Instruction { op: Opcode::Sei, func: sei, mode: implied },
            0xC2 => Instruction { op: Opcode::Rep, func: rep, mode: immediate_b },
            0xFB => Instruction { op: Opcode::Xce, func: xce, mode: implied },
            _ => panic!("Unimplemented opcode {:X}", op)
        }
    }

    fn exec(&self, cpu: &mut Core, bus: &mut MemoryBus) {
        (self.func)(cpu, bus, self.mode);
    }
}


/// An `AddressingMode` determines what data an `Instruction` is operating on.
/// They are implemented as a function (`AddressingModeFn`) on the cpu that
/// fetches some data which the instruction can use to perform its logic with.
#[derive(PartialEq)]
enum AddressingMode {
    Immediate(u16),
    ImmediateB(u8),
    Absolute(u16),
    AbsoluteX(u16),
    DirectPage(u8),
    IndirectX(u16),
    StackRelative(u16),
    Long(u8, u16),
    Implied
}

impl AddressingMode {
    fn read_byte(self, cpu: &mut Core, bus: &MemoryBus) -> u8 {
        match self {
            AddressingMode::Immediate(_) =>
                panic!("AddressingMode: read byte from immediate in 16-bit mode"),
            AddressingMode::ImmediateB(value) => value,
            _ => {
                let (bank, addr) = self.address(cpu, bus);
                bus.read(bank, addr)
            }
        }
    }


    fn read_word(self, cpu: &mut Core, bus: &MemoryBus) -> u16 {
        match self {
            AddressingMode::Immediate(value) => value,
            AddressingMode::ImmediateB(_) =>
                panic!("AddressingMode: read word from immediate in 8-bit mode"),
            _ => {
                let (bank, addr) = self.address(cpu, bus);
                let word_lo = bus.read(bank, addr) as u16;
                let addr = addr.wrapping_add(1);
                let word_hi = bus.read(bank, addr) as u16;
                (word_hi << 8) | word_lo
            }
        }
    }

    fn address(self, cpu: &mut Core, bus: &MemoryBus) -> (u8, u16) {
        match self {
            AddressingMode::Absolute(addr) => (cpu.reg_db, addr),
            AddressingMode::AbsoluteX(addr) => (cpu.reg_db, addr),
            AddressingMode::IndirectX(indirect_addr) => {
                let addr_lo = bus.read(cpu.reg_db, indirect_addr) as u16;
                let indirect_addr = indirect_addr.wrapping_add(1);
                let addr_hi = bus.read(cpu.reg_db, indirect_addr) as u16;

                let addr = (addr_hi << 8) | addr_lo;
                (cpu.reg_db, addr)
            },
            AddressingMode::StackRelative(offset) =>
                (cpu.reg_db, cpu.reg_sp.wrapping_add(offset)),
            AddressingMode::Long(bank, addr) => (bank, addr),
            AddressingMode::DirectPage(addr) => (0x00, addr as u16),
            AddressingMode::Implied =>
                panic!("AddressingMode: attempting to address an implied address"),
            AddressingMode::Immediate(_) | AddressingMode::ImmediateB(_) =>
                panic!("AddressingMode: attempting to addres an immediate value")
        }
    }
}


type AddressingModeFn = fn(&mut Core, &MemoryBus) -> AddressingMode;

fn implied(_cpu: &mut Core, _bus: &MemoryBus) -> AddressingMode {
    AddressingMode::Implied
}

fn immediate(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = cpu.read_byte(bus);
            AddressingMode::ImmediateB(value)
        },
        EmulationMode::Native => {
            let value = cpu.read_word(bus);
            AddressingMode::Immediate(value)
        }
    }
}


fn immediate_b(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let value = cpu.read_byte(bus);
    AddressingMode::ImmediateB(value)
}

fn absolute(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let addr = cpu.read_word(bus);
    AddressingMode::Absolute(addr)
}

fn absolute_x(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let addr = cpu.read_word(bus).wrapping_add(cpu.reg_x);
    AddressingMode::AbsoluteX(addr)
}

fn direct_page(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let addr = bus.read(cpu.reg_db, cpu.reg_pc);
    cpu.reg_pc = cpu.reg_pc.wrapping_add(1);
    AddressingMode::DirectPage(addr)
}

fn indirect_x(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let addr_index = cpu.reg_pc.wrapping_add(cpu.reg_d);
    let indirect_addr = bus.read(cpu.reg_db, addr_index) as u16;
    cpu.reg_pc = cpu.reg_pc.wrapping_add(1);

    AddressingMode::IndirectX(indirect_addr)
}

fn stack_relative(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let offset = bus.read(cpu.reg_db, cpu.reg_pc) as u16;
    AddressingMode::StackRelative(offset)
}

fn long(cpu: &mut Core, bus: &MemoryBus) -> AddressingMode {
    let bank = bus.read(cpu.reg_db, cpu.reg_pc);
    cpu.reg_pc = cpu.reg_pc.wrapping_add(1);

    let addr = cpu.read_word(bus);
    AddressingMode::Long(bank, addr)
}

/// An `Instruction` is the implementation of the 65816 CPU instruction related to
/// the opcode. An instruction performs its logic on the CPU and can use different
/// `AddressingMode`s to be able to work on different types of data.
type InstructionFn = fn(&mut Core, &mut MemoryBus, AddressingModeFn) -> ();

// Load Register from Memory

/// LDA (A9) - Load Accumulator
fn lda(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let address: AddressingMode = mode(cpu, bus);

    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = address.read_byte(cpu, bus);
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
        },
        EmulationMode::Native => {
            let value = address.read_word(cpu, bus);
            cpu.reg_psr.n = value & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
        }
    }

    cpu.cycles += 2;
}

/// LDX (A2) - Load X
fn ldx(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let address: AddressingMode = mode(cpu, bus);

    cpu.cycles += 2;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = address.read_byte(cpu, bus);
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
        },
        EmulationMode::Native => {
            let value = address.read_word(cpu, bus);
            cpu.reg_psr.n = value & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
        }
    }
}

/// LDY (AC) - Load Y
fn ldy(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let address: AddressingMode = mode(cpu, bus);

    cpu.cycles += 2;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = address.read_byte(cpu, bus);
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
        },
        EmulationMode::Native => {
            let value = address.read_word(cpu, bus);
            cpu.reg_psr.n = (value as u16) & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
        }
    }
}

// Store Register in Memory

/// STZ (9C) - Store Zero
fn stz(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let (bank, addr) = mode(cpu, bus).address(cpu, bus);

    bus.write(bank, addr, 0);
    cpu.cycles += 4;
}

/// STA (8D) - Store Accumulator
fn sta(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let (bank, addr) = mode(cpu, bus).address(cpu, bus);

    cpu.cycles += 4;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            bus.write(bank, addr, cpu.reg_a as u8);
        },
        EmulationMode::Native => {
            // TODO: Figure out if this is correct
            bus.write(bank, addr, cpu.reg_a as u8);
            let addr = addr.wrapping_add(1);
            bus.write(bank, addr, (cpu.reg_a >> 8) as u8);
        }
    }
}

// Arithmetic/Logical operations

/// ORA (03, 01) - ALU Or
fn ora(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let address: AddressingMode = mode(cpu, bus);

    cpu.cycles += 6;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = address.read_byte(cpu, bus);
            cpu.reg_a = (cpu.reg_a as u8 | value) as u16;
        },
        EmulationMode::Native => {
            let value = address.read_word(cpu, bus);
            cpu.reg_a = cpu.reg_a | value;
        }
    }
}

/// SBC (FF) - ALU Subtract
fn sbc(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let address: AddressingMode = mode(cpu, bus);

    cpu.cycles += 5;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = address.read_byte(cpu, bus) as u16;
            let carry = cpu.reg_psr.c as u16;
            let res = (cpu.reg_a as u16).wrapping_add(carry)
                                        .wrapping_sub(1)
                                        .wrapping_add(value);
            cpu.reg_a = res as u8 as u16;
            cpu.reg_psr.c = (value & 0xFF00) != 0;
            cpu.reg_psr.n = (value as u8) & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
        },
        EmulationMode::Native => {
            let value = address.read_word(cpu, bus) as u32;
            let carry = cpu.reg_psr.c as u32;
            let res = (cpu.reg_a as u32).wrapping_add(carry)
                                        .wrapping_sub(1)
                                        .wrapping_add(value);
            cpu.reg_a = res as u16;
            cpu.reg_psr.c = (value & 0xFF0000) != 0;
            cpu.reg_psr.n = (value as u16) & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
        }
    }
}

/// DEX (CA) - Decrement X
fn dex(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    if mode(cpu, bus) != AddressingMode::Implied {
        panic!("DEX: Invalid AddressingMode");
    }

    cpu.reg_x = cpu.reg_x.wrapping_sub(1);

    cpu.cycles += 2;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            cpu.reg_psr.n = (cpu.reg_x as u8) & (1 << 7) != 0;
            cpu.reg_psr.z = cpu.reg_x == 0;
        },
        EmulationMode::Native => {
            cpu.reg_psr.n = cpu.reg_x & (1 << 15) != 0;
            cpu.reg_psr.z = cpu.reg_x == 0;
        }
    }
}

/// TSB (04) - Test bit
fn tsb(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let address: AddressingMode = mode(cpu, bus);

    cpu.cycles += 6;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let mask = (cpu.reg_a & 0x00FF) as u8;
            let value = address.read_byte(cpu, bus);
            cpu.reg_psr.z = value & mask == 0;
        },
        EmulationMode::Native => {
            let value = address.read_word(cpu, bus);
            cpu.reg_psr.z = value & cpu.reg_a == 0;
        }
    };
}

// Normal Jumps

/// BRL (30) - Branch Long
fn brl(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let (_, addr) = mode(cpu, bus).address(cpu, bus);

    let sign = addr & (1 << 15) == 0;
    cpu.reg_pc = if sign {
        cpu.reg_pc.wrapping_add(addr)
    } else {
        cpu.reg_pc.wrapping_sub(addr)
    };
    cpu.cycles += 4;
}

/// JSR (20, FC) - Jump
fn jsr(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let (_, addr) = mode(cpu, bus).address(cpu, bus);

    cpu.push_stack(bus, cpu.reg_pc as u8);
    cpu.push_stack(bus, (cpu.reg_pc >> 8) as u8);
    cpu.reg_pc = addr;
    cpu.cycles += 6;
}

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

// Conditional Branches

/// BMI (30) - Branch if Minus
fn bmi(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let (_, addr) = mode(cpu, bus).address(cpu, bus);
    cpu.branch(addr, cpu.reg_psr.n);
}

/// BNE (D0) - Branch if not zero
fn bne(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    let (_, addr) = mode(cpu, bus).address(cpu, bus);
    cpu.branch(addr, !cpu.reg_psr.z);
}

// Interrupts, Exceptions and Breakpoints

/// BRK (00) - Break
fn brk(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingModeFn) {
    if mode(cpu, bus) != AddressingMode::Implied {
        panic!("BRK: Invalid AddressingMode");
    }

    // TODO: Check if this is correct (pc might be incremented earlier)
    let new_reg_pc = cpu.reg_pc.wrapping_add(2);

    // TODO: Cycles?
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
        AddressingMode::ImmediateB(value) => value,
        _ => panic!("REP: Invalid AddressingMode")
    };

    let registers = cpu.reg_psr.as_u8() & !value;
    cpu.reg_psr.set_from_u8(registers);
    cpu.cycles += 3;
}

/// XCE (FB) - Swap C and E
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
    reg_d: u16,

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
        let instr_byte = self.read_byte(bus);
        Instruction::new(instr_byte).exec(self, bus);
    }

    fn read_byte(&mut self, bus: &MemoryBus) -> u8 {
        let value = bus.read(self.reg_db, self.reg_pc);
        self.reg_pc = self.reg_pc.wrapping_add(1);
        value
    }

    fn read_word(&mut self, bus: &MemoryBus) -> u16 {
        let value_lo = bus.read(self.reg_db, self.reg_pc) as u16;
        self.reg_pc = self.reg_pc.wrapping_add(1);
        let value_hi = bus.read(self.reg_db, self.reg_pc) as u16;
        self.reg_pc = self.reg_pc.wrapping_add(1);
        (value_hi << 8) | value_lo
    }

    #[inline]
    fn branch(&mut self, rel_addr: u16, condition: bool) {
        self.cycles += 2;
        if condition {
            self.cycles += 1;
            let absolute_addr = self.reg_pc.wrapping_add(rel_addr as u16);
            if (absolute_addr | 0x00FF) != (self.reg_pc | 0x00FF) {
                self.cycles += 1;
            }

            self.reg_pc = absolute_addr;
        }
    }

    fn push_stack(&mut self, bus: &mut MemoryBus, value: u8) {
        bus.write(0x00, self.reg_sp, value);
        self.reg_sp -= 1;
    }

    fn pop_stack(&mut self, bus: &mut MemoryBus) -> u8 {
        self.reg_sp += 1;
        bus.read(0x00, self.reg_sp)
    }
}

