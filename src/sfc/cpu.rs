use super::mem::MemoryBus;

#[derive(Debug)]
pub enum Instruction {
    Tcd(AddressingMode),
    Tcs(AddressingMode),
    Lda(AddressingMode),
    Ldx(AddressingMode),
    Ldy(AddressingMode),
    Stz(AddressingMode),
    Sta(AddressingMode),
    Ora(AddressingMode),
    Sbc(AddressingMode),
    Dex(AddressingMode),
    Tsb(AddressingMode),
    Brl(AddressingMode),
    Jsr(AddressingMode),
    Rti(AddressingMode),
    Bmi(AddressingMode),
    Bne(AddressingMode),
    Brk(AddressingMode),
    Clc(AddressingMode),
    Sei(AddressingMode),
    Rep(AddressingMode),
    Sep(AddressingMode),
    Xce(AddressingMode)
}

impl Instruction {
    pub fn new(op: u8) -> Self {
        use AddressingMode::*;
        match op {
            0x5B => Instruction::Tcd(Implied),
            0x1B => Instruction::Tcs(Implied),
            0xA9 => Instruction::Lda(Immediate),
            0xA2 => Instruction::Ldx(Immediate),
            0xAC => Instruction::Ldy(Absolute),
            0x9C => Instruction::Stz(Absolute),
            0x85 => Instruction::Sta(DirectPage),
            0x8D => Instruction::Sta(Absolute),
            0x03 => Instruction::Ora(StackRelative),
            0xFF => Instruction::Sbc(Long),
            0xCA => Instruction::Dex(Implied),
            0x04 => Instruction::Tsb(DirectPage),
            0x82 => Instruction::Brl(Absolute),
            0x20 => Instruction::Jsr(Absolute),
            0xFC => Instruction::Jsr(AbsoluteX),
            0x40 => Instruction::Rti(Implied),
            0x30 => Instruction::Bmi(Immediate),
            0xD0 => Instruction::Bne(Immediate),
            0x00 => Instruction::Brk(Implied),
            0x18 => Instruction::Clc(Implied),
            0x78 => Instruction::Sei(Implied),
            0xC2 => Instruction::Rep(Immediate),
            0xE2 => Instruction::Sep(Immediate),
            0xFB => Instruction::Xce(Implied),
            _ => panic!("Unimplemented instruction {:X}", op)
        }
    }
}


/// An `AddressingMode` determines what data an `Instruction` is operating on.
/// They are implemented as a function (`AddressingModeFn`) on the cpu that
/// fetches some data which the instruction can use to perform its logic with.
#[derive(Debug, PartialEq)]
pub enum AddressingMode {
    Immediate,
    Absolute,
    AbsoluteX,
    DirectPage,
    StackRelative,
    Long,
    Implied
}

impl AddressingMode {
    fn read_byte(self, cpu: &mut Core, bus: &MemoryBus) -> u8 {
        match self {
            AddressingMode::Immediate => cpu.read_byte(bus),
            _ => {
                let (bank, addr) = self.address(cpu, bus);
                bus.read(bank, addr)
            }
        }
    }


    fn read_word(self, cpu: &mut Core, bus: &MemoryBus) -> u16 {
        match self {
            AddressingMode::Immediate => cpu.read_word(bus),
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
            AddressingMode::Absolute => (cpu.reg_db, cpu.read_word(bus)),
            AddressingMode::AbsoluteX =>
                (cpu.reg_db, cpu.read_word(bus).wrapping_add(cpu.reg_x)),
            AddressingMode::DirectPage => {
                let addr = cpu.read_byte(bus) as u16;
                (0x00, cpu.reg_d.wrapping_add(addr as u16))
            },
            AddressingMode::StackRelative => {
                let offset = bus.read(cpu.reg_db, cpu.reg_pc) as u16;
                (cpu.reg_db, cpu.reg_sp.wrapping_add(offset))
            },
            AddressingMode::Long => (cpu.read_byte(bus), cpu.read_word(bus)),
            AddressingMode::Implied =>
                panic!("AddressingMode: attempting to address an implied address"),
            AddressingMode::Immediate =>
                panic!("AddressingMode: attempting to addres an immediate value")
        }
    }
}


/// An `Instruction` is the implementation of the 65816 CPU instruction related to
/// the opcode. An instruction performs its logic on the CPU and can use different
/// `AddressingMode`s to be able to work on different types of data.
type InstructionFn = fn(&mut Core, &mut MemoryBus, AddressingMode) -> ();

// Register to Register Transfer

/// TCD (5B) - Transfer A to D
fn tcd(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("TCD: Invalid AddressingMode");
    }

    cpu.reg_d = cpu.reg_a;
    cpu.reg_psr.n = cpu.reg_a & (1 << 15) != 0;
    cpu.reg_psr.z = cpu.reg_a == 0;
    cpu.cycles += 2;
}

/// TCS (1B) - Transfer A to SP
fn tcs(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("TCS: Invalid AddressingMode");
    }

    cpu.reg_sp = cpu.reg_a;
    cpu.cycles += 2;
}

// Load Register from Memory

/// LDA (A9) - Load Accumulator
fn lda(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 2;
    match mode {
        AddressingMode::Immediate => match cpu.reg_psr.m {
            true => {
                let value = mode.read_byte(cpu, bus);
                cpu.reg_psr.n = value & (1 << 7) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_a = (cpu.reg_a & !0xFF) | value as u16;
            },
            false => {
                let value = mode.read_word(cpu, bus);
                cpu.reg_psr.n = value & (1 << 15) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_a = value;
            }
        },
        _ => match cpu.reg_psr.e {
            EmulationMode::Emulation => {
                let value = mode.read_byte(cpu, bus);
                cpu.reg_psr.n = value & (1 << 7) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_a = (cpu.reg_a & !0xFF) | value as u16;
            },
            EmulationMode::Native => {
                let value = mode.read_word(cpu, bus);
                cpu.reg_psr.n = value & (1 << 15) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_a = value;
            }
        }
    }
}

/// LDX (A2) - Load X
fn ldx(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 2;
    match mode {
        AddressingMode::Immediate => match cpu.reg_psr.m {
            true => {
                let value = mode.read_byte(cpu, bus);
                cpu.reg_psr.n = value & (1 << 7) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_x = (cpu.reg_x & !0xFF) | value as u16;
            },
            false => {
                let value = mode.read_word(cpu, bus);
                cpu.reg_psr.n = value & (1 << 15) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_x = value;
            }
        },
        _ => match cpu.reg_psr.e {
            EmulationMode::Emulation => {
                let value = mode.read_byte(cpu, bus);
                cpu.reg_psr.n = value & (1 << 7) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_x = (cpu.reg_x & !0xFF) | value as u16;
            },
            EmulationMode::Native => {
                let value = mode.read_word(cpu, bus);
                cpu.reg_psr.n = value & (1 << 15) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_x = value;
            }
        }
    }
}

/// LDY (AC) - Load Y
fn ldy(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 2;
    match mode {
        AddressingMode::Immediate => match cpu.reg_psr.x {
            true => {
                let value = mode.read_byte(cpu, bus);
                cpu.reg_psr.n = value & (1 << 7) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_y = (cpu.reg_y & !0xFF) | value as u16;
            },
            false => {
                let value = mode.read_word(cpu, bus);
                cpu.reg_psr.n = (value as u16) & (1 << 15) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_y = value;
            }
        },
        _ => match cpu.reg_psr.e {
            EmulationMode::Emulation => {
                let value = mode.read_byte(cpu, bus);
                cpu.reg_psr.n = value & (1 << 7) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_y = (cpu.reg_y & !0xFF) | value as u16;
            },
            EmulationMode::Native => {
                let value = mode.read_word(cpu, bus);
                cpu.reg_psr.n = (value as u16) & (1 << 15) != 0;
                cpu.reg_psr.z = value == 0;
                cpu.reg_y = value;
            }
        }
    }
}

// Store Register in Memory

/// STZ (9C) - Store Zero
fn stz(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (bank, addr) = mode.address(cpu, bus);

    bus.write(bank, addr, 0);
    cpu.cycles += 4;
}

/// STA (8D) - Store Accumulator
fn sta(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (bank, addr) = mode.address(cpu, bus);

    cpu.cycles += 4;
    match cpu.reg_psr.x {
        true => {
            bus.write(bank, addr, cpu.reg_a as u8);
        },
        false => {
            // TODO: Figure out if this is correct
            bus.write(bank, addr, cpu.reg_a as u8);
            let addr = addr.wrapping_add(1);
            bus.write(bank, addr, (cpu.reg_a >> 8) as u8);
        }
    }
}

// Arithmetic/Logical operations

/// ORA (03, 01) - ALU Or
fn ora(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 6;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = mode.read_byte(cpu, bus);
            cpu.reg_a = (cpu.reg_a as u8 | value) as u16;
        },
        EmulationMode::Native => {
            let value = mode.read_word(cpu, bus);
            cpu.reg_a = cpu.reg_a | value;
        }
    }
}

/// SBC (FF) - ALU Subtract
fn sbc(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 5;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let value = mode.read_byte(cpu, bus) as u16;
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
            let value = mode.read_word(cpu, bus) as u32;
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
fn dex(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
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
fn tsb(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 6;
    match cpu.reg_psr.e {
        EmulationMode::Emulation => {
            let mask = (cpu.reg_a & 0x00FF) as u8;
            let value = mode.read_byte(cpu, bus);
            cpu.reg_psr.z = value & mask == 0;
        },
        EmulationMode::Native => {
            let value = mode.read_word(cpu, bus);
            cpu.reg_psr.z = value & cpu.reg_a == 0;
        }
    };
}

// Normal Jumps

/// BRL (30) - Branch Long
fn brl(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (_, addr) = mode.address(cpu, bus);

    let sign = addr & (1 << 15) == 0;
    cpu.reg_pc = if sign {
        cpu.reg_pc.wrapping_add(addr)
    } else {
        cpu.reg_pc.wrapping_sub(addr)
    };
    cpu.cycles += 4;
}

/// JSR (20, FC) - Jump
fn jsr(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (_, addr) = mode.address(cpu, bus);

    cpu.push_stack(bus, cpu.reg_pc as u8);
    cpu.push_stack(bus, (cpu.reg_pc >> 8) as u8);
    cpu.reg_pc = addr;
    cpu.cycles += 6;
}

/// RTI (40) - Return from Interrupt
fn rti(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
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
fn bmi(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (_, addr) = mode.address(cpu, bus);
    cpu.branch(addr, cpu.reg_psr.n);
}

/// BNE (D0) - Branch if not zero
fn bne(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (_, addr) = mode.address(cpu, bus);
    cpu.branch(addr, !cpu.reg_psr.z);
}

// Interrupts, Exceptions and Breakpoints

/// BRK (00) - Break
fn brk(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("BRK: Invalid AddressingMode");
    }

    // TODO: Check if this is correct (pc might be incremented earlier)
    let new_reg_pc = cpu.reg_pc.wrapping_add(2);

    // TODO: Cycles?
    cpu.reg_psr.x = true;
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
fn clc(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("CLC: Invalid AddressingMode");
    }

    cpu.reg_psr.c = false;
    cpu.cycles += 2;
}

/// SEI (78) - Set interrupt disable bit
fn sei(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("SEI: Invalid AddressingMode");
    }

    cpu.reg_psr.i = true;
    cpu.cycles += 2;
}

/// REP (C2) - Reset Status Bits
fn rep(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let value = mode.read_byte(cpu, bus);
    let registers = cpu.reg_psr.as_u8() & !value;
    cpu.reg_psr.set_from_u8(registers);
    cpu.cycles += 3;
}

/// SEP (E2) - Reset Status Bits
fn sep(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let value = mode.read_byte(cpu, bus);
    let registers = cpu.reg_psr.as_u8() | value;
    cpu.reg_psr.set_from_u8(registers);
    cpu.cycles += 3;
}

/// XCE (FB) - Swap C and E
fn xce(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("SEI: Invalid AddressingMode");
    }

    use EmulationMode::*;
    let temp = cpu.reg_psr.c;
    cpu.reg_psr.c = cpu.reg_psr.e == Emulation;
    cpu.reg_psr.e = if temp { Emulation } else { Native };
    cpu.reg_psr.m = temp;
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
    x: bool, // Index flag (Break flag in emulation mode)
    m: bool, // Memory flag
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
        ((self.x as u8) << 4) +
        ((self.m as u8) << 5) +
        ((self.v as u8) << 6) +
        ((self.n as u8) << 7)
    }

    fn set_from_u8(&mut self, flags: u8) {
        self.c = flags & 0b0000_0001 != 0;
        self.z = flags & 0b0000_0010 != 0;
        self.i = flags & 0b0000_0100 != 0;
        self.d = flags & 0b0000_1000 != 0;
        self.x = flags & 0b0001_0000 != 0;
        if self.e == EmulationMode::Native {
            self.m = flags & 0b0010_0000 != 0;
        }
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
            x: false,
            m: true,
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
        match Instruction::new(self.read_byte(bus)) {
            Instruction::Tcd(mode) => tcd(self, bus, mode),
            Instruction::Tcs(mode) => tcs(self, bus, mode),
            Instruction::Lda(mode) => lda(self, bus, mode),
            Instruction::Ldx(mode) => ldx(self, bus, mode),
            Instruction::Ldy(mode) => ldy(self, bus, mode),
            Instruction::Stz(mode) => stz(self, bus, mode),
            Instruction::Sta(mode) => sta(self, bus, mode),
            Instruction::Ora(mode) => ora(self, bus, mode),
            Instruction::Sbc(mode) => sbc(self, bus, mode),
            Instruction::Dex(mode) => dex(self, bus, mode),
            Instruction::Tsb(mode) => tsb(self, bus, mode),
            Instruction::Brl(mode) => brl(self, bus, mode),
            Instruction::Jsr(mode) => jsr(self, bus, mode),
            Instruction::Rti(mode) => rti(self, bus, mode),
            Instruction::Bmi(mode) => bmi(self, bus, mode),
            Instruction::Bne(mode) => bne(self, bus, mode),
            Instruction::Brk(mode) => brk(self, bus, mode),
            Instruction::Clc(mode) => clc(self, bus, mode),
            Instruction::Sei(mode) => sei(self, bus, mode),
            Instruction::Rep(mode) => rep(self, bus, mode),
            Instruction::Sep(mode) => sep(self, bus, mode),
            Instruction::Xce(mode) => xce(self, bus, mode)
        }
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

