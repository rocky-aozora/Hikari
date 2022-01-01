use super::mem::MemoryBus;

#[derive(Debug)]
pub enum Instruction {
    Tax(AddressingMode),
    Tcd(AddressingMode),
    Tcs(AddressingMode),
    Lda(AddressingMode),
    Ldx(AddressingMode),
    Ldy(AddressingMode),
    Stz(AddressingMode),
    Sta(AddressingMode),
    Pha(AddressingMode),
    Php(AddressingMode),
    Pla(AddressingMode),
    Ora(AddressingMode),
    Adc(AddressingMode),
    Sbc(AddressingMode),
    Cmp(AddressingMode),
    Cpx(AddressingMode),
    Cpy(AddressingMode),
    Iny(AddressingMode),
    Ina(AddressingMode),
    Dex(AddressingMode),
    Tsb(AddressingMode),
    Rol(AddressingMode),
    Bra(AddressingMode),
    Brl(AddressingMode),
    Jsr(AddressingMode),
    Rti(AddressingMode),
    Bmi(AddressingMode),
    Bvs(AddressingMode),
    Bne(AddressingMode),
    Brk(AddressingMode),
    Clc(AddressingMode),
    Sei(AddressingMode),
    Rep(AddressingMode),
    Sep(AddressingMode),
    Xce(AddressingMode),
    Xba(AddressingMode)
}

impl Instruction {
    pub fn new(op: u8) -> Self {
        use Instruction::*;
        use AddressingMode::*;
        match op {
            0xAA => Tax(Implied),
            0x5B => Tcd(Implied),
            0x1B => Tcs(Implied),
            0xA9 => Lda(Immediate),
            0xB7 => Lda(DirectPageIndirectLongY),
            0xA2 => Ldx(Immediate),
            0xA0 => Ldy(Immediate),
            0xAC => Ldy(Absolute),
            0x9C => Stz(Absolute),
            0x85 => Sta(DirectPage),
            0x8D => Sta(Absolute),
            0x48 => Pha(Implied),
            0x08 => Php(Implied),
            0x68 => Pla(Implied),
            0x03 => Ora(StackRelative),
            0x69 => Adc(Immediate),
            0x79 => Adc(AbsoluteY),
            0xFF => Sbc(Long),
            0xCD => Cmp(Absolute),
            0xE0 => Cpx(Immediate),
            0xC0 => Cpy(Immediate),
            0xC8 => Iny(Implied),
            0x1A => Ina(Implied),
            0xCA => Dex(Implied),
            0x04 => Tsb(DirectPage),
            0x2A => Rol(Implied),
            0x80 => Bra(Immediate),
            0x82 => Brl(Immediate),
            0x20 => Jsr(Absolute),
            0xFC => Jsr(AbsoluteX),
            0x40 => Rti(Implied),
            0x30 => Bmi(Immediate),
            0x70 => Bvs(Immediate),
            0xD0 => Bne(Immediate),
            0x00 => Brk(Implied),
            0x18 => Clc(Implied),
            0x78 => Sei(Implied),
            0xC2 => Rep(Immediate),
            0xE2 => Sep(Immediate),
            0xFB => Xce(Implied),
            0xEB => Xba(Implied),
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
    AbsoluteY,
    DirectPage,
    DirectPageIndirectLongY,
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
            AddressingMode::AbsoluteY =>
                (cpu.reg_db, cpu.read_word(bus).wrapping_add(cpu.reg_y)),
            AddressingMode::DirectPage => {
                let addr = cpu.read_byte(bus) as u16;
                (0x00, cpu.reg_d.wrapping_add(addr as u16))
            },
            AddressingMode::DirectPageIndirectLongY => {
                let op = cpu.read_byte(bus) as u16;
                println!("op: {:02X}", op);
                println!("d:  {:04X}", cpu.reg_d);
                let index = op.wrapping_add(cpu.reg_d);
                println!("i:  {:04X}", index);
                println!("y:  {:04X}", cpu.reg_y);

                let addr_lo = bus.read(cpu.reg_db, index) as u16;
                let index = index.wrapping_add(1);
                let addr_hi = bus.read(cpu.reg_db, index) as u16;
                let index = index.wrapping_add(1);
                let addr_db = bus.read(cpu.reg_db, index);
                let addr = (addr_hi << 8) | addr_lo;
                println!("a:  {:04X}", addr);
                (addr_db, addr.wrapping_add(cpu.reg_y))
            },
            AddressingMode::StackRelative => {
                let offset = bus.read(cpu.reg_db, cpu.reg_pc) as u16;
                (cpu.reg_db, cpu.reg_sp.wrapping_add(offset))
            },
            AddressingMode::Long => (cpu.read_byte(bus), cpu.read_word(bus)),
            AddressingMode::Implied =>
                panic!("AddressingMode: attempting to address an implied address"),
            AddressingMode::Immediate =>
                panic!("AddressingMode: attempting to address an immediate value")
        }
    }
}

/// An `Instruction` is the implementation of the 65816 CPU instruction related to
/// the opcode. An instruction performs its logic on the CPU and can use different
/// `AddressingMode`s to be able to work on different types of data.
type InstructionFn = fn(&mut Core, &mut MemoryBus, AddressingMode) -> ();

/// TAX - Transfer A to X
fn tax(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("TAX: Invalid AddressingMode");
    }

    cpu.reg_x = cpu.reg_a;
    cpu.reg_psr.n = cpu.reg_a & (1 << 15) != 0;
    cpu.reg_psr.z = cpu.reg_a == 0;
    cpu.cycles += 2;
}

/// TCD - Transfer A to D
fn tcd(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("TCD: Invalid AddressingMode");
    }

    cpu.reg_d = cpu.reg_a;
    cpu.reg_psr.n = cpu.reg_a & (1 << 15) != 0;
    cpu.reg_psr.z = cpu.reg_a == 0;
    cpu.cycles += 2;
}

/// TCS - Transfer A to SP
fn tcs(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("TCS: Invalid AddressingMode");
    }

    cpu.reg_sp = cpu.reg_a;
    cpu.cycles += 2;
}

/// LDA - Load Accumulator
fn lda(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 2;
    match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
        RegisterSize::Byte => {
            let value = mode.read_byte(cpu, bus);
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_a = (cpu.reg_a & !0xFF) | value as u16;
        },
        RegisterSize::Word => {
            let value = mode.read_word(cpu, bus);
            cpu.reg_psr.n = value & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_a = value;
        }
    }
}

/// LDX - Load X
fn ldx(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 2;
    match cpu.target_reg_size(&mode, cpu.reg_psr.x) {
        RegisterSize::Byte => {
            let value = mode.read_byte(cpu, bus);
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_x = value as u16;
        },
        RegisterSize::Word => {
            let value = mode.read_word(cpu, bus);
            cpu.reg_psr.n = value & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_x = value;
        }
    }
}

/// LDY - Load Y
fn ldy(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 2;
    match cpu.target_reg_size(&mode, cpu.reg_psr.x) {
        RegisterSize::Byte => {
            let value = mode.read_byte(cpu, bus);
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_y = value as u16;
        },
        RegisterSize::Word => {
            let value = mode.read_word(cpu, bus);
            cpu.reg_psr.n = (value as u16) & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_y = value;
        }
    }
}

/// STZ - Store Zero
fn stz(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (bank, addr) = mode.address(cpu, bus);

    bus.write(bank, addr, 0);
    cpu.cycles += 4;
}

/// STA - Store Accumulator
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

/// PHA - Push A
fn pha(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("PHP: Invalid AddressingMode");
    }

    cpu.cycles += 3;
    match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
        RegisterSize::Byte => cpu.push_stack(bus, cpu.reg_a as u8),
        RegisterSize::Word=> {
            cpu.push_stack(bus, cpu.reg_a as u8);
            cpu.push_stack(bus, (cpu.reg_a >> 8) as u8);
        }
    }
}

/// PHP - Push P
fn php(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("PHP: Invalid AddressingMode");
    }

    cpu.push_stack(bus, cpu.reg_psr.as_u8());
    cpu.cycles += 3;
}

/// PLA - Pop A
fn pla(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("PHP: Invalid AddressingMode");
    }

    cpu.cycles += 4;
    match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
        RegisterSize::Byte => {
            let value = cpu.pop_stack(bus);
            cpu.reg_psr.n = value & (1 << 7) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_a = (cpu.reg_a & !0xFF) | value as u16;
        },
        RegisterSize::Word => {
            let value_hi = cpu.pop_stack(bus) as u16;
            let value_lo = cpu.pop_stack(bus) as u16;
            let value = (value_hi << 8) | value_lo;
            cpu.reg_psr.n = value & (1 << 15) != 0;
            cpu.reg_psr.z = value == 0;
            cpu.reg_a = value;
        }
    }
}

/// ORA - ALU Or
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

/// ADC - ALU Add
fn adc(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 5;
    match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
        RegisterSize::Byte => {
            let value = mode.read_byte(cpu, bus) as u16;
            let result = cpu.reg_a.wrapping_add(cpu.reg_psr.c as u16)
                                  .wrapping_add(value) as u16;

            let a_low = cpu.reg_a as u8;
            let val = value as u8;
            let res = result as u8;
            cpu.reg_a = (cpu.reg_a & !0xFF) | res as u16;
            cpu.reg_psr.v = !(a_low ^ val) & (a_low ^ res) & (1 << 7) != 0;
            cpu.reg_psr.n = res & (1 << 7) != 0;
            cpu.reg_psr.z = res == 0;
            cpu.reg_psr.c = result > 0xFF;
        },
        RegisterSize::Word => {
            let value = mode.read_byte(cpu, bus) as u32;
            let result = (cpu.reg_a as u32).wrapping_add(cpu.reg_psr.c as u32)
                                           .wrapping_add(value) as u32;

            let a = cpu.reg_a;
            let val = value as u16;
            let res = result as u16;
            cpu.reg_a = (cpu.reg_a & !0xFF) | res as u16;
            cpu.reg_psr.v = !(a ^ val) & (a ^ res) & (1 << 7) != 0;
            cpu.reg_psr.n = res & (1 << 15) != 0;
            cpu.reg_psr.z = res == 0;
            cpu.reg_psr.c = result > 0xFFFF;
        }
    }
}

/// SBC - ALU Subtract
fn sbc(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 5;
    match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
        RegisterSize::Byte => {
            let value = mode.read_byte(cpu, bus) as u16;
            let carry = cpu.reg_psr.c as u16;
            let result = (cpu.reg_a as u16).wrapping_add(carry)
                                           .wrapping_sub(1)
                                           .wrapping_add(value);
            let a_low = cpu.reg_a as u8;
            let val = value as u8;
            let res = result as u8;
            cpu.reg_a = (cpu.reg_a & !0xFF) | res as u16;
            cpu.reg_psr.v = !(a_low ^ val) & (a_low ^ res) & (1 << 7) != 0;
            cpu.reg_psr.n = res & (1 << 7) != 0;
            cpu.reg_psr.z = res == 0;
            cpu.reg_psr.c = result > 0xFF;
        },
        RegisterSize::Word => {
            let value = mode.read_word(cpu, bus) as u32;
            let carry = cpu.reg_psr.c as u32;
            let result = (cpu.reg_a as u32).wrapping_add(carry)
                                           .wrapping_sub(1)
                                           .wrapping_add(value);
            let a = cpu.reg_a;
            let val = value as u16;
            let res = result as u16;
            cpu.reg_a = (cpu.reg_a & !0xFF) | res as u16;
            cpu.reg_psr.v = !(a ^ val) & (a ^ res) & (1 << 7) != 0;
            cpu.reg_psr.n = res & (1 << 15) != 0;
            cpu.reg_psr.z = res == 0;
            cpu.reg_psr.c = result > 0xFFFF;
        }
    }
}

/// CMP - Compare
fn cmp(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
   match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
       RegisterSize::Byte => {
            let b = mode.read_byte(cpu, bus);
            let a = cpu.reg_a as u8;
            cpu.reg_psr.c = a >= b;
            cpu.reg_psr.z = a == b;
            cpu.reg_psr.n = a.wrapping_sub(b) & (1 << 7) != 0;
       },
       RegisterSize::Word => {
            let b = mode.read_word(cpu, bus);
            let a = cpu.reg_a;
            cpu.reg_psr.c = a >= b;
            cpu.reg_psr.z = a == b;
            cpu.reg_psr.n = a.wrapping_sub(b) & (1 << 15) != 0;
       }
   }
}

/// CPX - Compare
fn cpx(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
   match cpu.target_reg_size(&mode, cpu.reg_psr.x) {
       RegisterSize::Byte => {
            let b = mode.read_byte(cpu, bus);
            let a = cpu.reg_x as u8;
            cpu.reg_psr.c = a >= b;
            cpu.reg_psr.z = a == b;
            cpu.reg_psr.n = a.wrapping_sub(b) & (1 << 7) != 0;
       },
       RegisterSize::Word => {
            let b = mode.read_word(cpu, bus);
            let a = cpu.reg_x;
            cpu.reg_psr.c = a >= b;
            cpu.reg_psr.z = a == b;
            cpu.reg_psr.n = a.wrapping_sub(b) & (1 << 15) != 0;
       }
   }
}

/// CPY - Compare
fn cpy(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
   match cpu.target_reg_size(&mode, cpu.reg_psr.x) {
       RegisterSize::Byte => {
            let b = mode.read_byte(cpu, bus);
            let a = cpu.reg_y as u8;
            cpu.reg_psr.c = a >= b;
            cpu.reg_psr.z = a == b;
            cpu.reg_psr.n = a.wrapping_sub(b) & (1 << 7) != 0;
       },
       RegisterSize::Word => {
            let b = mode.read_word(cpu, bus);
            let a = cpu.reg_y;
            cpu.reg_psr.c = a >= b;
            cpu.reg_psr.z = a == b;
            cpu.reg_psr.n = a.wrapping_sub(b) & (1 << 15) != 0;
       }
   }
}

/// INY - Increment Y
fn iny(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("INY: Invalid AddressingMode");
    }

    cpu.cycles += 2;
    match cpu.target_reg_size(&mode, cpu.reg_psr.x) {
        RegisterSize::Byte => {
            cpu.reg_y = (cpu.reg_y as u8).wrapping_add(1) as u16;
            cpu.reg_psr.n = (cpu.reg_y as u8) & (1 << 7) != 0;
            cpu.reg_psr.z = cpu.reg_y == 0;
        },
        RegisterSize::Word => {
            cpu.reg_y = cpu.reg_y.wrapping_add(1);
            cpu.reg_psr.n = cpu.reg_y & (1 << 15) != 0;
            cpu.reg_psr.z = cpu.reg_y == 0;
        }
    }
}

/// INA - Increment A
fn ina(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("INY: Invalid AddressingMode");
    }

    cpu.cycles += 2;
    match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
        RegisterSize::Byte => {
            cpu.reg_a = (cpu.reg_a as u8).wrapping_add(1) as u16;
            cpu.reg_psr.n = (cpu.reg_a as u8) & (1 << 7) != 0;
            cpu.reg_psr.z = cpu.reg_a == 0;
        },
        RegisterSize::Word => {
            cpu.reg_a = cpu.reg_a.wrapping_add(1);
            cpu.reg_psr.n = cpu.reg_a & (1 << 15) != 0;
            cpu.reg_psr.z = cpu.reg_a == 0;
        }
    }
}

/// DEX - Decrement X
fn dex(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("DEX: Invalid AddressingMode");
    }

    cpu.cycles += 2;
    match cpu.target_reg_size(&mode, cpu.reg_psr.x) {
        RegisterSize::Byte => {
            cpu.reg_x = (cpu.reg_x as u8).wrapping_sub(1) as u16;
            cpu.reg_psr.n = (cpu.reg_x as u8) & (1 << 7) != 0;
            cpu.reg_psr.z = cpu.reg_x == 0;
        },
        RegisterSize::Word => {
            cpu.reg_x = cpu.reg_x.wrapping_sub(1);
            cpu.reg_psr.n = cpu.reg_x & (1 << 15) != 0;
            cpu.reg_psr.z = cpu.reg_x == 0;
        }
    }
}

/// TSB - Test bit
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

/// ROL - Rotate Left
fn rol(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    cpu.cycles += 2;
    match cpu.target_reg_size(&mode, cpu.reg_psr.m) {
        RegisterSize::Byte => {
            if mode == AddressingMode::Implied {
                let a = cpu.reg_a as u8;
                let c = cpu.reg_psr.c as u8;
                let res = ((a << 1) | c) as u16;
                cpu.reg_a = (cpu.reg_a & !0xFF) | res;
                cpu.reg_psr.c = a & (1 << 7) != 0;
                cpu.reg_psr.n = cpu.reg_a & (1 << 7) != 0;
                cpu.reg_psr.z = (cpu.reg_a as u8) == 0;
            } else {
                unimplemented!();
            }
        },
        RegisterSize::Word => {
            if mode == AddressingMode::Implied {
                let c = cpu.reg_psr.c as u16;
                cpu.reg_psr.c = cpu.reg_a & (1 << 7) != 0;
                cpu.reg_a = (cpu.reg_a << 1) | c;
                cpu.reg_psr.n = cpu.reg_a & (1 << 15) != 0;
                cpu.reg_psr.z = cpu.reg_a == 0;
            } else {
                unimplemented!();
            }
        }
    }
}

/// BRA - Branch
fn bra(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let addr = mode.read_byte(cpu, bus) as i8;
    cpu.reg_pc = cpu.reg_pc.wrapping_add(addr as u16);
    cpu.cycles += 4;
}

/// BRL - Branch Long
fn brl(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let addr = mode.read_word(cpu, bus) as i16;
    cpu.reg_pc = cpu.reg_pc.wrapping_add(addr as u16);
    cpu.cycles += 4;
}

/// JSR - Jump
fn jsr(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let (_, addr) = mode.address(cpu, bus);

    cpu.push_stack(bus, cpu.reg_pc as u8);
    cpu.push_stack(bus, (cpu.reg_pc >> 8) as u8);
    cpu.reg_pc = addr;
    cpu.cycles += 6;
}

/// RTI - Return from Interrupt
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

/// BMI - Branch if Minus
fn bmi(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let addr = mode.read_byte(cpu, bus);
    cpu.branch(addr, cpu.reg_psr.n);
}

/// BVS - Branch if overflow
fn bvs(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let addr = mode.read_byte(cpu, bus);
    cpu.branch(addr, cpu.reg_psr.v);
}

/// BNE - Branch if not zero
fn bne(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let addr = mode.read_byte(cpu, bus);
    cpu.branch(addr, !cpu.reg_psr.z);
}

/// BRK - Break
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

/// CLC - Clear carry flag
fn clc(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("CLC: Invalid AddressingMode");
    }

    cpu.reg_psr.c = false;
    cpu.cycles += 2;
}

/// SEI - Set interrupt disable bit
fn sei(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("SEI: Invalid AddressingMode");
    }

    cpu.reg_psr.i = true;
    cpu.cycles += 2;
}

/// REP - Reset Status Bits
fn rep(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let value = mode.read_byte(cpu, bus);
    let registers = cpu.reg_psr.as_u8() & !value;
    cpu.reg_psr.set_from_u8(registers);
    if cpu.reg_psr.x {
        cpu.reg_x = cpu.reg_x & 0x00FF;
        cpu.reg_y = cpu.reg_y & 0x00FF;
    }
    cpu.cycles += 3;
}

/// SEP - Reset Status Bits
fn sep(cpu: &mut Core, bus: &mut MemoryBus, mode: AddressingMode) {
    let value = mode.read_byte(cpu, bus);
    let registers = cpu.reg_psr.as_u8() | value;
    cpu.reg_psr.set_from_u8(registers);
    if cpu.reg_psr.x {
        cpu.reg_x = cpu.reg_x & 0x00FF;
        cpu.reg_y = cpu.reg_y & 0x00FF;
    }
    cpu.cycles += 3;
}

/// XCE - Swap C and E
fn xce(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("SEI: Invalid AddressingMode");
    }

    use EmulationMode::*;
    let temp = cpu.reg_psr.c;
    cpu.reg_psr.c = cpu.reg_psr.e == Emulation;
    cpu.reg_psr.e = if temp { Emulation } else { Native };
    cpu.cycles += 2;
}

/// XBA - Swap A and B
fn xba(cpu: &mut Core, _: &mut MemoryBus, mode: AddressingMode) {
    if mode != AddressingMode::Implied {
        panic!("SEI: Invalid AddressingMode");
    }

    let lo = cpu.reg_a & 0xFF;
    let hi = cpu.reg_a >> 8;
    cpu.reg_a = (lo << 8) | hi;
    cpu.reg_psr.n = hi & (1 << 7) != 0;
    cpu.reg_psr.z = hi == 0;
    cpu.cycles += 2;
}

#[derive(PartialEq, Debug)]
pub enum EmulationMode {
    Emulation,
    Native
}

impl Default for EmulationMode {
    fn default() -> EmulationMode {
        EmulationMode::Emulation
    }
}

#[derive(Debug)]
pub struct ProcessorStatusRegister {
    pub c: bool, // Carry
    pub z: bool, // Zero
    pub i: bool, // Interrupt disable
    pub d: bool, // Decimal mode
    pub x: bool, // Index flag (Break flag in emulation mode)
    pub m: bool, // Memory flag
    pub v: bool, // Overflow
    pub n: bool, // Negative (Sign)
    pub e: EmulationMode, // 6502 Emulation mode
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
        if self.e == EmulationMode::Native {
            self.x = flags & 0b0001_0000 != 0;
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
            x: true,
            m: true,
            v: false,
            n: false,
            e: EmulationMode::Emulation
        }
    }
}

enum RegisterSize {
    Byte,
    Word
}

#[derive(Default, Debug)]
pub struct Core {
    pub reg_a: u16,
    pub reg_x: u16,
    pub reg_y: u16,

    reg_sp: u16,
    pub reg_pc: u16,
    pub reg_db: u8,
    pub reg_d: u16,

    pub reg_psr: ProcessorStatusRegister,
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
            Instruction::Tax(mode) => tax(self, bus, mode),
            Instruction::Tcd(mode) => tcd(self, bus, mode),
            Instruction::Tcs(mode) => tcs(self, bus, mode),
            Instruction::Lda(mode) => lda(self, bus, mode),
            Instruction::Ldx(mode) => ldx(self, bus, mode),
            Instruction::Ldy(mode) => ldy(self, bus, mode),
            Instruction::Stz(mode) => stz(self, bus, mode),
            Instruction::Sta(mode) => sta(self, bus, mode),
            Instruction::Pha(mode) => pha(self, bus, mode),
            Instruction::Php(mode) => php(self, bus, mode),
            Instruction::Pla(mode) => pla(self, bus, mode),
            Instruction::Ora(mode) => ora(self, bus, mode),
            Instruction::Adc(mode) => adc(self, bus, mode),
            Instruction::Sbc(mode) => sbc(self, bus, mode),
            Instruction::Cmp(mode) => cmp(self, bus, mode),
            Instruction::Cpx(mode) => cpx(self, bus, mode),
            Instruction::Cpy(mode) => cpy(self, bus, mode),
            Instruction::Iny(mode) => iny(self, bus, mode),
            Instruction::Ina(mode) => ina(self, bus, mode),
            Instruction::Dex(mode) => dex(self, bus, mode),
            Instruction::Tsb(mode) => tsb(self, bus, mode),
            Instruction::Rol(mode) => rol(self, bus, mode),
            Instruction::Bra(mode) => bra(self, bus, mode),
            Instruction::Brl(mode) => brl(self, bus, mode),
            Instruction::Jsr(mode) => jsr(self, bus, mode),
            Instruction::Rti(mode) => rti(self, bus, mode),
            Instruction::Bmi(mode) => bmi(self, bus, mode),
            Instruction::Bvs(mode) => bvs(self, bus, mode),
            Instruction::Bne(mode) => bne(self, bus, mode),
            Instruction::Brk(mode) => brk(self, bus, mode),
            Instruction::Clc(mode) => clc(self, bus, mode),
            Instruction::Sei(mode) => sei(self, bus, mode),
            Instruction::Rep(mode) => rep(self, bus, mode),
            Instruction::Sep(mode) => sep(self, bus, mode),
            Instruction::Xce(mode) => xce(self, bus, mode),
            Instruction::Xba(mode) => xba(self, bus, mode)
        }
    }

    #[inline(always)]
    fn read_byte(&mut self, bus: &MemoryBus) -> u8 {
        let value = bus.read(self.reg_db, self.reg_pc);
        self.reg_pc = self.reg_pc.wrapping_add(1);
        value
    }

    #[inline(always)]
    fn read_word(&mut self, bus: &MemoryBus) -> u16 {
        let value_lo = bus.read(self.reg_db, self.reg_pc) as u16;
        self.reg_pc = self.reg_pc.wrapping_add(1);
        let value_hi = bus.read(self.reg_db, self.reg_pc) as u16;
        self.reg_pc = self.reg_pc.wrapping_add(1);
        (value_hi << 8) | value_lo
    }

    #[inline]
    fn branch(&mut self, operand: u8, condition: bool) {
        let rel_addr = operand as i8;
        self.cycles += 2;
        if condition {
            self.cycles += 1;
            let addr = self.reg_pc.wrapping_add(rel_addr as u16);
            if (addr | 0x00FF) != (self.reg_pc | 0x00FF) {
                self.cycles += 1;
            }

            self.reg_pc = addr;
        }
    }

    #[inline]
    fn target_reg_size(&self, mode: &AddressingMode, target_flag: bool) -> RegisterSize {
        match mode {
            AddressingMode::Immediate | AddressingMode::Implied => match target_flag {
                true => RegisterSize::Byte,
                false => RegisterSize::Word
            },
            _ => match self.reg_psr.e {
                EmulationMode::Emulation => RegisterSize::Byte,
                EmulationMode::Native => RegisterSize::Word
            }
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

