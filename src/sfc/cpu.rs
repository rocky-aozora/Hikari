use super::mem::MemoryBus;


/// The data that is fetched by an `AddressingMode`.
enum Fetched {
    Nothing
}


/// An `AddressingMode` determines what data an `Instruction` is operating on.
/// They are implemented as a function on the cpu that fetches some data which the
/// instruction can use to perform its logic with.
type AddressingMode = fn(&mut Core) -> Fetched;

fn implied(_cpu: &mut Core) -> Fetched {
    Fetched::Nothing
}


/// An `Instruction` is the implementation of the 65816 CPU instruction related to
/// the opcode. An instruction performs its logic on the CPU and can use different
/// `AddressingMode`s to be able to work on different types of data.
type Instruction = fn(&mut Core, AddressingMode) -> ();

/// SEI (78) - Set interrupt disable bit
fn sei(cpu: &mut Core, mode: AddressingMode) {
    mode(cpu);
    cpu.reg_psr.i = InterruptFlag::Disabled;
    cpu.cycles += 2;
}


#[derive(Debug)]
enum InterruptFlag {
    Disabled,
    Enabled
}

impl Default for InterruptFlag {
    fn default() -> InterruptFlag {
        InterruptFlag::Enabled
    }
}

#[derive(Debug)]
enum EmulationFlag {
    Emulation,
    Native
}

impl Default for EmulationFlag {
    fn default() -> EmulationFlag {
        EmulationFlag::Emulation
    }
}

#[derive(Default, Debug)]
struct ProcessorStatusRegister {
    i: InterruptFlag,
    e: EmulationFlag
}

#[derive(Default, Debug)]
pub struct Core {
    reg_a: u16,
    reg_x: u16,
    reg_y: u16,

    reg_sp: u16,
    pub reg_pc: u16,
    pub reg_pb: u8,
    reg_db: u8,

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

    pub fn run_cycle(&mut self, bus: &MemoryBus) -> u8 {
        let op = bus.read(self.reg_pb, self.reg_pc);
        self.reg_pc += 1;

        match fetch_instruction(&op) {
            Some((instruction, address_mode)) => instruction(self, address_mode),
            None => panic!("Core: Unknown instruction {:X}", op)
        }

        self.cycles = 0;
        op
    }
}

fn fetch_instruction(op: &u8) -> Option<(Instruction, AddressingMode)> {
    match op {
        0x78 => Some((sei, implied)),
        _ => None
    }
}

