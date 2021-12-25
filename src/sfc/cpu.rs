use super::mem::MemoryBus;

#[derive(Default, Debug)]
struct ProcessorStatusRegister(u8);

#[derive(Default, Debug)]
pub struct Core {
    reg_a: u16,
    reg_x: u16,
    reg_y: u16,

    reg_sp: u16,
    reg_pc: u16,
    reg_pb: u8,
    reg_db: u8,

    reg_psr: ProcessorStatusRegister
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

    pub fn run_cycle(&mut self, bus: &MemoryBus) {
        // TODO: Count cycles etc
        let instruction = bus.read(self.reg_pb, self.reg_pc);
        println!("{:X}", instruction);
    }
}

