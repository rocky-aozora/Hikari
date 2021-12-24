#[derive(Default, Debug)]
struct ProcessorStatusRegister(u8);

#[derive(Default, Debug)]
pub struct Core {
    reg_a: u16,
    reg_x: u16,
    reg_y: u16,

    reg_sp: u16,
    reg_pc: u16,

    reg_psr: ProcessorStatusRegister
}

impl Core {
    pub fn new() -> Core {
        Core::default()
    }
}

