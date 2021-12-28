use std::io::{self, Write};

use crate::sfc::SuperFamicom;
use crate::sfc::cpu::{Instruction, Opcode};
use crate::sfc::mem::Addr;


pub enum Command {
    Continue,
    Step,
    Inspect,
    Quit,
    Invalid,
    Empty
}

pub struct Debugger {
    device: SuperFamicom
}

impl Debugger {
    pub fn new(mut device: SuperFamicom) -> Debugger {
        device.initialize();
        Debugger { device }
    }

    pub fn repl(&mut self) {
        'main: loop {
            print!("hikari-dbg> ");
            io::stdout().flush().unwrap();
            let command = parse(read());

            match command {
                Command::Continue => self.continue_emulator(),
                Command::Step => self.step_emulator(),
                Command::Inspect => self.inspect_emulator(),
                Command::Quit => { println!(); break 'main; },
                Command::Invalid => println!("Unknown command"),
                Command::Empty => {}
            }
        }
    }

    fn continue_emulator(&mut self) {
        // TODO: Implement breakpoints
        loop { self.step_emulator() }
    }

    fn step_emulator(&mut self) {
        let current_pc = self.device.cpu().reg_pc;
        let current_db = self.device.cpu().reg_db;
        let addr = self.device.bus().resolve_addr(current_db, current_pc);
        let instr = self.inspect_instr_at(addr);

        let full_addr: u32 = ((current_db as u32) << 16) | current_pc as u32;
        let instr_dbg = format!("{:?}", instr);
        println!("${:06X}: {}", full_addr, instr_dbg.to_uppercase());

        self.device.run_cycle();
    }

    fn inspect_emulator(&self) {
        println!("{:#?}", self.device.cpu());
    }

    fn inspect_instr_at(&self, addr: Addr) -> Opcode {
        let data = match addr {
            Addr::RomSel(bank, addr) =>
                self.device.bus().cart().read(bank, addr),
            _ => panic!("Debugger: cannot inspect address: {:?}", addr)
        };

        Instruction::new(data).op
    }
}

fn parse(input: String) -> Command {
    match input.as_str() {
        "c" | "continue" => Command::Continue,
        "s" | "step" => Command::Step,
        "i" | "inspect" => Command::Inspect,
        "q" | "quit" | "^D" => Command::Quit,
        "" => Command::Empty,
        _ => Command::Invalid
    }
}

fn read() -> String {
    let mut input = String::new();
    let bytes = io::stdin().read_line(&mut input).unwrap();
    if bytes == 0 { return String::from("^D"); }

    input.trim().into()
}

