use std::io::{self, Write};

use crate::sfc::SuperFamicom;
use crate::sfc::cpu::Instruction;
use crate::sfc::mem::Addr;


pub enum Command {
    BreakPoint(u16),
    Continue,
    Step,
    Inspect,
    Header,
    Quit,
    Invalid,
    Empty
}

pub struct Debugger {
    device: SuperFamicom,
    break_point: u16
}

impl Debugger {
    pub fn new(mut device: SuperFamicom) -> Debugger {
        device.initialize();
        Debugger { device, break_point: 0 }
    }

    pub fn repl(&mut self) {
        'main: loop {
            print!("hikari-dbg> ");
            io::stdout().flush().unwrap();
            let command = parse(read());

            match command {
                Command::BreakPoint(addr) => self.set_breakpoint(addr),
                Command::Continue => self.continue_emulator(),
                Command::Step => { self.step_emulator(); },
                Command::Inspect => self.inspect_emulator(),
                Command::Header => self.print_header(),
                Command::Quit => { println!(); break 'main; },
                Command::Invalid => println!("Unknown command"),
                Command::Empty => {}
            }
        }
    }

    fn set_breakpoint(&mut self, addr: u16) {
        self.break_point = addr;
    }

    fn continue_emulator(&mut self) {
        loop {
            if self.step_emulator() {
                break
            }
        }
    }

    fn step_emulator(&mut self) -> bool {
        let current_pc = self.device.cpu().reg_pc;
        let current_db = self.device.cpu().reg_db;
        let addr = self.device.bus().resolve_addr(current_db, current_pc);
        let instr = self.inspect_instr_at(addr);

        let full_addr: u32 = ((current_db as u32) << 16) | current_pc as u32;
        println!("${:06X}: {:?}", full_addr, instr);

        if current_pc == self.break_point {
            true
        } else {
            self.device.run_cycle();
            false
        }
    }

    fn inspect_emulator(&self) {
        println!("{:#?}", self.device.cpu());
    }

    fn print_header(&self) {
        self.device.bus().cart().print_header();
    }

    fn inspect_instr_at(&self, addr: Addr) -> Instruction {
        let op = match addr {
            Addr::RomSel(bank, addr) =>
                self.device.bus().cart().read(bank, addr),
            _ => panic!("Debugger: cannot inspect address: {:?}", addr)
        };

        Instruction::new(op)
    }
}

fn parse(input: String) -> Command {
    match input.as_str() {
        "b" | "breakpoint" => Command::BreakPoint(u16::from_str_radix(&read(), 16).unwrap()),
        "c" | "continue" => Command::Continue,
        "s" | "step" => Command::Step,
        "i" | "inspect" => Command::Inspect,
        "h" | "header" => Command::Header,
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

