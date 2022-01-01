use std::io::{self, Write};

use crate::sfc::SuperFamicom;
use crate::sfc::cpu::Instruction;
use crate::sfc::cpu::EmulationMode;
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
                self.break_point = 0;
                break
            }
        }
    }

    fn step_emulator(&mut self) -> bool {
        let bus = self.device.bus();
        let current_pc = self.device.cpu().reg_pc;
        let current_db = self.device.cpu().reg_db;
        let addr = bus.resolve_addr(current_db, current_pc);
        let instruction = self.inspect_instr_at(addr);

        if current_pc == self.break_point {
            true
        } else {
            print!("${:02X}:{:04X}| ", current_db, current_pc);
            println!("{:?}", instruction);
            self.device.run_cycle();
            false
        }
    }

    fn inspect_emulator(&self) {
        let cpu = self.device.cpu();
        println!("CPU Status");
        println!("=======================");
        println!("A  = {:04X}", cpu.reg_a);
        println!("X  = {:04X}", cpu.reg_x);
        println!("Y  = {:04X}", cpu.reg_y);
        println!("D  = {:04X}", cpu.reg_d);
        println!("DB = {:02X}", cpu.reg_db);
        println!("-----------------------");
        println!("E = {}", if cpu.reg_psr.e == EmulationMode::Emulation { "X" } else { " " });
        print!("N = {} ", if cpu.reg_psr.n { "X" } else { " " });
        println!("V = {}", if cpu.reg_psr.v { "X" } else { " " });
        print!("M = {} ", if cpu.reg_psr.m { "X" } else { " " });
        println!("X = {}", if cpu.reg_psr.x { "X" } else { " " });
        print!("D = {} ", if cpu.reg_psr.d { "X" } else { " " });
        println!("I = {}", if cpu.reg_psr.i { "X" } else { " " });
        print!("Z = {} ", if cpu.reg_psr.z { "X" } else { " " });
        println!("C = {}", if cpu.reg_psr.c { "X" } else { " " });
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

