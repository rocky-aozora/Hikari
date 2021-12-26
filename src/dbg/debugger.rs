use std::io::{self, Write};

use crate::sfc::SuperFamicom;


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
        loop { self.device.run_cycle(); }
    }

    fn step_emulator(&mut self) {
        let current_pc = format!("Executing at {:X}:", self.device.cpu().reg_pc);
        let instruction = self.device.run_cycle();

        // TODO: Dissasemle
        println!("{} {:X}", current_pc, instruction);
    }

    fn inspect_emulator(&self) {
        println!("{:#?}", self.device.cpu());
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

