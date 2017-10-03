use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::ops::{Index, IndexMut};

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Registers {
    registers: [Option<u64>; 17],
}

impl Debug for Registers {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        for reg in &self.registers {
            match *reg {
                None => write!(fmt, " XXX")?,
                Some(x) => write!(fmt, " 0x{:x}", x)?,
            }
        }
        Ok(())
    }
}

impl Index<u8> for Registers {
    type Output = Option<u64>;

    fn index(&self, index: u8) -> &Option<u64> {
        &self.registers[index as usize]
    }
}

impl IndexMut<u8> for Registers {
    fn index_mut(&mut self, index: u8) -> &mut Option<u64> {
        &mut self.registers[index as usize]
    }
}

impl Index<DwarfRegister> for Registers {
    type Output = Option<u64>;

    fn index(&self, reg: DwarfRegister) -> &Option<u64> {
        &self[reg as u8]
    }
}

impl IndexMut<DwarfRegister> for Registers {
    fn index_mut(&mut self, reg: DwarfRegister) -> &mut Option<u64> {
        &mut self[reg as u8]
    }
}

pub enum DwarfRegister {
    SP = 7,
    IP = 16,
    
    Rax = 0,
    Rbx = 3,
    Rcx = 2,
    Rdx = 1,
    Rdi = 5,
    Rsi = 4,
    Rbp = 6,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}
