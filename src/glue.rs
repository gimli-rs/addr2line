use registers::{Registers, DwarfRegister};
use libunwind_shim::_Unwind_Exception;
use find_cfi::find_cfi_sections;

extern "C" {
    pub fn unwind_trampoline(exception: *mut _Unwind_Exception);
    fn unwind_lander(regs: *const LandingRegisters);
}

#[repr(C)]
struct LandingRegisters {
    rax: u64,
    rbx: u64,
    rcx: u64,
    rdx: u64,
    rdi: u64,
    rsi: u64,
    rbp: u64,
    r8: u64,
    r9: u64,
    r10: u64,
    r11: u64,
    r12: u64,
    r13: u64,
    r14: u64,
    r15: u64,
    rsp: u64,
    // rflags? cs,fs,gs?
}

#[repr(C)]
pub struct SavedRegs {
    pub r15: u64,
    pub r14: u64,
    pub r13: u64,
    pub r12: u64,
    pub rbx: u64,
    pub rbp: u64,
}

#[no_mangle]
pub unsafe extern "C" fn unwind_recorder(exception: *mut _Unwind_Exception, stack: u64, saved_regs: *mut SavedRegs) {
    let cfi = find_cfi_sections();
    ::do_laundry(&cfi, stack, &*saved_regs, exception);
}

pub unsafe fn land(regs: &Registers) {
    let mut lr = LandingRegisters {
        rax: regs[DwarfRegister::Rax].unwrap_or(0),
        rbx: regs[DwarfRegister::Rbx].unwrap_or(0),
        rcx: regs[DwarfRegister::Rcx].unwrap_or(0),
        rdx: regs[DwarfRegister::Rdx].unwrap_or(0),
        rdi: regs[DwarfRegister::Rdi].unwrap_or(0),
        rsi: regs[DwarfRegister::Rsi].unwrap_or(0),
        rbp: regs[DwarfRegister::Rbp].unwrap_or(0),
        r8:  regs[DwarfRegister::R8 ].unwrap_or(0),
        r9:  regs[DwarfRegister::R9 ].unwrap_or(0),
        r10: regs[DwarfRegister::R10].unwrap_or(0),
        r11: regs[DwarfRegister::R11].unwrap_or(0),
        r12: regs[DwarfRegister::R12].unwrap_or(0),
        r13: regs[DwarfRegister::R13].unwrap_or(0),
        r14: regs[DwarfRegister::R14].unwrap_or(0),
        r15: regs[DwarfRegister::R15].unwrap_or(0),
        rsp: regs[DwarfRegister::SP].unwrap(),
    };
    lr.rsp -= 8;
    *(lr.rsp as *mut u64) = regs[DwarfRegister::IP].unwrap();
    unwind_lander(&lr);
}
