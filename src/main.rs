extern crate gimli;
extern crate goblin;
extern crate libc;
extern crate backtrace;

use libc::{c_void, c_int, c_char};
use std::ffi::CStr;

#[repr(C)]
struct DlPhdrInfo {
    addr: u64,
    name: *const c_char,
    phdr: *const Phdr64,
    phnum: u16,
}

#[repr(C)]
struct Phdr32 {
    type_: u32,
    offset: u32,
    vaddr: u32,
    paddr: u32,
    filesz: u32,
    memsz: u32,
    flags: u32,
    align: u32,
}

#[repr(C)]
struct Phdr64 {
    type_: u32,
    flags: u32,
    offset: u64,
    vaddr: u64,
    paddr: u64,
    filesz: u64,
    memsz: u64,
    align: u64,
}

#[repr(C)]
struct DwarfEhFrameHdr {
    version: u8,
    eh_frame_ptr_enc: u8,
    fd_count_enc: u8,
    table_enc: u8,
}

const PT_GNU_EH_FRAME: u32 = 0x6474e550;
const PT_LOAD: u32 = 1;

type PersonalityRoutine = extern "C" fn(version: c_int, actions: c_int, class: u64, object: *mut _Unwind_Exception, context: *mut _Unwind_Context) -> _Unwind_Reason_Code;
type PhdrCb = extern "C" fn(info: *const DlPhdrInfo, size: usize, data: *mut c_void) -> c_int;
extern "C" {
    fn dl_iterate_phdr(callback: PhdrCb, data: *mut c_void) -> c_int;
    fn unwind_trampoline(exception: *mut _Unwind_Exception);
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

use gimli::UnwindSection;

#[repr(C)]
#[derive(Copy, Clone, PartialEq)]
pub enum _Unwind_Action {
    _UA_SEARCH_PHASE = 1,
    _UA_CLEANUP_PHASE = 2,
    _UA_HANDLER_FRAME = 4,
    _UA_FORCE_UNWIND = 8,
    _UA_END_OF_STACK = 16,
}

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum _Unwind_Reason_Code {
    _URC_NO_REASON = 0,
    _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
    _URC_FATAL_PHASE2_ERROR = 2,
    _URC_FATAL_PHASE1_ERROR = 3,
    _URC_NORMAL_STOP = 4,
    _URC_END_OF_STACK = 5,
    _URC_HANDLER_FOUND = 6,
    _URC_INSTALL_CONTEXT = 7,
    _URC_CONTINUE_UNWIND = 8,
    _URC_FAILURE = 9, // used only by ARM EHABI
}
pub type _Unwind_Exception_Class = u64;
pub type _Unwind_Exception_Cleanup_Fn = extern "C" fn(unwind_code: _Unwind_Reason_Code,
                                                      exception: *mut _Unwind_Exception);
#[repr(C)]
pub struct _Unwind_Exception {
    pub exception_class: _Unwind_Exception_Class,
    pub exception_cleanup: _Unwind_Exception_Cleanup_Fn,
    pub private: [_Unwind_Word; 20],
}

pub type _Unwind_Word = usize;
pub type _Unwind_Ptr = usize;
pub struct _Unwind_Context {
    lsda: u64,
    ip: u64,
    initial_address: u64,
    registers: *mut Registers,
}
pub type _Unwind_Trace_Fn = extern "C" fn(ctx: *mut _Unwind_Context, arg: *mut c_void)
                                          -> _Unwind_Reason_Code;

#[no_mangle]
pub extern "C" fn _Unwind_Resume(exception: *mut _Unwind_Exception) -> ! {
    _Unwind_RaiseException(exception);
    unreachable!();
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_DeleteException(exception: *mut _Unwind_Exception) {
    ((*exception).exception_cleanup)(_Unwind_Reason_Code::_URC_FOREIGN_EXCEPTION_CAUGHT, exception);
    println!("exception deleted.");
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_GetRegionStart(ctx: *mut _Unwind_Context) -> _Unwind_Ptr {
    (*ctx).initial_address as usize
}
#[no_mangle]
pub extern "C" fn _Unwind_GetTextRelBase(ctx: *mut _Unwind_Context) -> _Unwind_Ptr {
    unreachable!();
}
#[no_mangle]
pub extern "C" fn _Unwind_GetDataRelBase(ctx: *mut _Unwind_Context) -> _Unwind_Ptr {
    unreachable!();
}


#[no_mangle]
pub unsafe extern "C" fn _Unwind_GetLanguageSpecificData(ctx: *mut _Unwind_Context) -> *mut c_void {
    (*ctx).lsda as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_SetGR(ctx: *mut _Unwind_Context, reg_index: c_int, value: _Unwind_Word) {
    (*(*ctx).registers)[reg_index as u8] = Some(value as u64);
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_SetIP(ctx: *mut _Unwind_Context, value: _Unwind_Word) {
    (*(*ctx).registers)[DwarfRegister::IP] = Some(value as u64);
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_GetIPInfo(ctx: *mut _Unwind_Context, ip_before_insn: *mut c_int)
                                    -> _Unwind_Word {
    *ip_before_insn = 0;
    (*ctx).ip as usize
}

#[no_mangle]
pub extern "C" fn _Unwind_FindEnclosingFunction(pc: *mut c_void) -> *mut c_void {
    unreachable!();
}

#[no_mangle]
pub extern "C" fn _Unwind_RaiseException(exception: *mut _Unwind_Exception) -> _Unwind_Reason_Code {
    unsafe {
        unwind_trampoline(exception);
    }
    unreachable!();
}

/*
#[no_mangle]
pub extern "C" fn _Unwind_Backtrace(trace: _Unwind_Trace_Fn,
                                    trace_argument: *mut c_void)
                                    -> _Unwind_Reason_Code {
    unreachable!();
}*/

#[derive(Debug)]
struct EhRef {
    start_addr: u64,
    end_addr: u64,
    cfi: (u64, u64),
}

extern "C" fn mycb(info: *const DlPhdrInfo, size: usize, data: *mut c_void) -> c_int {
    let data = data as *mut Vec<EhRef>;
    //assert_eq!(size, std::mem::size_of::<DlPhdrInfo>());
    unsafe {
        println!("addr 0x{:x}", (*info).addr);

        //let elf = goblin::elf::Elf::parse(std::slice::from_raw_parts((*info).addr as *const u8, 0x10000000));

        let name = CStr::from_ptr((*info).name);
        println!("{:?} has {}", name, (*info).phnum);

        let phdr = std::slice::from_raw_parts((*info).phdr, (*info).phnum as usize);

        /*
        let mut cfi = None;
        //let mut text_addr = None;
        //let mut data_addr = None;
        for phdr in phdr {
            match phdr.type_ {
                PT_GNU_EH_FRAME => cfi = Some(((*info).addr + phdr.vaddr, phdr.memsz)),
                PT_LOAD => println!("pt load 0x{:x} size 0x{:x}", (*info).addr + phdr.vaddr, phdr.memsz),
                _ => (),
            }
    }*/

        // FIXME unwrap
        let text = phdr.iter().filter(|x| x.type_ == PT_LOAD).next().unwrap();
        let eh_frame = phdr.iter().filter(|x| x.type_ == PT_GNU_EH_FRAME).next().unwrap();

        let start_addr = (*info).addr + text.vaddr;
        let cfi_start = (*info).addr + eh_frame.vaddr;
        (*data).push(EhRef {
            start_addr,
            end_addr: start_addr + text.memsz,
            cfi: (cfi_start, cfi_start + eh_frame.memsz),
        });
        //*(data as *mut Option<(u64, u64)>) = cfi;

        0
    }
}

struct Bomb(String);

impl Drop for Bomb {
    fn drop(&mut self) {
        println!("bomb dropped: {:?}", self.0);
    }
}

fn unwind_info_for_address<'bases, R: gimli::Reader>(sel: &gimli::EhFrame<R>,
                                              bases: &'bases gimli::BaseAddresses,
                                              ctx: gimli::UninitializedUnwindContext<gimli::EhFrame<R>, R>,
                                              address: u64)
                                              -> gimli::Result<(gimli::UnwindTableRow<R>, Option<gimli::Pointer>, Option<gimli::Pointer>, u64)> {

    let mut target_fde = None;

    let mut i = 0;
    let mut entries = sel.entries(bases);
    while let Some(entry) = entries.next()? {
        match entry {
            gimli::CieOrFde::Cie(_) => continue,
            gimli::CieOrFde::Fde(partial) => {
                let fde = partial.parse(|offset| sel.cie_from_offset(bases, offset))?;
                //println!("{} fde {:x} - {:x}", i, fde.initial_address(), fde.len());
                if fde.contains(address) {
                    target_fde = Some(fde);
                    break;
                }
            }
        }
        i += 1;
    }
    
    if let Some(fde) = target_fde {
        let mut result_row = None;
        let mut ctx = ctx.initialize(fde.cie()).unwrap();
        
        {
            let mut table = gimli::UnwindTable::new(&mut ctx, &fde);
            while let Some(row) = table.next_row()? {
                if row.contains(address) {
                    result_row = Some(row.clone());
                    break;
                }
            }
        }
        
        if let Some(row) = result_row {
            return Ok((row, fde.personality(), fde.lsda(), fde.initial_address()));
        }
    }
    
    Err(gimli::Error::NoUnwindInfoForAddress)
}

#[derive(Default, Clone, PartialEq, Eq)]
struct Registers {
    registers: [Option<u64>; 17],
}

use std::fmt::{Debug, Formatter, Result as FmtResult};
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

use std::ops::{Index, IndexMut};

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

enum DwarfRegister {
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

unsafe fn deref_ptr(ptr: gimli::Pointer) -> u64 {
    match ptr {
        gimli::Pointer::Direct(x) => x,
        gimli::Pointer::Indirect(x) => *(x as *const u64),
    }
}

unsafe fn land(regs: &Registers) {
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

unsafe fn do_laundry(cfi: &Vec<EhRef>, stack: u64, saved_regs: &SavedRegs, exception: *mut _Unwind_Exception) {
    //let (cfi_addr, cfi_sz) = cfi.clone().unwrap();

    let cfi: Vec<_> = cfi.iter().map(|er| {
        let (cfi_addr, cfi_sz) = er.cfi;
        let bases = gimli::BaseAddresses::default()
            .set_cfi(cfi_addr);
        let eh_frame_hdr: &'static [u8] = std::slice::from_raw_parts(cfi_addr as *const u8, cfi_sz as usize);
        let efh = &*(cfi_addr as *const DwarfEhFrameHdr);
        assert_eq!(efh.version, 1);
        let fpe = gimli::DwEhPe(efh.eh_frame_ptr_enc);
        println!("fpe {} {}", fpe.format(), fpe.application());
        let mut rest = gimli::EndianBuf::new(&eh_frame_hdr[4..], gimli::NativeEndian);
        let cfi_addr = deref_ptr(gimli::parse_encoded_pointer(fpe, &bases, 64, &gimli::EndianBuf::new(eh_frame_hdr, gimli::NativeEndian), &mut rest).unwrap());
        let cfi_sz = 0x10000000;
        
        let eh_frame: &'static [u8] = std::slice::from_raw_parts(cfi_addr as *const u8, cfi_sz as usize);
        println!("cfi at {:p} sz {:x}", cfi_addr as *const u8, cfi_sz);
        let eh_frame = gimli::EhFrame::new(eh_frame, gimli::LittleEndian);
        
        let bases = bases.set_cfi(cfi_addr);

        (er, eh_frame, bases)
    }).collect();

    let mut registers = Registers::default();
    registers[DwarfRegister::Rbx] = Some(saved_regs.rbx);
    registers[DwarfRegister::Rbp] = Some(saved_regs.rbp);
    registers[DwarfRegister::SP] = Some(stack + 8);
    registers[DwarfRegister::R12] = Some(saved_regs.r12);
    registers[DwarfRegister::R13] = Some(saved_regs.r13);
    registers[DwarfRegister::R14] = Some(saved_regs.r14);
    registers[DwarfRegister::R15] = Some(saved_regs.r15);
    registers[DwarfRegister::IP] = Some(*(stack as *const u64));

    while let Some(mut caller) = registers[DwarfRegister::IP] {
        //let caller = registers[DwarfRegister::IP].unwrap() - 1; //*((cfa - 8) as *const u64);
        caller -= 1; // THIS IS NECESSARY
        println!("caller is 0x{:x}", caller);

        //if caller == 0 { break; }
        //backtrace::resolve(caller as *mut std::os::raw::c_void, |sym| println!("wtf {:?}", sym.name()));
        backtrace::resolve(caller as *mut std::os::raw::c_void, |sym| println!("{:?} ({:?}:{:?})", sym.name(), sym.filename(), sym.lineno()));

        let &(_, ref eh_frame, ref bases) = cfi.iter().filter(|x| caller >= x.0.start_addr && caller < x.0.end_addr).next().unwrap();

        match unwind_info_for_address(&eh_frame, &bases, gimli::UninitializedUnwindContext::new(), caller) {
            Ok((row, personality, lsda, initial_address)) => {
                //Ok((row, _)) => {
                println!("ok: {:?} (0x{:x} - 0x{:x})", row.cfa(), row.start_address(), row.end_address());
                let cfa = match *row.cfa() {
                    gimli::CfaRule::RegisterAndOffset { register, offset } =>
                        registers[register].unwrap().wrapping_add(offset as u64),
                    _ => unimplemented!(),
                };
                println!("cfa is 0x{:x}", cfa);

                // FIXME the whole phase1/2 shenanigans
                if let Some(personality) = personality {
                    println!("HAS PERSONALITY");
                    let personality: PersonalityRoutine = std::mem::transmute(deref_ptr(personality));

                    let mut ctx = _Unwind_Context {
                        lsda: deref_ptr(lsda.unwrap()),
                        ip: caller,
                        initial_address,
                        registers: &mut registers,
                    };
                    match personality(1, _Unwind_Action::_UA_CLEANUP_PHASE as c_int, (*exception).exception_class,
                                      exception, &mut ctx) {
                        _Unwind_Reason_Code::_URC_CONTINUE_UNWIND => (),
                        _Unwind_Reason_Code::_URC_INSTALL_CONTEXT => land(&registers),
                        x => panic!("wtf reason code {:?}", x),
                    }
                }
                
                let mut newregs = registers.clone();
                newregs[DwarfRegister::IP] = None;
                for &(reg, ref rule) in row.registers() {
                    println!("rule {} {:?}", reg, rule);
                    assert!(reg != 7); // stack = cfa
                    newregs[reg] = match *rule {
                        gimli::RegisterRule::Undefined => unreachable!(), // registers[reg],
                        gimli::RegisterRule::SameValue => Some(registers[reg].unwrap()), // not sure why this exists
                        gimli::RegisterRule::Register(r) => registers[r],
                        gimli::RegisterRule::Offset(n) => Some(*((cfa.wrapping_add(n as u64)) as *const u64)),
                        gimli::RegisterRule::ValOffset(n) => Some(cfa.wrapping_add(n as u64)),
                        gimli::RegisterRule::Expression(_) => unimplemented!(),
                        gimli::RegisterRule::ValExpression(_) => unimplemented!(),
                        gimli::RegisterRule::Architectural => unreachable!(),
                    };
                }
                newregs[7] = Some(cfa);

                registers = newregs;
                println!("registers:{:?}", registers);
            }
            Err(e) => panic!("err {:?}", e),
        }
    }
    /*

        //let baseptr = 0; //(*info).addr as isize;
        let bt = backtrace::Backtrace::new();
        //let addrs = bt.frames().iter().map(|x| x.ip().wrapping_offset(-baseptr));
        
        //for addr in addrs {
        for frame in &bt.frames()[7..] {
            println!("FRAME@{:p}", frame.ip());
            for sym in frame.symbols() {
                println!("\taka {} ({:?}:{:?})", sym.name().unwrap().as_str().unwrap(), sym.filename(), sym.lineno());
            }

            let addr = frame.ip();
            match eh_frame.unwind_info_for_address(&bases, gimli::UninitializedUnwindContext::new(), addr as u64) {
                Ok((row, _)) => {
                    println!("ok: {:?}", row.cfa());
                }
                Err(e) => println!("err {:?}", e),
            }

            println!();
        }*/
}

#[repr(C)]
pub struct SavedRegs {
    r15: u64,
    r14: u64,
    r13: u64,
    r12: u64,
    rbx: u64,
    rbp: u64,
}

#[no_mangle]
pub unsafe extern "C" fn unwind_recorder(exception: *mut _Unwind_Exception, stack: u64, saved_regs: *mut SavedRegs) {
    let mut cfi: Vec<EhRef> = Vec::new();
    dl_iterate_phdr(mycb, &mut cfi as *mut _ as *mut c_void);

    println!("{:?}", cfi);

    do_laundry(&cfi, stack, &*saved_regs, exception);
}

fn bar() {
    panic!("test panic");
    unsafe {
        //unwind_trampoline();
    }
}
fn foo() {
    let _b = Bomb("why though".to_owned());
    bar()
}

fn main() {
    foo();
    println!("down");
}
