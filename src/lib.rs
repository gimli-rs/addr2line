extern crate gimli;
extern crate libc;

use libc::c_int;

use gimli::{UnwindSection, UnwindTable, UnwindTableRow, EhFrame, BaseAddresses, UninitializedUnwindContext, Pointer, Reader, CieOrFde, DwEhPe, EndianBuf, NativeEndian, CfaRule, RegisterRule};

mod registers;
mod find_cfi;
mod range;
pub mod libunwind_shim;
pub mod glue;
use registers::{Registers, DwarfRegister};
use find_cfi::EhRef;
use libunwind_shim::{_Unwind_Exception, _Unwind_Context, _Unwind_Action, _Unwind_Reason_Code};
use glue::SavedRegs;


#[repr(C)]
struct DwarfEhFrameHdr {
    version: u8,
    eh_frame_ptr_enc: u8,
    fd_count_enc: u8,
    table_enc: u8,
}

type PersonalityRoutine = extern "C" fn(version: c_int, actions: c_int, class: u64, object: *mut _Unwind_Exception, context: *mut _Unwind_Context) -> _Unwind_Reason_Code;

struct UnwindInfo<R: Reader> {
    row: UnwindTableRow<R>,
    personality: Option<Pointer>,
    lsda: Option<Pointer>,
    initial_address: u64,
}

fn unwind_info_for_address<'bases, R: Reader>(sel: &EhFrame<R>,
                                              bases: &'bases BaseAddresses,
                                              ctx: UninitializedUnwindContext<EhFrame<R>, R>,
                                              address: u64)
                                              -> gimli::Result<UnwindInfo<R>> {

    let mut target_fde = None;

    let mut entries = sel.entries(bases);
    while let Some(entry) = entries.next()? {
        match entry {
            CieOrFde::Cie(_) => continue,
            CieOrFde::Fde(partial) => {
                let fde = partial.parse(|offset| sel.cie_from_offset(bases, offset))?;
                //println!("{} fde {:x} - {:x}", i, fde.initial_address(), fde.len());
                if fde.contains(address) {
                    target_fde = Some(fde);
                    break;
                }
            }
        }
    }
    
    if let Some(fde) = target_fde {
        let mut result_row = None;
        let mut ctx = ctx.initialize(fde.cie()).unwrap();
        
        {
            let mut table = UnwindTable::new(&mut ctx, &fde);
            while let Some(row) = table.next_row()? {
                if row.contains(address) {
                    result_row = Some(row.clone());
                    break;
                }
            }
        }
        
        if let Some(row) = result_row {
            return Ok(UnwindInfo {
                row,
                personality: fde.personality(),
                lsda: fde.lsda(),
                initial_address: fde.initial_address(),
            });
        }
    }
    
    Err(gimli::Error::NoUnwindInfoForAddress)
}

unsafe fn deref_ptr(ptr: Pointer) -> u64 {
    match ptr {
        Pointer::Direct(x) => x,
        Pointer::Indirect(x) => *(x as *const u64),
    }
}

unsafe fn do_laundry(cfi: &Vec<EhRef>, stack: u64, saved_regs: &SavedRegs, exception: *mut _Unwind_Exception) {
    //let (cfi_addr, cfi_sz) = cfi.clone().unwrap();

    let cfi: Vec<_> = cfi.iter().map(|er| {
        let bases = BaseAddresses::default()
            .set_cfi(er.cfi.start);
        let eh_frame_hdr: &'static [u8] = std::slice::from_raw_parts(er.cfi.start as *const u8, er.cfi.len() as usize);
        let efh = &*(er.cfi.start as *const DwarfEhFrameHdr);
        assert_eq!(efh.version, 1);
        let fpe = DwEhPe(efh.eh_frame_ptr_enc);
        println!("fpe {} {}", fpe.format(), fpe.application());
        let mut rest = EndianBuf::new(&eh_frame_hdr[4..], NativeEndian);
        let cfi_addr = deref_ptr(gimli::parse_encoded_pointer(fpe, &bases, 64, &EndianBuf::new(eh_frame_hdr, NativeEndian), &mut rest).unwrap());
        let cfi_sz = 0x10000000; // FIXME HACK
        
        let eh_frame: &'static [u8] = std::slice::from_raw_parts(cfi_addr as *const u8, cfi_sz as usize);
        println!("cfi at {:p} sz {:x}", cfi_addr as *const u8, cfi_sz);
        let eh_frame = EhFrame::new(eh_frame, NativeEndian);
        
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
        caller -= 1; // THIS IS NECESSARY
        println!("caller is 0x{:x}", caller);

        //backtrace::resolve(caller as *mut std::os::raw::c_void, |sym| println!("wtf {:?}", sym.name()));
        //backtrace::resolve(caller as *mut std::os::raw::c_void, |sym| println!("{:?} ({:?}:{:?})", sym.name(), sym.filename(), sym.lineno()));

        let &(_, ref eh_frame, ref bases) = cfi.iter().filter(|x| x.0.text.contains(caller)).next().unwrap();

        match unwind_info_for_address(&eh_frame, &bases, UninitializedUnwindContext::new(), caller) {
            Ok(UnwindInfo { row, personality, lsda, initial_address }) => {
                //Ok((row, _)) => {
                println!("ok: {:?} (0x{:x} - 0x{:x})", row.cfa(), row.start_address(), row.end_address());
                let cfa = match *row.cfa() {
                    CfaRule::RegisterAndOffset { register, offset } =>
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
                        _Unwind_Reason_Code::_URC_INSTALL_CONTEXT => glue::land(&registers),
                        x => panic!("wtf reason code {:?}", x),
                    }
                }
                
                let mut newregs = registers.clone();
                newregs[DwarfRegister::IP] = None;
                for &(reg, ref rule) in row.registers() {
                    println!("rule {} {:?}", reg, rule);
                    assert!(reg != 7); // stack = cfa
                    newregs[reg] = match *rule {
                        RegisterRule::Undefined => unreachable!(), // registers[reg],
                        RegisterRule::SameValue => Some(registers[reg].unwrap()), // not sure why this exists
                        RegisterRule::Register(r) => registers[r],
                        RegisterRule::Offset(n) => Some(*((cfa.wrapping_add(n as u64)) as *const u64)),
                        RegisterRule::ValOffset(n) => Some(cfa.wrapping_add(n as u64)),
                        RegisterRule::Expression(_) => unimplemented!(),
                        RegisterRule::ValExpression(_) => unimplemented!(),
                        RegisterRule::Architectural => unreachable!(),
                    };
                }
                newregs[7] = Some(cfa);

                registers = newregs;
                println!("registers:{:?}", registers);
            }
            Err(e) => panic!("err {:?}", e),
        }
    }
}

