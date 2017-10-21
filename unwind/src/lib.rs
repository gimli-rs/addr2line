extern crate gimli;
extern crate libc;
extern crate fallible_iterator;
#[macro_use] extern crate log;

use gimli::{UnwindSection, UnwindTable, UnwindTableRow, EhFrame, BaseAddresses, UninitializedUnwindContext, Pointer, Reader, CieOrFde, EndianBuf, NativeEndian, CfaRule, RegisterRule, EhFrameHdr, ParsedEhFrameHdr};
use fallible_iterator::FallibleIterator;

mod registers;
mod find_cfi;
mod range;
pub mod libunwind_shim;
pub mod glue;
use registers::{Registers, DwarfRegister};
use find_cfi::EhRef;


pub struct StackFrames<'a> {
    unwinder: &'a mut DwarfUnwinder,
    registers: Registers,
    state: Option<(UnwindTableRow<StaticReader>, u64)>,
}

#[derive(Debug)]
pub struct StackFrame {
    personality: Option<u64>,
    lsda: Option<u64>,
    initial_address: u64,
}

pub trait Unwinder: Default {
    fn trace<F>(&mut self, f: F) where F: FnMut(&mut StackFrames);
}

type StaticReader = EndianBuf<'static, NativeEndian>;

struct ObjectRecord {
    er: EhRef,
    eh_frame_hdr: ParsedEhFrameHdr<StaticReader>,
    eh_frame: EhFrame<StaticReader>,
    bases: BaseAddresses,
}

pub struct DwarfUnwinder {
    cfi: Vec<ObjectRecord>,
    ctx: Option<UninitializedUnwindContext<EhFrame<StaticReader>, StaticReader>>,
}

impl Default for DwarfUnwinder {
    fn default() -> DwarfUnwinder {
        let cfi = find_cfi::find_cfi_sections().into_iter().map(|er| {
            unsafe {
                let bases = BaseAddresses::default().set_cfi(er.cfi.start);

                let eh_frame_hdr: &'static [u8] = std::slice::from_raw_parts(er.cfi.start as *const u8, er.cfi.len() as usize);

                let eh_frame_hdr = EhFrameHdr::new(eh_frame_hdr, NativeEndian).parse(&bases, 8).unwrap();

                let cfi_addr = deref_ptr(eh_frame_hdr.eh_frame_ptr());
                let cfi_sz = 0x10000000; // FIXME HACK

                let eh_frame: &'static [u8] = std::slice::from_raw_parts(cfi_addr as *const u8, cfi_sz as usize);
                trace!("cfi at {:p} sz {:x}", cfi_addr as *const u8, cfi_sz);
                let eh_frame = EhFrame::new(eh_frame, NativeEndian);

                let bases = bases.set_cfi(cfi_addr).set_data(er.cfi.start);

                ObjectRecord { er, eh_frame_hdr, eh_frame, bases }
            }
        }).collect();

        DwarfUnwinder {
            cfi,
            ctx: Some(UninitializedUnwindContext::new()),
        }
    }
}

pub struct UnwindPayload<'a> {
    unwinder: &'a mut DwarfUnwinder,
    tracer: &'a mut FnMut(&mut StackFrames),
}

impl Unwinder for DwarfUnwinder {
    fn trace<F>(&mut self, mut f: F) where F: FnMut(&mut StackFrames) {
        let mut payload = UnwindPayload {
            unwinder: self,
            tracer: &mut f,
        };

        unsafe { glue::unwind_trampoline(&mut payload) };
    }
}


struct UnwindInfo<R: Reader> {
    row: UnwindTableRow<R>,
    personality: Option<Pointer>,
    lsda: Option<Pointer>,
    initial_address: u64,
    ctx: UninitializedUnwindContext<EhFrame<StaticReader>, StaticReader>,
}

fn unwind_info_for_address<'bases>(eh_frame_hdr: &ParsedEhFrameHdr<StaticReader>,
                                   sel: &EhFrame<StaticReader>,
                                   bases: &'bases BaseAddresses,
                                   ctx: UninitializedUnwindContext<EhFrame<StaticReader>, StaticReader>,
                                   address: u64)
                                   -> gimli::Result<UnwindInfo<StaticReader>> {
    let fdeptr = eh_frame_hdr.table().unwrap().lookup(address, bases).unwrap();
    let fdeptr = match fdeptr {
        Pointer::Direct(x) => x,
        _ => unreachable!(),
    };

    let entry = gimli::parse_cfi_entry(bases, *sel, &mut EndianBuf::new(unsafe { std::slice::from_raw_parts(fdeptr as *const u8, 0x1000000) }, NativeEndian)).unwrap().unwrap();
    let target_fde = match entry {
        CieOrFde::Fde(fde) => Some(fde.parse(|offset| sel.cie_from_offset(bases, offset))?),
        CieOrFde::Cie(_) => unimplemented!(), // return error here probably
    };


    if let Some(fde) = target_fde {
        trace!("fde {:x} - {:x}", fde.initial_address(), fde.len());
        assert!(fde.contains(address));
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
                ctx: ctx.reset(),
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


impl<'a> StackFrames<'a> {
    pub fn registers(&mut self) -> &mut Registers {
        &mut self.registers
    }
}

impl<'a> FallibleIterator for StackFrames<'a> {
    type Item = StackFrame;
    type Error = gimli::Error;

    fn next(&mut self) -> Result<Option<StackFrame>, Self::Error> {
        let registers = &mut self.registers;

        if let Some((row, cfa)) = self.state.take() {
            let mut newregs = registers.clone();
            newregs[DwarfRegister::IP] = None;
            for &(reg, ref rule) in row.registers() {
                trace!("rule {} {:?}", reg, rule);
                assert!(reg != 7); // stack = cfa
                newregs[reg] = match *rule {
                    RegisterRule::Undefined => unreachable!(), // registers[reg],
                    RegisterRule::SameValue => Some(registers[reg].unwrap()), // not sure why this exists
                    RegisterRule::Register(r) => registers[r],
                    RegisterRule::Offset(n) => Some(unsafe { *((cfa.wrapping_add(n as u64)) as *const u64) }),
                    RegisterRule::ValOffset(n) => Some(cfa.wrapping_add(n as u64)),
                    RegisterRule::Expression(_) => unimplemented!(),
                    RegisterRule::ValExpression(_) => unimplemented!(),
                    RegisterRule::Architectural => unreachable!(),
                };
            }
            newregs[7] = Some(cfa);

            *registers = newregs;
            trace!("registers:{:?}", registers);
        }


        if let Some(mut caller) = registers[DwarfRegister::IP] {
            caller -= 1; // THIS IS NECESSARY
            debug!("caller is 0x{:x}", caller);

            let &ObjectRecord { ref eh_frame, ref bases, ref eh_frame_hdr, .. } = self.unwinder.cfi.iter().filter(|x| x.er.text.contains(caller)).next().ok_or(gimli::Error::NoUnwindInfoForAddress)?;

            let ctx = self.unwinder.ctx.take().unwrap_or(UninitializedUnwindContext::new());
            let UnwindInfo { row, personality, lsda, initial_address, ctx } = unwind_info_for_address(eh_frame_hdr, eh_frame, &bases, ctx, caller)?;
            self.unwinder.ctx = Some(ctx);

            trace!("ok: {:?} (0x{:x} - 0x{:x})", row.cfa(), row.start_address(), row.end_address());
            let cfa = match *row.cfa() {
                CfaRule::RegisterAndOffset { register, offset } =>
                    registers[register].unwrap().wrapping_add(offset as u64),
                _ => unimplemented!(),
            };
            trace!("cfa is 0x{:x}", cfa);

            self.state = Some((row, cfa));

            Ok(Some(StackFrame {
                personality: personality.map(|x| unsafe { deref_ptr(x) }),
                lsda: lsda.map(|x| unsafe { deref_ptr(x) }),
                initial_address,
            }))
        } else {
            Ok(None)
        }
    }
}
