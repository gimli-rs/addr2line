#![allow(non_camel_case_types, non_snake_case, unused_variables)]

use libc::{c_void, c_int};
use registers::{Registers, DwarfRegister};
use glue::unwind_trampoline;

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
    pub lsda: u64,
    pub ip: u64,
    pub initial_address: u64,
    pub registers: *mut Registers,
}
pub type _Unwind_Trace_Fn = extern "C" fn(ctx: *mut _Unwind_Context, arg: *mut c_void)
                                          -> _Unwind_Reason_Code;

#[no_mangle]
pub unsafe extern "C" fn _Unwind_Resume(exception: *mut _Unwind_Exception) -> ! {
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
pub unsafe extern "C" fn _Unwind_GetTextRelBase(ctx: *mut _Unwind_Context) -> _Unwind_Ptr {
    unreachable!();
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_GetDataRelBase(ctx: *mut _Unwind_Context) -> _Unwind_Ptr {
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
pub unsafe extern "C" fn _Unwind_FindEnclosingFunction(pc: *mut c_void) -> *mut c_void {
    unreachable!();
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_RaiseException(exception: *mut _Unwind_Exception) -> _Unwind_Reason_Code {
    unwind_trampoline(exception);
    unreachable!();
}

#[no_mangle]
pub unsafe extern "C" fn _Unwind_Backtrace(trace: _Unwind_Trace_Fn,
                                    trace_argument: *mut c_void)
                                    -> _Unwind_Reason_Code {
    unreachable!();
}

