use alloc::borrow::Cow;
use alloc::boxed::Box;
use alloc::sync::Arc;
use alloc::vec::Vec;
use std::ffi::OsStr;
use std::fs::File;
use std::path::{Path, PathBuf};

use memmap2::Mmap;
use object::{Object, ObjectSection, SymbolMap, SymbolMapName};
use typed_arena::Arena;

use crate::{
    Context, FrameIter, Location, LocationRangeIter, LookupContinuation, LookupResult,
    SplitDwarfLoad,
};

type Reader<'a> = gimli::EndianSlice<'a, gimli::RunTimeEndian>;
type Error = Box<dyn std::error::Error>;
type Result<T> = std::result::Result<T, Error>;

/// A loader for the DWARF data required for a `Context`.
///
/// For performance reasons, a [`Context`] normally borrows the input data.
/// However, that means the input data must outlive the `Context`, which
/// is inconvenient for long-lived `Context`s.
/// This loader uses an arena to store the input data, together with the
/// `Context` itself. This ensures that the input data lives as long as
/// the `Context`.
///
/// The loader performs some additional tasks:
/// - Loads the symbol table from the executable file (see [`Self::find_symbol`]).
/// - Loads Mach-O dSYM files that are located next to the executable file.
/// - Locates and loads split DWARF files (DWO and DWP).
pub struct Loader {
    internal: LoaderInternal<'static>,
    arena_data: Arena<Vec<u8>>,
    arena_mmap: Arena<Mmap>,
}

impl Loader {
    /// Load the DWARF data for an executable file and create a `Context`.
    #[inline]
    pub fn new(path: impl AsRef<Path>) -> Result<Self> {
        Self::new_with_sup(path, None::<&Path>)
    }

    /// Load the DWARF data for an executable file and create a `Context`.
    ///
    /// Optionally also use a supplementary object file.
    pub fn new_with_sup(
        path: impl AsRef<Path>,
        sup_path: Option<impl AsRef<Path>>,
    ) -> Result<Self> {
        let arena_data = Arena::new();
        let arena_mmap = Arena::new();

        let internal = LoaderInternal::new(
            path.as_ref(),
            sup_path.as_ref().map(AsRef::as_ref),
            &arena_data,
            &arena_mmap,
        )?;
        Ok(Loader {
            // Convert to static lifetime to allow self-reference by `internal`.
            // `internal` is only accessed through `borrow_internal`, which ensures
            // that the static lifetime does not leak.
            internal: unsafe {
                core::mem::transmute::<LoaderInternal<'_>, LoaderInternal<'static>>(internal)
            },
            arena_data,
            arena_mmap,
        })
    }

    fn borrow_internal<'a, F, T>(&'a self, f: F) -> T
    where
        F: FnOnce(&'a LoaderInternal<'a>, &'a Arena<Vec<u8>>, &'a Arena<Mmap>) -> T,
    {
        // Do not leak the static lifetime.
        let internal = unsafe {
            core::mem::transmute::<&LoaderInternal<'static>, &'a LoaderInternal<'a>>(&self.internal)
        };
        f(internal, &self.arena_data, &self.arena_mmap)
    }

    /// Get the base address used for relative virtual addresses.
    ///
    /// Currently this is only non-zero for PE.
    pub fn relative_address_base(&self) -> u64 {
        self.borrow_internal(|i, _data, _mmap| i.relative_address_base)
    }

    /// Find the source file and line corresponding to the given virtual memory address.
    ///
    /// This calls [`Context::find_location`] with the given address.
    pub fn find_location(&self, probe: u64) -> Result<Option<Location<'_>>> {
        self.borrow_internal(|i, _data, _mmap| Ok(i.ctx.find_location(probe)?))
    }

    /// Return source file and lines for a range of addresses.
    ///
    /// This calls [`Context::find_location_range`] with the given range.
    pub fn find_location_range(
        &self,
        probe_low: u64,
        probe_high: u64,
    ) -> Result<LocationRangeIter<'_, Reader>> {
        self.borrow_internal(|i, _data, _mmap| {
            Ok(i.ctx.find_location_range(probe_low, probe_high)?)
        })
    }

    /// Return an iterator for the function frames corresponding to the given virtual
    /// memory address.
    ///
    /// This calls [`Context::find_frames`] with the given address.
    pub fn find_frames(&self, probe: u64) -> Result<FrameIter<'_, Reader<'_>>> {
        self.borrow_internal(|i, data, mmap| i.find_frames(probe, data, mmap))
    }

    /// Find the symbol table entry corresponding to the given virtual memory address.
    pub fn find_symbol(&self, probe: u64) -> Option<&str> {
        self.borrow_internal(|i, _data, _mmap| i.find_symbol(probe))
    }
}

struct LoaderInternal<'a> {
    ctx: Context<Reader<'a>>,
    relative_address_base: u64,
    symbols: SymbolMap<SymbolMapName<'a>>,
    dwarf_package: Option<gimli::DwarfPackage<Reader<'a>>>,
}

impl<'a> LoaderInternal<'a> {
    fn new(
        path: &Path,
        sup_path: Option<&Path>,
        arena_data: &'a Arena<Vec<u8>>,
        arena_mmap: &'a Arena<Mmap>,
    ) -> Result<Self> {
        let file = File::open(path)?;
        let map = arena_mmap.alloc(unsafe { Mmap::map(&file)? });
        let mut object = object::File::parse(&**map)?;

        let relative_address_base = object.relative_address_base();
        let symbols = object.symbol_map();

        // Load supplementary object file.
        // TODO: use debuglink and debugaltlink
        let sup_map;
        let sup_object = if let Some(sup_path) = sup_path {
            let sup_file = File::open(sup_path)?;
            sup_map = arena_mmap.alloc(unsafe { Mmap::map(&sup_file)? });
            Some(object::File::parse(&**sup_map)?)
        } else {
            None
        };

        // Load Mach-O dSYM file, ignoring errors.
        if let Some(map) = (|| {
            let uuid = object.mach_uuid().ok()??;
            path.parent()?.read_dir().ok()?.find_map(|candidate| {
                let candidate = candidate.ok()?;
                let path = candidate.path();
                if path.extension().and_then(OsStr::to_str) != Some("dSYM") {
                    return None;
                }
                let path = path.join("Contents/Resources/DWARF");
                path.read_dir().ok()?.find_map(|candidate| {
                    let candidate = candidate.ok()?;
                    let path = candidate.path();
                    let file = File::open(path).ok()?;
                    let map = unsafe { Mmap::map(&file) }.ok()?;
                    let object = object::File::parse(&*map).ok()?;
                    if object.mach_uuid() == Ok(Some(uuid)) {
                        Some(map)
                    } else {
                        None
                    }
                })
            })
        })() {
            let map = arena_mmap.alloc(map);
            object = object::File::parse(&**map)?;
        }

        // Load the DWARF sections.
        let endian = if object.is_little_endian() {
            gimli::RunTimeEndian::Little
        } else {
            gimli::RunTimeEndian::Big
        };
        let mut dwarf =
            gimli::Dwarf::load(|id| load_section(Some(id.name()), &object, endian, arena_data))?;
        if let Some(sup_object) = &sup_object {
            dwarf.load_sup(|id| load_section(Some(id.name()), sup_object, endian, arena_data))?;
        }

        let ctx = Context::from_dwarf(dwarf)?;

        // Load the DWP file, ignoring errors.
        let dwarf_package = (|| {
            let mut dwp_path = path.to_path_buf();
            let dwp_extension = path
                .extension()
                .map(|previous_extension| {
                    let mut previous_extension = previous_extension.to_os_string();
                    previous_extension.push(".dwp");
                    previous_extension
                })
                .unwrap_or_else(|| "dwp".into());
            dwp_path.set_extension(dwp_extension);
            let dwp_file = File::open(&dwp_path).ok()?;
            let map = arena_mmap.alloc(unsafe { Mmap::map(&dwp_file) }.ok()?);
            let dwp_object = object::File::parse(&**map).ok()?;

            let endian = if dwp_object.is_little_endian() {
                gimli::RunTimeEndian::Little
            } else {
                gimli::RunTimeEndian::Big
            };
            let empty = gimli::EndianSlice::new(&[][..], endian);
            gimli::DwarfPackage::load(
                |id| load_section(id.dwo_name(), &dwp_object, endian, arena_data),
                empty,
            )
            .ok()
        })();

        Ok(LoaderInternal {
            ctx,
            relative_address_base,
            symbols,
            dwarf_package,
        })
    }

    fn find_symbol(&self, probe: u64) -> Option<&str> {
        self.symbols.get(probe).map(|x| x.name())
    }

    fn find_frames(
        &self,
        probe: u64,
        arena_data: &'a Arena<Vec<u8>>,
        arena_mmap: &'a Arena<Mmap>,
    ) -> Result<FrameIter<'a, Reader>> {
        let mut frames = self.ctx.find_frames(probe);
        loop {
            let (load, continuation) = match frames {
                LookupResult::Output(output) => return Ok(output?),
                LookupResult::Load { load, continuation } => (load, continuation),
            };

            let r = self.load_dwo(load, arena_data, arena_mmap)?;
            frames = continuation.resume(r);
        }
    }

    fn load_dwo(
        &self,
        load: SplitDwarfLoad<Reader<'a>>,
        arena_data: &'a Arena<Vec<u8>>,
        arena_mmap: &'a Arena<Mmap>,
    ) -> Result<Option<Arc<gimli::Dwarf<Reader<'a>>>>> {
        // Load the DWO file from the DWARF package, if available.
        if let Some(dwp) = self.dwarf_package.as_ref() {
            if let Some(cu) = dwp.find_cu(load.dwo_id, &load.parent)? {
                return Ok(Some(Arc::new(cu)));
            }
        }

        // Determine the path to the DWO file.
        let mut path = PathBuf::new();
        if let Some(p) = load.comp_dir.as_ref() {
            path.push(convert_path(p)?);
        }
        let Some(p) = load.path.as_ref() else {
            return Ok(None);
        };
        path.push(convert_path(p)?);

        // Load the DWO file, ignoring errors.
        let dwo = (|| {
            let file = File::open(&path).ok()?;
            let map = arena_mmap.alloc(unsafe { Mmap::map(&file) }.ok()?);
            let object = object::File::parse(&**map).ok()?;
            let endian = if object.is_little_endian() {
                gimli::RunTimeEndian::Little
            } else {
                gimli::RunTimeEndian::Big
            };
            let mut dwo_dwarf =
                gimli::Dwarf::load(|id| load_section(id.dwo_name(), &object, endian, arena_data))
                    .ok()?;
            let dwo_unit_header = dwo_dwarf.units().next().ok()??;
            let dwo_unit = dwo_dwarf.unit(dwo_unit_header).ok()?;
            if dwo_unit.dwo_id != Some(load.dwo_id) {
                return None;
            }
            dwo_dwarf.make_dwo(&load.parent);
            Some(Arc::new(dwo_dwarf))
        })();
        Ok(dwo)
    }
}

fn load_section<'input, Endian: gimli::Endianity>(
    name: Option<&'static str>,
    file: &object::File<'input>,
    endian: Endian,
    arena_data: &'input Arena<Vec<u8>>,
) -> Result<gimli::EndianSlice<'input, Endian>> {
    let data = match name.and_then(|name| file.section_by_name(name)) {
        Some(section) => match section.uncompressed_data()? {
            Cow::Borrowed(b) => b,
            Cow::Owned(b) => arena_data.alloc(b),
        },
        None => &[],
    };
    Ok(gimli::EndianSlice::new(data, endian))
}

#[cfg(unix)]
fn convert_path<R: gimli::Reader<Endian = gimli::RunTimeEndian>>(r: &R) -> Result<PathBuf> {
    use std::os::unix::ffi::OsStrExt;
    let bytes = r.to_slice()?;
    let s = OsStr::from_bytes(&bytes);
    Ok(PathBuf::from(s))
}

#[cfg(not(unix))]
fn convert_path<R: gimli::Reader<Endian = gimli::RunTimeEndian>>(r: &R) -> Result<PathBuf> {
    let bytes = r.to_slice()?;
    let s = std::str::from_utf8(&bytes)?;
    Ok(PathBuf::from(s))
}
