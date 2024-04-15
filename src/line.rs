use alloc::boxed::Box;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::mem;
use core::num::NonZeroU64;

use crate::{Error, Location};

struct LineSequence {
    start: u64,
    end: u64,
    rows: Box<[LineRow]>,
}

struct LineRow {
    address: u64,
    file_index: u64,
    line: u32,
    column: u32,
}

pub(crate) struct Lines {
    files: Box<[String]>,
    sequences: Box<[LineSequence]>,
}

impl Lines {
    pub(crate) fn parse<R: gimli::Reader>(
        dw_unit: &gimli::Unit<R>,
        ilnp: gimli::IncompleteLineProgram<R, R::Offset>,
        sections: &gimli::Dwarf<R>,
    ) -> Result<Self, Error> {
        let mut sequences = Vec::new();
        let mut sequence_rows = Vec::<LineRow>::new();
        let mut rows = ilnp.rows();
        while let Some((_, row)) = rows.next_row()? {
            if row.end_sequence() {
                if let Some(start) = sequence_rows.first().map(|x| x.address) {
                    let end = row.address();
                    let mut rows = Vec::new();
                    mem::swap(&mut rows, &mut sequence_rows);
                    sequences.push(LineSequence {
                        start,
                        end,
                        rows: rows.into_boxed_slice(),
                    });
                }
                continue;
            }

            let address = row.address();
            let file_index = row.file_index();
            // Convert line and column to u32 to save a little memory.
            // We'll handle the special case of line 0 later,
            // and return left edge as column 0 in the public API.
            let line = row.line().map(NonZeroU64::get).unwrap_or(0) as u32;
            let column = match row.column() {
                gimli::ColumnType::LeftEdge => 0,
                gimli::ColumnType::Column(x) => x.get() as u32,
            };

            if let Some(last_row) = sequence_rows.last_mut() {
                if last_row.address == address {
                    last_row.file_index = file_index;
                    last_row.line = line;
                    last_row.column = column;
                    continue;
                }
            }

            sequence_rows.push(LineRow {
                address,
                file_index,
                line,
                column,
            });
        }
        sequences.sort_by_key(|x| x.start);

        let mut files = Vec::new();
        let header = rows.header();
        match header.file(0) {
            Some(file) => files.push(render_file(dw_unit, file, header, sections)?),
            None => files.push(String::from("")), // DWARF version <= 4 may not have 0th index
        }
        let mut index = 1;
        while let Some(file) = header.file(index) {
            files.push(render_file(dw_unit, file, header, sections)?);
            index += 1;
        }

        Ok(Self {
            files: files.into_boxed_slice(),
            sequences: sequences.into_boxed_slice(),
        })
    }

    pub(crate) fn file(&self, index: u64) -> Option<&str> {
        self.files.get(index as usize).map(String::as_str)
    }

    pub(crate) fn ranges(&self) -> impl Iterator<Item = gimli::Range> + '_ {
        self.sequences.iter().map(|sequence| gimli::Range {
            begin: sequence.start,
            end: sequence.end,
        })
    }

    pub(crate) fn location_ranges(
        &self,
        probe_low: u64,
        probe_high: u64,
    ) -> Result<LineLocationRangeIter<'_>, Error> {
        LineLocationRangeIter::new(self, probe_low, probe_high)
    }
}

pub(crate) struct LineLocationRangeIter<'ctx> {
    lines: &'ctx Lines,
    seqs: &'ctx [LineSequence],
    seq_idx: usize,
    row_idx: usize,
    probe_high: u64,
}

impl<'ctx> LineLocationRangeIter<'ctx> {
    fn new(lines: &'ctx Lines, probe_low: u64, probe_high: u64) -> Result<Self, Error> {
        // Find index for probe_low.
        let seq_idx = lines.sequences.binary_search_by(|sequence| {
            if probe_low < sequence.start {
                Ordering::Greater
            } else if probe_low >= sequence.end {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        });
        let seq_idx = match seq_idx {
            Ok(x) => x,
            Err(0) => 0, // probe below sequence, but range could overlap
            Err(_) => lines.sequences.len(),
        };

        let row_idx = if let Some(seq) = lines.sequences.get(seq_idx) {
            let idx = seq.rows.binary_search_by(|row| row.address.cmp(&probe_low));
            match idx {
                Ok(x) => x,
                Err(0) => 0, // probe below sequence, but range could overlap
                Err(x) => x - 1,
            }
        } else {
            0
        };

        Ok(Self {
            lines,
            seqs: &*lines.sequences,
            seq_idx,
            row_idx,
            probe_high,
        })
    }
}

impl<'ctx> Iterator for LineLocationRangeIter<'ctx> {
    type Item = (u64, u64, Location<'ctx>);

    fn next(&mut self) -> Option<(u64, u64, Location<'ctx>)> {
        while let Some(seq) = self.seqs.get(self.seq_idx) {
            if seq.start >= self.probe_high {
                break;
            }

            match seq.rows.get(self.row_idx) {
                Some(row) => {
                    if row.address >= self.probe_high {
                        break;
                    }

                    let file = self
                        .lines
                        .files
                        .get(row.file_index as usize)
                        .map(String::as_str);
                    let nextaddr = seq
                        .rows
                        .get(self.row_idx + 1)
                        .map(|row| row.address)
                        .unwrap_or(seq.end);

                    let item = (
                        row.address,
                        nextaddr - row.address,
                        Location {
                            file,
                            line: if row.line != 0 { Some(row.line) } else { None },
                            // If row.line is specified then row.column always has meaning.
                            column: if row.line != 0 {
                                Some(row.column)
                            } else {
                                None
                            },
                        },
                    );
                    self.row_idx += 1;

                    return Some(item);
                }
                None => {
                    self.seq_idx += 1;
                    self.row_idx = 0;
                }
            }
        }
        None
    }
}

fn render_file<R: gimli::Reader>(
    dw_unit: &gimli::Unit<R>,
    file: &gimli::FileEntry<R, R::Offset>,
    header: &gimli::LineProgramHeader<R, R::Offset>,
    sections: &gimli::Dwarf<R>,
) -> Result<String, gimli::Error> {
    let mut path = if let Some(ref comp_dir) = dw_unit.comp_dir {
        comp_dir.to_string_lossy()?.into_owned()
    } else {
        String::new()
    };

    // The directory index 0 is defined to correspond to the compilation unit directory.
    if file.directory_index() != 0 {
        if let Some(directory) = file.directory(header) {
            path_push(
                &mut path,
                sections
                    .attr_string(dw_unit, directory)?
                    .to_string_lossy()?
                    .as_ref(),
            );
        }
    }

    path_push(
        &mut path,
        sections
            .attr_string(dw_unit, file.path_name())?
            .to_string_lossy()?
            .as_ref(),
    );

    Ok(path)
}

fn path_push(path: &mut String, p: &str) {
    if has_unix_root(p) || has_windows_root(p) {
        *path = p.to_string();
    } else {
        let dir_separator = if has_windows_root(path.as_str()) {
            '\\'
        } else {
            '/'
        };

        if !path.is_empty() && !path.ends_with(dir_separator) {
            path.push(dir_separator);
        }
        *path += p;
    }
}

/// Check if the path in the given string has a unix style root
fn has_unix_root(p: &str) -> bool {
    p.starts_with('/')
}

/// Check if the path in the given string has a windows style root
fn has_windows_root(p: &str) -> bool {
    p.starts_with('\\') || p.get(1..3) == Some(":\\")
}