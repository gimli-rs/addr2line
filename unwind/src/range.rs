#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AddrRange {
    pub start: u64,
    pub end: u64,
}

impl AddrRange {
    pub fn contains(&self, addr: u64) -> bool {
        addr >= self.start && addr < self.end
    }

    pub fn len(&self) -> u64 {
        self.end - self.start
    }
}
