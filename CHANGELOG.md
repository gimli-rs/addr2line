## 0.11.0 (2020/01/11)

### Breaking changes

* Updated `gimli` and `object` dependencies.

* [#130](https://github.com/gimli-rs/addr2line/pull/130)
  Changed `Location::file` from `Option<String>` to `Option<&str>`.
  This required adding lifetime parameters to `Location` and other structs that
  contain it.

* [#152](https://github.com/gimli-rs/addr2line/pull/152)
  Changed `Location::line` and `Location::column` from `Option<u64>`to `Option<u32>`.

* [#156](https://github.com/gimli-rs/addr2line/pull/156)
  Deleted `alloc` feature, and fixed `no-std` builds with stable rust.
  Removed default `Reader` parameter for `Context`, and added `ObjectContext` instead.

### Added

* [#134](https://github.com/gimli-rs/addr2line/pull/134)
  Added `Context::from_dwarf`.

### Changed

* [#133](https://github.com/gimli-rs/addr2line/pull/133)
  Fixed handling of units that can't be parsed.

* [#155](https://github.com/gimli-rs/addr2line/pull/155)
  Fixed `addr2line` output to match binutils.

* [#130](https://github.com/gimli-rs/addr2line/pull/130)
  Improved `.debug_line` parsing performance.

* [#148](https://github.com/gimli-rs/addr2line/pull/148)
  [#150](https://github.com/gimli-rs/addr2line/pull/150)
  [#151](https://github.com/gimli-rs/addr2line/pull/151)
  [#152](https://github.com/gimli-rs/addr2line/pull/152)
  Improved `.debug_info` parsing performance.

* [#137](https://github.com/gimli-rs/addr2line/pull/137)
  [#138](https://github.com/gimli-rs/addr2line/pull/138)
  [#139](https://github.com/gimli-rs/addr2line/pull/139)
  [#140](https://github.com/gimli-rs/addr2line/pull/140)
  [#146](https://github.com/gimli-rs/addr2line/pull/146)
  Improved benchmarks.


## 0.10.0 (2019/07/07)

### Breaking changes

* [#127](https://github.com/gimli-rs/addr2line/pull/127)
  Update `gimli`.


## 0.9.0 (2019/05/02)

### Breaking changes

* [#121](https://github.com/gimli-rs/addr2line/pull/121)
  Update `gimli`, `object`, and `fallible-iterator` dependencies.

### Added

* [#121](https://github.com/gimli-rs/addr2line/pull/121)
  Reexport `gimli`, `object`, and `fallible-iterator`.


## 0.8.0 (2019/02/06)

### Breaking changes

* [#107](https://github.com/gimli-rs/addr2line/pull/107)
  Update `object` dependency to 0.11. This is part of the public API.

### Added

* [#101](https://github.com/gimli-rs/addr2line/pull/101)
  Add `object` feature (enabled by default). Disable this feature to remove
  the `object` dependency and `Context::new` API.

* [#102](https://github.com/gimli-rs/addr2line/pull/102)
  Add `std` (enabled by default) and `alloc` features.

### Changed

* [#108](https://github.com/gimli-rs/addr2line/issues/108)
  `demangle` no longer ouputs the hash for rust symbols.

* [#109](https://github.com/gimli-rs/addr2line/issues/109)
  Set default `R` for `Context<R>`.
