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
