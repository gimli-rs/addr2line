#!/usr/bin/env bash

set -ex

case "$GIMLI_JOB" in
    "build")
        cargo test
        cargo test --release
        ;;

    "features")
        cargo build --no-default-features
        cargo build --no-default-features --features "std"
        cargo build --no-default-features --features "std cpp_demangle"
        cargo build --no-default-features --features "std rustc-demangle"
        cargo build --no-default-features --features "std-object"
        cargo build --no-default-features --features "fallible-iterator"
        cargo build --no-default-features --features "smallvec"
        ;;

    "doc")
        cargo doc
        ;;

    "bench")
        cargo bench
        ;;

    "coverage")
        RUSTFLAGS="--cfg procmacro2_semver_exempt" cargo install --force cargo-tarpaulin
        cargo build --example addr2line
        cargo tarpaulin --verbose --ciserver travis-ci --coveralls "$TRAVIS_JOB_ID";
        ;;

    "cross")
        rustup target add $TARGET
        cargo install cross --force
        cross test --target $TARGET $GIMLI_PROFILE --features "$GIMLI_FEATURES"
        ;;

    *)
        echo "Error! Unknown \$GIMLI_JOB: $GIMLI_JOB"
        exit 1
esac
