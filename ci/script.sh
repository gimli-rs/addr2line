#!/usr/bin/env bash

set -ex

case "$GIMLI_JOB" in
    "build")
        if [ "$TRAVIS_OS_NAME" = "linux" ]; then
            cargo test
            cargo test --release
        else
            cargo build
            cargo build --release
        fi
        ;;

    "features")
        cargo build --no-default-features --features "std"
        cargo build --no-default-features --features "std cpp_demangle"
        cargo build --no-default-features --features "std rustc-demangle"
        cargo build --no-default-features --features "std object"
        ;;

    "nightly_features")
        cargo build --no-default-features --features "alloc"
        ;;

    "doc")
        cargo doc
        ;;

    "bench")
        cargo bench
        ;;

    "coverage")
        RUSTFLAGS="--cfg procmacro2_semver_exempt" cargo install --force cargo-tarpaulin
        cargo tarpaulin --verbose --no-count --ciserver travis-ci --coveralls "$TRAVIS_JOB_ID";
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
