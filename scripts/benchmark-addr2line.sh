#!/bin/bash

ADDR2LINE_BINARY="target/release/addr2line"
WORKDIR="scripts/tmp"
SYMBOLS_PATH="$WORKDIR/symbols.txt"

rm -rf $WORKDIR
mkdir -p $WORKDIR
benchmark() {
    $ADDR2LINE_BINARY --all -a -e $1 | grep 0x | shuf --random-source=<(yes 123456789) | head -n $2 > $SYMBOLS_PATH
    BINARY_NAME=`basename -s .debug $1`
    BINARY_SIZE=`du -hs $1 | cut -f1`
    echo "=== Running benchmark for $BINARY_NAME ==="

    hyperfine \
        --style=color \
        -n "gimli addr2line" \
        -n "llvm-addr2line" \
        -n "binutils addr2line" \
        -n "eu-addr2line" \
        --min-runs=1 \
        --sort=command \
        --export-json="$WORKDIR/benchmark-$BINARY_NAME.json" \
        -L input $BINARY_NAME \
        -L tool $ADDR2LINE_BINARY,/usr/bin/llvm-addr2line,/usr/bin/addr2line,/usr/bin/eu-addr2line \
        -L size $BINARY_SIZE \
        -L symbol_queries $2 \
        "{tool} -af -e $1 < $SYMBOLS_PATH"
}

benchmark /usr/lib/debug/usr/lib64/firefox/libxul.so.debug 500
benchmark /usr/lib/debug/usr/bin/clangd-18.debug 500
benchmark /usr/lib/debug/usr/bin/mold.debug 1000
benchmark /usr/lib/debug/usr/lib/postgresql16/bin/postgres.debug 10000

rm -f $SYMBOLS_PATH
