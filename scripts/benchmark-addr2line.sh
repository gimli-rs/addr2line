#!/bin/bash

ADDR2LINE_BINARY="target/release/addr2line"
SYMBOLS_PATH="symbols.txt"

benchmark() {
    $ADDR2LINE_BINARY --all -a -e $1 | grep 0x | shuf --random-source=<(yes 123456789) | head -n $2 > $SYMBOLS_PATH
    BINARY_NAME=`basename -s .debug $1`
    echo "=== Running benchmark for $BINARY_NAME (`du -hs $1 | cut -f1`, $2 symbol queries) ==="

    hyperfine --style=color -n "gimli-addr2line $BINARY_NAME" -n "binutils-addr2line $BINARY_NAME" -n "llvm-addr2line $BINARY_NAME" -n "elfutils-addr2line $BINARY_NAME" \
        --min-runs=1 --sort=command -L tool $ADDR2LINE_BINARY,/usr/bin/addr2line,/usr/bin/llvm-addr2line,/usr/bin/eu-addr2line "{tool} -af -e $1 < $SYMBOLS_PATH"
}

benchmark /usr/lib/debug/usr/lib64/firefox/libxul.so.debug 500
benchmark /usr/lib/debug/usr/bin/clangd-18.debug 500
benchmark /usr/lib/debug/usr/bin/mold.debug 1000
benchmark target/release/addr2line 5000
benchmark /usr/lib/debug/usr/lib/postgresql16/bin/postgres.debug 10000

rm -f $SYMBOLS_PATH
