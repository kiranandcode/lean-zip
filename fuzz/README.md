# Fuzzing lean-zip

```bash
LEAN_CC=afl-clang-fast lake build lean-zip

# zip extract
afl-fuzz -i fuzz/corpus/zip -o fuzz/output -t 10000 -m none \
    -- .lake/build/bin/lean-zip extract @@ -d /tmp/fuzz-out

# gzip
afl-fuzz -i fuzz/corpus/gzip -o fuzz/gzip-output -t 10000 -m none \
    -- .lake/build/bin/lean-zip gunzip @@

# raw inflate
afl-fuzz -i fuzz/corpus/inflate -o fuzz/inflate-output -t 10000 -m none \
    -- .lake/build/bin/lean-zip inflate @@

# tar
afl-fuzz -i fuzz/corpus/tar -o fuzz/tar-output -t 10000 -m none \
    -- .lake/build/bin/lean-zip untar @@ -d /tmp/fuzz-out

# compression
afl-fuzz -i fuzz/corpus/compress -o fuzz/compress-output -t 30000 -m none \
    -- .lake/build/bin/lean-zip create /tmp/fuzz.zip @@
```

ASAN: `LEAN_CC=afl-clang-fast-asan` (wrapper around `afl-clang-fast -fsanitize=address`).

Panic detection: `LEAN_ABORT_ON_PANIC=1` turns Lean panics into SIGABRT.
