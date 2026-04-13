import Zip

/-! CLI interface to lean-zip's pure-Lean ZIP operations.

Usage:
  lean-zip create <archive.zip> <file1> [file2 ...]
  lean-zip create -r <archive.zip> <dir>
  lean-zip list <archive.zip>
  lean-zip extract <archive.zip> [-d <outdir>]

All operations use the pure Lean implementations (no C FFI).
-/

private def leftpad (s : String) (n : Nat) : String :=
  let pad := n - s.length
  "".pushn ' ' pad ++ s

def printUsage : IO Unit := do
  IO.eprintln "lean-zip — pure Lean ZIP tool"
  IO.eprintln ""
  IO.eprintln "Usage:"
  IO.eprintln "  lean-zip create <archive.zip> <file1> [file2 ...]"
  IO.eprintln "  lean-zip create -r <archive.zip> <dir>"
  IO.eprintln "  lean-zip list <archive.zip>"
  IO.eprintln "  lean-zip extract <archive.zip> [-d <outdir>]"

def cmdCreate (args : List String) : IO Unit := do
  match args with
  | "-r" :: archive :: dirs =>
    if dirs.isEmpty then
      throw (IO.userError "create -r: no directory specified")
    for dir in dirs do
      Archive.createFromDir archive dir
    IO.println s!"created {archive}"
  | archive :: files =>
    if files.isEmpty then
      throw (IO.userError "create: no files specified")
    let pairs : Array (String × System.FilePath) := files.toArray.map fun (f : String) =>
      let name : String := if f.startsWith "/" then (f.drop 1).toString else f
      (name, (f : System.FilePath))
    Archive.create archive pairs
    IO.println s!"created {archive}"
  | _ =>
    throw (IO.userError "create: expected <archive.zip> <files...>")

def cmdList (args : List String) : IO Unit := do
  match args with
  | [archive] =>
    let entries ← Archive.list archive
    IO.println s!"  Length      Compressed  Method  Name"
    IO.println s!"  ----------  ----------  ------  ----"
    let mut totalSize : UInt64 := 0
    let mut totalComp : UInt64 := 0
    for entry in entries do
      let methodStr := if entry.method == 8 then "Deflate" else "Stored "
      let uSize := toString entry.uncompressedSize
      let cSize := toString entry.compressedSize
      IO.println s!"  {leftpad uSize 10}  {leftpad cSize 10}  {methodStr}  {entry.path}"
      totalSize := totalSize + entry.uncompressedSize
      totalComp := totalComp + entry.compressedSize
    IO.println s!"  ----------  ----------          ----"
    let tSize := toString totalSize
    let tComp := toString totalComp
    IO.println s!"  {leftpad tSize 10}  {leftpad tComp 10}          {entries.size} file(s)"
  | _ =>
    throw (IO.userError "list: expected <archive.zip>")

def cmdGunzip (args : List String) : IO Unit := do
  match args with
  | [input] =>
    let data ← IO.FS.readBinFile input
    match Zip.Native.GzipDecode.decompress data with
    | .ok result => IO.FS.writeBinFile (input ++ ".out") result
                    IO.println s!"decompressed {data.size} → {result.size} bytes"
    | .error e => throw (IO.userError s!"gzip error: {e}")
  | _ => throw (IO.userError "gunzip: expected <file.gz>")

def cmdInflate (args : List String) : IO Unit := do
  match args with
  | [input] =>
    let data ← IO.FS.readBinFile input
    match Zip.Native.Inflate.inflate data with
    | .ok result => IO.FS.writeBinFile (input ++ ".out") result
                    IO.println s!"inflated {data.size} → {result.size} bytes"
    | .error e => throw (IO.userError s!"inflate error: {e}")
  | _ => throw (IO.userError "inflate: expected <file>")

def cmdUntargz (args : List String) : IO Unit := do
  match args with
  | [input] =>
    Tar.extractTarGzNative input "."
    IO.println "done"
  | [input, "-d", dir] =>
    Tar.extractTarGzNative input dir
    IO.println "done"
  | _ => throw (IO.userError "untar-gz: expected <file.tar.gz> [-d <outdir>]")

def cmdUntar (args : List String) : IO Unit := do
  match args with
  | [input] =>
    IO.FS.withFile input .read fun h => do
      let stream := IO.FS.Stream.ofHandle h
      Tar.extract stream "."
    IO.println "done"
  | [input, "-d", dir] =>
    IO.FS.withFile input .read fun h => do
      let stream := IO.FS.Stream.ofHandle h
      Tar.extract stream dir
    IO.println "done"
  | _ => throw (IO.userError "untar: expected <file.tar> [-d <outdir>]")

def cmdExtract (args : List String) : IO Unit := do
  let (archive, outDir) ← match args with
    | [archive] => pure (archive, ("." : System.FilePath))
    | [archive, "-d", dir] => pure (archive, (dir : System.FilePath))
    | _ => throw (IO.userError "extract: expected <archive.zip> [-d <outdir>]")
  let entries ← Archive.list archive
  IO.println s!"extracting {entries.size} file(s) from {archive} to {outDir}"
  Archive.extract archive outDir (useNative := true)
  IO.println "done"

def main (args : List String) : IO Unit := do
  match args with
  | "create" :: rest => cmdCreate rest
  | "list" :: rest => cmdList rest
  | "extract" :: rest => cmdExtract rest
  | "gunzip" :: rest => cmdGunzip rest
  | "inflate" :: rest => cmdInflate rest
  | "untar" :: rest => cmdUntar rest
  | "untar-gz" :: rest => cmdUntargz rest
  | _ => printUsage; if args.isEmpty then pure () else throw (IO.userError "unknown command")
