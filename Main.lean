import Leaner
import Cli

open Cli
open Lean
open Leaner.Formatter
open Leaner.Core
open Leaner.Core.Parser

def version := "0.1.2"

partial def collectLeanFiles (path : System.FilePath) : IO (Array System.FilePath) := do
  if path.components.any (· == ".lake") then
    return #[]
  if ← path.isDir then
    let mut files := #[]
    for entry in ← path.readDir do
      files := files ++ (← collectLeanFiles entry.path)
    return files
  else if path.extension == some "lean" then
    return #[path]
  else
    return #[]

def loadConfig (width? : Option Nat) (indent? : Option Nat) : IO Config.FormatterConfig := do
  let baseConfig ← Config.loadConfigFromCwd
  return Config.mergeCliOptions baseConfig.formatter width? indent?

abbrev MtimeCache := Lean.RBMap String (Int × UInt32) compare

def getCache : IO System.FilePath := do
  let tmp ← IO.getEnv "TEMP" >>= fun t =>
    IO.getEnv "TMPDIR" >>= fun t2 =>
    return t.orElse (fun _ => t2) |>.getD "/tmp"
  let cwd ← IO.currentDir
  let key := cwd.toString.map (fun c => if c.isAlphanum then c else '_')
  return (tmp : System.FilePath) / s!"leaner_{key}.cache"

def cacheHeader : String := s!"leaner-cache v{version}"

def loadMtimeCache : IO MtimeCache := do
  let path ← getCache
  if !(← path.pathExists) then return RBMap.empty
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n"
  match lines with
  | header :: rest =>
    if header != cacheHeader then return RBMap.empty
    let mut cache : MtimeCache := RBMap.empty
    for line in rest do
      if line.isEmpty then continue
      match line.splitOn "\t" with
      | [p, sec, nsec] =>
        if let (some s, some n) := (sec.toInt?, nsec.toNat?) then
          cache := cache.insert p (s, n.toUInt32)
      | _ => pure ()
    return cache
  | _ => return RBMap.empty

def saveMtimeCache (cache : MtimeCache) : IO Unit := do
  let lines := cache.toList.map fun (p, mtime) => s!"{p}\t{mtime.1}\t{mtime.2}"
  let path ← getCache
  IO.FS.writeFile path (cacheHeader ++ "\n" ++ String.intercalate "\n" lines ++ "\n")

def getFileMtime (path : System.FilePath) : IO (Int × UInt32) := do
  let fileInfo ← path.metadata
  return (fileInfo.modified.sec, fileInfo.modified.nsec)

def isCachedFormatted (cache : MtimeCache) (path : System.FilePath) : IO Bool := do
  match cache.find? path.toString with
  | none => return false
  | some (cachedSec, cachedNsec) =>
    let (sec, nsec) ← getFileMtime path
    return sec == cachedSec && nsec == cachedNsec

def formatHandler (p : Parsed) : IO UInt32 := do
  let files := p.variableArgsAs! String
  let check := p.hasFlag "check"
  let diff := p.hasFlag "diff"
  let noCache := p.hasFlag "no-cache"
  let width? := (p.flag? "width").bind (·.as? Nat)
  let indent? := (p.flag? "indent").bind (·.as? Nat)

  let config ← loadConfig width? indent?

  if files.isEmpty then
    IO.eprintln "Error: No files specified"
    return 1

  let env ← initParseEnv

  let mut hasError := false
  let mut allFiles : Array System.FilePath := #[]
  for file in files do
    let path : System.FilePath := file
    if !(← path.pathExists) then
      IO.eprintln s!"Error: File not found: {file}"
      hasError := true
      continue
    allFiles := allFiles ++ (← collectLeanFiles path)

  let useCache := !check && !diff && !noCache
  let cache ← if useCache then loadMtimeCache else pure RBMap.empty

  let tasks ← allFiles.mapM fun path => do
    if useCache && (← isCachedFormatted cache path) then
      return (path, none)
    let task ← IO.asTask do
      let source := (← IO.FS.readFile path).replace "\r\n" "\n"
      let result ← formatSource source env path.toString config
      return (source, result)
    return (path, some task)

  let mut formatted := 0
  let mut unchanged := 0
  let mut skipped := 0
  let mut nPartial := 0
  let mut newCache := cache

  for (path, task?) in tasks do
    let file := path.toString
    match task? with
    | none => skipped := skipped + 1
    | some task =>
      match task.get with
      | .error e =>
        IO.eprintln s!"Error: {file}: {e}"
        hasError := true
      | .ok (source, result) =>
        for diag in result.diagnostics do
          IO.eprintln s!"{file}: {diag}"

        if result.isPartial then
          nPartial := nPartial + 1
        else if result.changed then
          formatted := formatted + 1
          if check then
            IO.println s!"Would reformat: {file}"
          else if diff then
            IO.print s!"--- {path}\n+++ {path} (formatted)\n"
            for line in simpleDiff source result.output do
              IO.println s!"{line}"
          else
            IO.FS.writeFile path result.output
            let mtime ← getFileMtime path
            newCache := newCache.insert file mtime
        else
          unchanged := unchanged + 1
          if !check && !diff then
            let mtime ← getFileMtime path
            newCache := newCache.insert file mtime

  if useCache then
    saveMtimeCache newCache

  let parts := #[
    if formatted > 0 then s!"{formatted} formatted" else "",
    if skipped   > 0 then s!"{skipped} skipped" else "",
    if unchanged > 0 then s!"{unchanged} unchanged" else "",
    if nPartial  > 0 then s!"{nPartial} partial" else ""
  ].filter (· != "")
  IO.println (String.intercalate ", " parts.toList)

  if check && formatted > 0 then return 1
  if hasError then return 1
  return 0

def checkHandler (p : Parsed) : IO UInt32 := do
  let files := p.variableArgsAs! String
  let width? := (p.flag? "width").bind (·.as? Nat)
  let indent? := (p.flag? "indent").bind (·.as? Nat)

  let config ← loadConfig width? indent?

  if files.isEmpty then
    IO.eprintln "Error: No files specified"
    return 1

  let env ← initParseEnv

  let mut allFiles : Array System.FilePath := #[]
  for file in files do
    let path : System.FilePath := file
    if !(← path.pathExists) then
      IO.eprintln s!"Error: File not found: {file}"
      continue
    allFiles := allFiles ++ (← collectLeanFiles path)

  let tasks ← allFiles.mapM fun path => do
    let task ← IO.asTask (formatFile path env config)
    return (path, task)

  let mut formatted := 0
  let mut unchanged := 0
  let mut nPartial  := 0

  for (path, task) in tasks do
    match task.get with
    | .error e =>
      IO.eprintln s!"Error: {path}: {e}"
    | .ok result =>
      if result.isPartial then
        nPartial := nPartial + 1
      else if result.changed then
        IO.println s!"Would reformat: {path}"
        formatted := formatted + 1
      else
        unchanged := unchanged + 1

  let parts := #[
    if formatted > 0 then s!"{formatted} would reformat" else "",
    if unchanged > 0 then s!"{unchanged} unchanged" else "",
    if nPartial  > 0 then s!"{nPartial} partial" else ""
  ].filter (· != "")
  IO.println (String.intercalate ", " parts.toList)

  if formatted > 0 then return 1
  return 0

def formatCmd : Cmd := `[Cli|
  format VIA formatHandler; [version]
  "Format Lean 4 source files"

  FLAGS:
    c, check;               "Check if files are formatted without modifying them"
    d, diff;                "Show diff instead of modifying files"
    "no-cache";             "Ignore and discard the mtime cache"
    w, width : Nat;         "Maximum line width (default: 100, or from leaner.toml)"
    i, indent : Nat;        "Indentation width in spaces (default: 2, or from leaner.toml)"

  ARGS:
    ...files : String;      "Files to format"
]

def checkCmd : Cmd := `[Cli|
  check VIA checkHandler; [version]
  "Check if Lean 4 source files are properly formatted"

  FLAGS:
    w, width : Nat;         "Maximum line width (default: 100, or from leaner.toml)"
    i, indent : Nat;        "Indentation width in spaces (default: 2, or from leaner.toml)"

  ARGS:
    ...files : String;      "Files to check"
]

def leanerCmd : Cmd := `[Cli|
  leaner NOOP; [version]
  "Lean 4 code quality tools: formatter, linter, and dead code eliminator"

  SUBCOMMANDS:
    formatCmd;
    checkCmd
]

def main (args : List String) : IO UInt32 :=
  leanerCmd.validate args
