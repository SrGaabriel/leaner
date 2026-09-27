import Leaner
import Cli

open Cli
open Lean
open Leaner.Formatter
open Leaner.Core
open Leaner.Core.Parser
open Leaner.Weeder

def version := "0.2.0"

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

  let mut hasError := false
  let mut allFiles : Array System.FilePath := #[]
  for file in files do
    let path : System.FilePath := file
    if !(← path.pathExists) then
      IO.eprintln s!"Error: File not found: {file}"
      hasError := true
      continue
    allFiles := allFiles ++ (← collectLeanFiles path)

  let baseEnv ← initParseEnv
  let env ← (do
    let mut allImports : Lean.NameSet := {}
    for path in allFiles do
      try
        let source := (← IO.FS.readFile path).replace "\r\n" "\n"
        let inputCtx := Lean.Parser.mkInputContext source path.toString
        let (header, _, _) ← Lean.Parser.parseHeader inputCtx
        for imp in Lean.Elab.headerToImports header do
          allImports := allImports.insert imp.module
      catch _ => pure ()
    if allImports.isEmpty then
      return baseEnv
    let importable ← allImports.toArray.filterM fun n => do
      try
        let path ← Lean.findOLean n
        path.pathExists
      catch _ => pure false
    if importable.isEmpty then return baseEnv
    let imports : Array Lean.Import := importable.map ({ module := · })
    try
      unsafe Lean.enableInitializersExecution
      Lean.importModules imports {} (loadExts := true)
    catch e =>
      IO.eprintln s!"Warning: failed to load full project env ({e.toString}); falling back to Init-only"
      return baseEnv) <|> pure baseEnv

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
          IO.eprintln s!"Warning: only minimally formatted (parser couldn't fully parse): {file}"
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

private def parseEntryPoints (s : String) : Array Name :=
  s.splitOn "," |>.toArray.filterMap fun part =>
    let part := part.trimAscii.toString
    if part.isEmpty then none
    else some part.toName

def weedHandler (p : Parsed) : IO UInt32 := do
  let positional := p.variableArgsAs! String
  let roots : Array System.FilePath ←
    if positional.isEmpty then do
      let cwd ← IO.currentDir
      pure #[cwd]
    else
      pure (positional.map (System.FilePath.mk ·))
  let entryStr := (p.flag? "entry-points").map (·.as! String) |>.getD ""
  let extraEntries := parseEntryPoints entryStr
  let json := p.hasFlag "json"

  let baseConfig ← Config.loadConfigFromCwd
  let dceCfg := baseConfig.dce

  let cfg : RootConfig := {
    entryPoints := extraEntries ++ dceCfg.entryPoints.map (·.toName)
    includeMain := !p.hasFlag "no-main"
    includeExported := !p.hasFlag "no-exports"
    includeExtern := !p.hasFlag "no-extern"
    includeInit := !p.hasFlag "no-init"
    includeInstances := !p.hasFlag "no-instances"
    includeSimp := !p.hasFlag "no-simp"
    includeExt := !p.hasFlag "no-ext"
    flagTheorems := p.hasFlag "theorems"
    flagAxioms := p.hasFlag "axioms"
  }

  for root in roots do
    if !(← root.pathExists) then
      IO.eprintln s!"Error: project root not found: {root}"
      return 1

  let result ← runWeederMulti roots cfg

  if json then
    let escape (s : String) : String :=
      s.foldl (init := "") fun acc c =>
        match c with
        | '"' => acc ++ "\\\""
        | '\\' => acc ++ "\\\\"
        | '\n' => acc ++ "\\n"
        | _ => acc.push c
    let entries := result.dead.toList.map fun d =>
      "    {\"name\":\"" ++ escape d.name.toString ++
      "\",\"module\":\"" ++ escape d.module.toString ++
      "\",\"file\":\"" ++ escape d.source.toString ++
      "\",\"line\":" ++ toString d.range.start.line ++
      ",\"column\":" ++ toString d.range.start.column ++ "}"
    IO.println ("{\"summary\":\"" ++ escape result.summary ++ "\",")
    IO.println " \"dead\":["
    IO.println (String.intercalate ",\n" entries)
    IO.println " ]}"
  else
    if !result.dead.isEmpty then
      IO.println result.formatDead
      IO.println ""
    IO.println result.summary

  if p.hasFlag "delete" && !result.dead.isEmpty then
    match result.env?, result.graph?, result.reached? with
    | some env, some g, some reached =>
      let (filesChanged, skipped) ← applyDeletions env g reached result.dead
      let kept := result.dead.size - skipped.size
      IO.println s!"Deleted {kept} declaration(s) across {filesChanged} file(s)."
      if !skipped.isEmpty then
        IO.println s!"Skipped {skipped.size} declaration(s) (referenced by surviving code or in a partially-dead `mutual` block):"
        for d in skipped do
          IO.println s!"  {d.source}: `{d.name}`"
    | _, _, _ =>
      IO.eprintln "Error: --delete requires the analysis environment/graph, but they were not kept."
      return 1

  if result.dead.isEmpty then return 0 else return 1

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

def weedCmd : Cmd := `[Cli|
  weed VIA weedHandler; [version]
  "Find unused declarations across an entire Lean 4 project"

  FLAGS:
    "entry-points" : String;  "Comma-separated additional entry-point declarations"
    "no-main";                "Do not treat `main` as an entry point"
    "no-exports";             "Do not treat @[export] declarations as roots"
    "no-extern";              "Do not treat @[extern] declarations as roots"
    "no-init";                "Do not treat @[init]/@[builtin_init] declarations as roots"
    "no-instances";           "Do not treat type-class instances as roots"
    "no-simp";                "Do not treat @[simp] lemmas as roots"
    "no-ext";                 "Do not treat @[ext] theorems as roots"
    t, theorems;              "Also flag unused theorems (off by default — theorems exist for proof, not call)"
    a, axioms;                "Also flag unused axioms (off by default — axioms are interface stubs)"
    "delete";                 "DANGEROUS: rewrite each source file in place to remove every dead declaration. Make sure your work is committed first."
    j, json;                  "Emit JSON output"

  ARGS:
    ...root : String;         "One or more project root directories (defaults to CWD). To analyse a Lake-required dependency, pass its package directory explicitly, e.g. `weed . .lake/packages/Lib`"
]

def leanerCmd : Cmd := `[Cli|
  leaner NOOP; [version]
  "Lean 4 code quality tools: formatter, linter, and dead code eliminator"

  SUBCOMMANDS:
    formatCmd;
    checkCmd;
    weedCmd
]

def main (args : List String) : IO UInt32 :=
  leanerCmd.validate args
