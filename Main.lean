import Leaner
import Cli

open Cli
open Leaner.Formatter
open Leaner.Core

def loadConfig (width? : Option Nat) (indent? : Option Nat) : IO Config.FormatterConfig := do
  let baseConfig ← Config.loadConfigFromCwd

  return Config.mergeCliOptions baseConfig.formatter width? indent?

def formatHandler (p : Parsed) : IO UInt32 := do
  let files := p.variableArgsAs! String
  let check := p.hasFlag "check"
  let diff := p.hasFlag "diff"
  let width? := (p.flag? "width").bind (·.as? Nat)
  let indent? := (p.flag? "indent").bind (·.as? Nat)

  let config ← loadConfig width? indent?

  if files.isEmpty then
    IO.eprintln "Error: No files specified"
    return 1

  let mut hasError := false
  let mut changedCount := 0

  for file in files do
    let path : System.FilePath := file
    if !(← path.pathExists) then
      IO.eprintln s!"Error: File not found: {file}"
      hasError := true
      continue

    let result ← formatFile path config

    for diag in result.diagnostics do
      IO.eprintln s!"{file}: {diag}"

    if result.changed then
      changedCount := changedCount + 1

      if check then
        IO.println s!"Would reformat: {file}"
      else if diff then
        if let some d ← formatToDiff path config then
          IO.println d
      else
        IO.FS.writeFile path result.output
        IO.println s!"Formatted: {file}"

  if check && changedCount > 0 then
    IO.println s!"\n{changedCount} file(s) would be reformatted"
    return 1

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

  let mut needsFormat := false

  for file in files do
    let path : System.FilePath := file
    if !(← path.pathExists) then
      IO.eprintln s!"Error: File not found: {file}"
      continue

    let result ← formatFile path config
    if result.changed then
      IO.println s!"Would reformat: {file}"
      needsFormat := true

  if needsFormat then
    return 1
  else
    IO.println "All files are properly formatted"
    return 0

def formatCmd : Cmd := `[Cli|
  format VIA formatHandler; ["0.1.0"]
  "Format Lean 4 source files"

  FLAGS:
    c, check;               "Check if files are formatted without modifying them"
    d, diff;                "Show diff instead of modifying files"
    w, width : Nat;         "Maximum line width (default: 100, or from leaner.toml)"
    i, indent : Nat;        "Indentation width in spaces (default: 2, or from leaner.toml)"

  ARGS:
    ...files : String;      "Files to format"
]

def checkCmd : Cmd := `[Cli|
  check VIA checkHandler; ["0.1.0"]
  "Check if Lean 4 source files are properly formatted"

  FLAGS:
    w, width : Nat;         "Maximum line width (default: 100, or from leaner.toml)"
    i, indent : Nat;        "Indentation width in spaces (default: 2, or from leaner.toml)"

  ARGS:
    ...files : String;      "Files to check"
]

def leanerCmd : Cmd := `[Cli|
  leaner NOOP; ["0.1.0"]
  "Lean 4 code quality tools: formatter, linter, and dead code eliminator"

  SUBCOMMANDS:
    formatCmd;
    checkCmd
]

def main (args : List String) : IO UInt32 :=
  leanerCmd.validate args
