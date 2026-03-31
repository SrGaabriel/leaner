import Lean
import Kenosis.Toml

namespace Leaner.Core.Config

open Kenosis.Toml

private def lookup (v : TomlValue) (key : String) : Option TomlValue :=
  match v with
  | .table kvs => kvs.find? (·.1 == key) |>.map (·.2)
  | _ => none

private def str? : TomlValue → Option String
  | .str s => some s
  | _ => none

private def bool? : TomlValue → Option Bool
  | .bool b => some b
  | _ => none

private def nat? : TomlValue → Option Nat
  | .num n => if n >= 0 then some n.toUInt32.toNat else none
  | _ => none

inductive IndentStyle where
  | spaces (count : Nat)
  | tabs
  deriving Inhabited, BEq, Repr

instance : ToString IndentStyle where
  toString
    | .spaces n => s!"{n} spaces"
    | .tabs => "tabs"

inductive LineEnding where
  | lf
  | crlf
  | auto
  deriving Inhabited, BEq, Repr

structure FormatterConfig where
  maxLineWidth : Nat := 100
  indentStyle : IndentStyle := .spaces 2
  lineEnding : LineEnding := .lf
  insertFinalNewline : Bool := true
  trimTrailingWhitespace : Bool := true
  spaceAfterOpenBracket : Bool := false
  spaceBeforeCloseBracket : Bool := false
  alignDefinitions : Bool := false
  alignMatchArms : Bool := false
  maxBlankLines : Nat := 2
  deriving Inhabited, Repr

structure LinterConfig where
  warnUnusedVariables : Bool := true
  warnUnusedImports : Bool := true
  checkNamingConventions : Bool := true
  disabledRules : Array String := #[]
  deriving Inhabited, Repr

structure DCEConfig where
  entryPoints : Array String := #[]
  analyzeDependencies : Bool := true
  excludePatterns : Array String := #[]
  deriving Inhabited, Repr

structure Config where
  formatter : FormatterConfig := {}
  linter : LinterConfig := {}
  dce : DCEConfig := {}
  includePatterns : Array String := #["**/*.lean"]
  excludePatterns : Array String := #[".lake/**", "lake-packages/**"]
  deriving Inhabited, Repr

namespace Config

def default : Config := {}

def indentString (cfg : FormatterConfig) : String :=
  match cfg.indentStyle with
  | .spaces n => "".pushn ' ' n
  | .tabs => "\t"

def lineEndingString (cfg : FormatterConfig) : String :=
  match cfg.lineEnding with
  | .lf => "\n"
  | .crlf => "\r\n"
  | .auto => "\n"

end Config

def configFileName : String := "leaner.toml"

def findConfigFile (startDir : System.FilePath) : IO (Option System.FilePath) := do
  let mut dir := startDir
  for _ in [:100] do
    let configPath := dir / configFileName
    if ← configPath.pathExists then
      return some configPath
    let parent := dir.parent
    match parent with
    | some p =>
      if p == dir then return none
      dir := p
    | none => return none
  return none

private def parseIndentStyle (v : TomlValue) : IndentStyle :=
  match str? v with
  | some "tabs" => .tabs
  | some s =>
    if s.endsWith " spaces" then
      match (s.dropEnd 7).toString.toNat? with
      | some n => .spaces n
      | none => .spaces 2
    else
      match s.toNat? with
      | some n => .spaces n
      | none => .spaces 2
  | none =>
    match nat? v with
    | some n => .spaces n
    | none => .spaces 2

private def parseLineEnding (v : TomlValue) : LineEnding :=
  match str? v with
  | some "lf" => .lf
  | some "crlf" => .crlf
  | some "auto" => .auto
  | _ => .lf

private def loadFormatterConfig (table : TomlValue) : FormatterConfig := Id.run do
  let mut cfg : FormatterConfig := {}

  if let some v := lookup table "max_line_width" then
    if let some n := nat? v then
      cfg := { cfg with maxLineWidth := n }

  if let some v := lookup table "indent_style" then
    cfg := { cfg with indentStyle := parseIndentStyle v }

  if let some v := lookup table "indent_size" then
    if let some n := nat? v then
      cfg := { cfg with indentStyle := .spaces n }

  if let some v := lookup table "line_ending" then
    cfg := { cfg with lineEnding := parseLineEnding v }

  if let some v := lookup table "insert_final_newline" then
    if let some b := bool? v then
      cfg := { cfg with insertFinalNewline := b }

  if let some v := lookup table "trim_trailing_whitespace" then
    if let some b := bool? v then
      cfg := { cfg with trimTrailingWhitespace := b }

  if let some v := lookup table "max_blank_lines" then
    if let some n := nat? v then
      cfg := { cfg with maxBlankLines := n }

  if let some v := lookup table "align_definitions" then
    if let some b := bool? v then
      cfg := { cfg with alignDefinitions := b }

  if let some v := lookup table "align_match_arms" then
    if let some b := bool? v then
      cfg := { cfg with alignMatchArms := b }

  cfg

private def loadConfigFromToml (toml : TomlValue) : Config := Id.run do
  let mut cfg : Config := {}

  if let some formatterTable := lookup toml "formatter" then
    cfg := { cfg with formatter := loadFormatterConfig formatterTable }

  cfg := { cfg with formatter := loadFormatterConfig toml }

  cfg

def loadConfig (path : System.FilePath) : IO Config := do
  let content ← IO.FS.readFile path
  match TomlReader.run content parseTomlDocument with
  | .ok toml => return loadConfigFromToml toml
  | .error e =>
    IO.eprintln s!"Warning: Failed to parse {path}: {e}"
    return Config.default

def loadConfigFrom (dir : System.FilePath) : IO Config := do
  match ← findConfigFile dir with
  | some path => loadConfig path
  | none => return Config.default

def loadConfigFromCwd : IO Config := do
  let cwd ← IO.currentDir
  loadConfigFrom cwd

def mergeCliOptions (cfg : FormatterConfig) (width? : Option Nat) (indent? : Option Nat) : FormatterConfig :=
  let cfg := match width? with
    | some w => { cfg with maxLineWidth := w }
    | none => cfg
  let cfg := match indent? with
    | some i => { cfg with indentStyle := .spaces i }
    | none => cfg
  cfg

end Leaner.Core.Config
