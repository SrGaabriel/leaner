import Lean
import Leaner.Core
import Leaner.Formatter.Doc
import Leaner.Formatter.SyntaxPrinter

namespace Leaner.Formatter

open Lean
open Leaner.Core
open Leaner.Core.Comments
open Leaner.Formatter.Doc
open Leaner.Formatter.SyntaxPrinter

structure FormatResult where
  output : String
  changed : Bool
  diagnostics : Array Diagnostic := #[]
  deriving Inhabited

def postProcess (source : String) (config : Config.FormatterConfig) : String := Id.run do
  let mut result := source

  if config.trimTrailingWhitespace then
    let lines := result.splitOn "\n"
    let lines := lines.map fun line =>
      line.toList.reverse.dropWhile Char.isWhitespace |>.reverse |> String.ofList
    result := "\n".intercalate lines

  if config.insertFinalNewline && !result.endsWith "\n" then
    result := result ++ "\n"

  match config.lineEnding with
  | .lf => result := result.replace "\r\n" "\n"
  | .crlf =>
    result := result.replace "\r\n" "\n"
    result := result.replace "\n" "\r\n"
  | .auto => pure ()

  result

def formatSourceAST (source : String) (env : Environment) (fileName : String := "<input>")
    (config : Config.FormatterConfig := {}) : IO FormatResult := do
  let parseResult ← Parser.parseModule source env { fileName, resolveImports := false }

  match parseResult with
  | .error messages =>
    let diags := messages.toList.map fun msg =>
      Diagnostic.error
        { start := { line := msg.pos.line, column := msg.pos.column }
          stop := { line := msg.pos.line, column := msg.pos.column } }
        msg.caption
        (some "parser")
    return { output := source, changed := false, diagnostics := diags.toArray }

  | .ok stx _ _ messages =>
    if messages.hasErrors then
      return { output := source, changed := false, diagnostics := #[] }

    let formatted := formatToString stx config source

    let output := postProcess formatted config

    return {
      output
      changed := output != source
      diagnostics := #[]
    }

def formatSourceMinimal (source : String) (env : Environment) (fileName : String := "<input>")
    (config : Config.FormatterConfig := {}) : IO FormatResult := do
  let parseResult ← Parser.parseModule source env { fileName, resolveImports := false }

  match parseResult with
  | .error messages =>
    let diags := messages.toList.map fun msg =>
      Diagnostic.error
        { start := { line := msg.pos.line, column := msg.pos.column }
          stop := { line := msg.pos.line, column := msg.pos.column } }
        msg.caption
        (some "parser")
    return { output := source, changed := false, diagnostics := diags.toArray }

  | .ok _ _ _ _ =>
    let output := postProcess source config

    return {
      output
      changed := output != source
      diagnostics := #[]
    }

def formatSource (source : String) (env : Environment) (fileName : String := "<input>")
    (config : Config.FormatterConfig := {}) : IO FormatResult := do
  let result ← formatSourceAST source env fileName config

  if result.diagnostics.size > 0 then
    formatSourceMinimal source env fileName config
  else
    return result

def formatFile (path : System.FilePath) (env : Environment) (config : Config.FormatterConfig := {}) : IO FormatResult := do
  let source := (← IO.FS.readFile path).replace "\r\n" "\n"
  formatSource source env path.toString config

def formatFiles (paths : Array System.FilePath) (env : Environment) (config : Config.FormatterConfig := {})
    : IO (Array (System.FilePath × FormatResult)) := do
  let mut results := #[]
  for path in paths do
    let result ← formatFile path env config
    results := results.push (path, result)
  return results

def formatFileInPlace (path : System.FilePath) (env : Environment) (config : Config.FormatterConfig := {}) : IO Bool := do
  let result ← formatFile path env config
  if result.changed then
    IO.FS.writeFile path result.output
    return true
  return false

def formatFilesInPlace (paths : Array System.FilePath) (env : Environment) (config : Config.FormatterConfig := {})
    : IO (Array System.FilePath) := do
  let mut changed := #[]
  for path in paths do
    if ← formatFileInPlace path env config then
      changed := changed.push path
  return changed

inductive DiffLine where
  | context (line : String)
  | added (line : String)
  | removed (line : String)
  deriving Inhabited

instance : ToString DiffLine where
  toString
    | .context line => s!" {line}"
    | .added line => s!"+{line}"
    | .removed line => s!"-{line}"

def simpleDiff (original formatted : String) : Array DiffLine := Id.run do
  let origLines := original.splitOn "\n"
  let fmtLines := formatted.splitOn "\n"

  let mut result : Array DiffLine := #[]

  let maxLen := max origLines.length fmtLines.length
  for i in [:maxLen] do
    let orig := origLines[i]?
    let fmt := fmtLines[i]?

    match orig, fmt with
    | some o, some f =>
      if o == f then
        result := result.push (.context o)
      else
        result := result.push (.removed o)
        result := result.push (.added f)
    | some o, none =>
      result := result.push (.removed o)
    | none, some f =>
      result := result.push (.added f)
    | none, none => pure ()

  result

def formatToDiff (path : System.FilePath) (env : Environment) (config : Config.FormatterConfig := {})
    : IO (Option String) := do
  let source := (← IO.FS.readFile path).replace "\r\n" "\n"
  let result ← formatFile path env config

  if !result.changed then
    return none

  let diff := simpleDiff source result.output
  let mut output := s!"--- {path}\n+++ {path} (formatted)\n"
  for line in diff do
    output := output ++ s!"{line}\n"
  return some output

end Leaner.Formatter
