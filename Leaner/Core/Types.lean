import Lean

namespace Leaner.Core

open Lean

structure Position where
  line : Nat
  column : Nat
  deriving Inhabited, BEq, Hashable, Repr

instance : ToString Position where
  toString p := s!"{p.line}:{p.column}"

instance : Ord Position where
  compare a b :=
    match compare a.line b.line with
    | .eq => compare a.column b.column
    | ord => ord

structure Range where
  start : Position
  stop : Position
  deriving Inhabited, BEq, Hashable, Repr

instance : ToString Range where
  toString r := s!"{r.start}-{r.stop}"

def Position.fromRawPos (source : String) (pos : String.Pos.Raw) : Position := Id.run do
  let mut line := 1
  let mut col := 1
  let mut i : String.Pos.Raw := ⟨0⟩
  while i < pos do
    if String.Pos.Raw.get source i == '\n' then
      line := line + 1
      col := 1
    else
      col := col + 1
    i := String.Pos.Raw.next source i
  { line, column := col }

def Position.toRawPos (source : String) (pos : Position) : String.Pos.Raw := Id.run do
  let mut line := 1
  let mut col := 1
  let mut i : String.Pos.Raw := ⟨0⟩
  let endPos := source.endPos.offset
  while i < endPos && (line < pos.line || (line == pos.line && col < pos.column)) do
    if String.Pos.Raw.get source i == '\n' then
      line := line + 1
      col := 1
    else
      col := col + 1
    i := String.Pos.Raw.next source i
  i

structure SourceFile where
  path : System.FilePath
  content : String
  deriving Inhabited

namespace SourceFile

def read (path : System.FilePath) : IO SourceFile := do
  let content ← IO.FS.readFile path
  return { path, content }

def lineCount (sf : SourceFile) : Nat :=
  sf.content.splitOn "\n" |>.length

def getLine (sf : SourceFile) (lineNum : Nat) : Option String :=
  let lines := sf.content.splitOn "\n"
  if lineNum > 0 && lineNum <= lines.length then
    lines[lineNum - 1]?
  else
    none

def positionAt (sf : SourceFile) (pos : String.Pos.Raw) : Position :=
  Position.fromRawPos sf.content pos

def rawPosAt (sf : SourceFile) (pos : Position) : String.Pos.Raw :=
  Position.toRawPos sf.content pos

end SourceFile

inductive Severity where
  | error
  | warning
  | info
  | hint
  deriving Inhabited, BEq, Repr

instance : ToString Severity where
  toString
    | .error => "error"
    | .warning => "warning"
    | .info => "info"
    | .hint => "hint"

structure Diagnostic where
  severity : Severity
  range : Range
  message : String
  source : Option String := none
  deriving Inhabited, Repr

namespace Diagnostic

def error (range : Range) (message : String) (source : Option String := none) : Diagnostic :=
  { severity := .error, range, message, source }

def warning (range : Range) (message : String) (source : Option String := none) : Diagnostic :=
  { severity := .warning, range, message, source }

def info (range : Range) (message : String) (source : Option String := none) : Diagnostic :=
  { severity := .info, range, message, source }

def hint (range : Range) (message : String) (source : Option String := none) : Diagnostic :=
  { severity := .hint, range, message, source }

end Diagnostic

instance : ToString Diagnostic where
  toString d :=
    let src := match d.source with
      | some s => s!" [{s}]"
      | none => ""
    s!"{d.range}: {d.severity}{src}: {d.message}"

structure TextEdit where
  range : Range
  newText : String
  deriving Inhabited, BEq, Repr

structure ProcessResult where
  diagnostics : Array Diagnostic := #[]
  edits : Array TextEdit := #[]
  deriving Inhabited

end Leaner.Core
