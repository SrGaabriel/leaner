import Lean
import Leaner.Core
import Leaner.Formatter.Doc

namespace Leaner.Formatter.SyntaxPrinter

open Lean
open Leaner.Core
open Leaner.Formatter.Doc

private def triviaLastLineIndent (leading : String) : Option Nat :=
  if !leading.contains '\n' then none
  else
    let lastLine := (leading.splitOn "\n").getLast!
    let n := lastLine.toList.takeWhile (· == ' ') |>.length
    if n >= 2 then some n else none

private def syntaxMinTriviaIndent : Syntax → Nat → Nat
  | .atom (.original l _ _ _) _, acc =>
    match triviaLastLineIndent l.toString with
    | some n => min acc n
    | none   => acc
  | .ident (.original l _ _ _) _ _ _, acc =>
    match triviaLastLineIndent l.toString with
    | some n => min acc n
    | none   => acc
  | .node _ _ args, acc => args.foldl (fun a s => syntaxMinTriviaIndent s a) acc
  | _, acc => acc

def detectSourceIndentUnit (stx : Syntax) : Nat :=
  let minFound := syntaxMinTriviaIndent stx 1000
  if minFound >= 1000 then 2 else minFound

private def countLeadingSpaces (s : String) : Nat :=
  s.toList.takeWhile (· == ' ') |>.length

structure PrinterContext where
  config : Config.FormatterConfig := {}
  source : String
  sourceIndentUnit : Nat := 2
  deriving Inhabited

structure PrinterState where
  pos : Nat := 0
  deriving Inhabited

abbrev PrinterM := ReaderT PrinterContext (StateM PrinterState)

def getConfig : PrinterM Config.FormatterConfig := do
  return (← read).config

def getPos : PrinterM Nat := do
  return (← get).pos

def setPos (pos : Nat) : PrinterM Unit := do
  modify fun s => { s with pos }

private def rescaleSpaces (origSpaces : Nat) (origUnit targetUnit : Nat) : Nat :=
  if origUnit == 0 then origSpaces
  else origSpaces / origUnit * targetUnit + origSpaces % origUnit

private def normalizeLeadingTrivia (trivia : String) (origUnit targetUnit : Nat) : String :=
  if origUnit == targetUnit || !trivia.contains '\n' then trivia
  else
    let lines := trivia.splitOn "\n"
    String.intercalate "\n" (lines.mapIdx fun i line =>
      if i + 1 == lines.length then
        let origSpaces := countLeadingSpaces line
        "".pushn ' ' (rescaleSpaces origSpaces origUnit targetUnit)
      else
        let trimmed := String.ofList (line.toList.dropWhile (· == ' '))
        if trimmed.isEmpty then ""
        else
          let origSpaces := countLeadingSpaces line
          "".pushn ' ' (rescaleSpaces origSpaces origUnit targetUnit) ++ trimmed)

def printAtom (info : SourceInfo) (val : String) : PrinterM Doc := do
  match info with
  | .original l _ t _ =>
    setPos t.stopPos.byteIdx
    let ctx ← read
    let targetUnit := match ctx.config.indentStyle with
      | .spaces n => n
      | .tabs => ctx.sourceIndentUnit
    let leading := normalizeLeadingTrivia l.toString ctx.sourceIndentUnit targetUnit
    return Doc.str leading ++ Doc.str val ++ Doc.str t.toString
  | _ =>
    return Doc.str val

mutual
  partial def printSyntax (stx : Syntax) : PrinterM Doc := do
    match stx with
    | .missing => return Doc.empty
    | .atom info val => printAtom info val
    | .ident info rawVal _ _ =>
      printAtom info rawVal.toString
    | .node _ kind args => printNode kind args

  partial def printNode (kind : SyntaxNodeKind) (args : Array Syntax) : PrinterM Doc := do
    if kind == `choice then
      if !args.isEmpty then printSyntax args[0]!
      else return Doc.empty
    else
      printChildren args

  partial def printChildren (args : Array Syntax) : PrinterM Doc := do
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.concatDocs docs
end

def formatSyntax (stx : Syntax) (config : Config.FormatterConfig) (source : String) : Doc := Id.run do
  let sourceIndentUnit := detectSourceIndentUnit stx
  let ctx : PrinterContext := { config, source, sourceIndentUnit }
  let (doc, state) := printSyntax stx |>.run ctx |>.run {}
  let tail := String.Pos.Raw.extract source ⟨state.pos⟩ source.rawEndPos
  doc ++ Doc.str tail

def formatToString (stx : Syntax) (config : Config.FormatterConfig) (source : String) : String :=
  let doc := formatSyntax stx config source
  Doc.render config.maxLineWidth doc

end Leaner.Formatter.SyntaxPrinter
