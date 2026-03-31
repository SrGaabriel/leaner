import Lean
import Leaner.Core
import Leaner.Formatter.Doc

namespace Leaner.Formatter.SyntaxPrinter

open Lean
open Leaner.Core
open Leaner.Core.Comments
open Leaner.Formatter.Doc

structure PrinterContext where
  config : Config.FormatterConfig := {}
  source : String
  comments : Array Comment
  deriving Inhabited

structure PrinterState where
  pos : Nat := 0
  emittedComments : Array Nat := #[]
  indentLevel : Nat := 0
  deriving Inhabited

abbrev PrinterM := ReaderT PrinterContext (StateM PrinterState)

def getConfig : PrinterM Config.FormatterConfig := do
  return (← read).config

def getSource : PrinterM String := do
  return (← read).source

def getComments : PrinterM (Array Comment) := do
  return (← read).comments

def getPos : PrinterM Nat := do
  return (← get).pos

def setPos (pos : Nat) : PrinterM Unit := do
  modify fun s => { s with pos }

def getIndent : PrinterM Nat := do
  return (← get).indentLevel

def withIndent (n : Nat) (m : PrinterM α) : PrinterM α := do
  let old ← getIndent
  modify fun s => { s with indentLevel := old + n }
  let result ← m
  modify fun s => { s with indentLevel := old }
  return result

def isEmitted (idx : Nat) : PrinterM Bool := do
  return (← get).emittedComments.contains idx

def markEmitted (idx : Nat) : PrinterM Unit := do
  modify fun s => { s with emittedComments := s.emittedComments.push idx }

def hasBeenEmitted (i : Nat) : PrinterM Bool := do
  return (← get).emittedComments.contains i

def getCommentsBefore (pos : Nat) : PrinterM (Array Comment) := do
  let comments ← getComments
  let mut result := #[]
  for i in [:comments.size] do
    let c := comments[i]!
    let alreadyEmitted ← hasBeenEmitted i
    if c.endPos <= pos && !alreadyEmitted then
      result := result.push c
      markEmitted i
  return result

def getTrailingComment (pos : Nat) (line : Nat) : PrinterM (Option Comment) := do
  let comments ← getComments
  for i in [:comments.size] do
    let c := comments[i]!
    let alreadyEmitted ← hasBeenEmitted i
    if c.startPos > pos && c.line == line && c.kind == .line && !alreadyEmitted then
      markEmitted i
      return some c
  return none

def commentToDoc (c : Comment) : Doc :=
  Doc.str c.text

def emitLeadingComments (pos : Nat) : PrinterM Doc := do
  let comments ← getCommentsBefore pos
  if comments.isEmpty then
    return Doc.empty
  let mut doc := Doc.empty
  for c in comments do
    doc := doc ++ commentToDoc c ++ Doc.hardlineDoc
  return doc

def emitTrailingComment (pos : Nat) (line : Nat) : PrinterM Doc := do
  if let some c ← getTrailingComment pos line then
    return Doc.str " " ++ commentToDoc c
  return Doc.empty

def countNewlines (s : String) : Nat :=
  s.toList.filter (· == '\n') |>.length

def limitNewlines (n : Nat) (maxNewlines : Nat) : Nat :=
  min n maxNewlines

def stripBlockCommentsFromTrivia (trivia : String) : String := Id.run do
  return trivia

def printAtom (info : SourceInfo) (val : String) : PrinterM Doc := do
  let cfg ← getConfig

  match info with
  | .original l pos t _ =>
    let startPos := pos.byteIdx
    let leading := l.toString
    let trailing := t.toString

    let leadingComments ← emitLeadingComments startPos

    let mut doc := leadingComments

    if leading.any (· == '\n') then
      let newlines := countNewlines leading
      let limited := limitNewlines newlines (cfg.maxBlankLines + 1)
      for _ in [:limited] do
        doc := doc ++ Doc.str "\n"
      let parts := leading.splitOn "\n"
      if let some indent := parts.getLast? then
        if !indent.isEmpty then
          doc := doc ++ Doc.str indent
    else if !leading.isEmpty then
      doc := doc ++ Doc.str leading

    doc := doc ++ Doc.str val

    if trailing.any (· == '\n') then
      let newlines := countNewlines trailing
      let limited := limitNewlines newlines (cfg.maxBlankLines + 1)
      for _ in [:limited] do
        doc := doc ++ Doc.hardlineDoc
    else if !trailing.isEmpty then
      doc := doc ++ Doc.str trailing

    return doc

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
      if !args.isEmpty then
        printSyntax args[0]!
      else
        return Doc.empty
    else if kind == `Lean.Parser.Command.declaration then
      printChildren args
    else if kind == `Lean.Parser.Command.def then
      printDef args
    else if kind == `Lean.Parser.Command.theorem then
      printChildren args
    else if kind == `Lean.Parser.Command.structure then
      printStructure args
    else if kind == `Lean.Parser.Command.namespace then
      printChildren args
    else if kind == `Lean.Parser.Command.section then
      printChildren args
    else if kind == `Lean.Parser.Command.open then
      printChildren args
    else if kind == `Lean.Parser.Command.variable then
      printChildren args
    else if kind == `Lean.Parser.Term.fun then
      printFun args
    else if kind == `Lean.Parser.Term.match then
      printMatch args
    else if kind == `Lean.Parser.Term.do then
      printChildren args
    else if kind == `Lean.Parser.Term.if then
      printIf args
    else if kind == `Lean.Parser.Term.let then
      printChildren args
    else if kind == `Lean.Parser.Term.where then
      printWhere args
    else
      printChildren args

  partial def printChildren (args : Array Syntax) : PrinterM Doc := do
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.concatDocs docs

  partial def printDef (args : Array Syntax) : PrinterM Doc := do
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.mkGroup (Doc.concatDocs docs)

  partial def printStructure (args : Array Syntax) : PrinterM Doc := do
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.mkGroup (Doc.concatDocs docs)

  partial def printFun (args : Array Syntax) : PrinterM Doc := do
    let cfg ← getConfig
    let indentSize := match cfg.indentStyle with
      | .spaces n => n
      | .tabs => 2
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.mkGroup (Doc.mkNest indentSize (Doc.concatDocs docs))

  partial def printMatch (args : Array Syntax) : PrinterM Doc := do
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.mkGroup (Doc.concatDocs docs)

  partial def printIf (args : Array Syntax) : PrinterM Doc := do
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.mkGroup (Doc.concatDocs docs)

  partial def printWhere (args : Array Syntax) : PrinterM Doc := do
    let cfg ← getConfig
    let indentSize := match cfg.indentStyle with
      | .spaces n => n
      | .tabs => 2
    let mut docs : Array Doc := #[]
    for arg in args do
      let doc ← printSyntax arg
      docs := docs.push doc
    return Doc.mkNest indentSize (Doc.concatDocs docs)
end

def emitRemainingComments : PrinterM Doc := do
  let comments ← getComments
  let emitted := (← get).emittedComments
  let mut doc := Doc.empty
  for i in [:comments.size] do
    if !emitted.contains i then
      let c := comments[i]!
      doc := doc ++ commentToDoc c ++ Doc.hardlineDoc
  return doc

def formatSyntax (stx : Syntax) (config : Config.FormatterConfig) (source : String) : Doc := Id.run do
  let comments := extractComments source

  let ctx : PrinterContext := { config, source, comments }
  let (doc, state) := printSyntax stx |>.run ctx |>.run {}

  let (remainingDoc, _) := emitRemainingComments |>.run ctx |>.run state

  doc ++ remainingDoc

def formatToString (stx : Syntax) (config : Config.FormatterConfig) (source : String) : String :=
  let doc := formatSyntax stx config source
  Doc.render config.maxLineWidth doc

end Leaner.Formatter.SyntaxPrinter
