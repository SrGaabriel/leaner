import Lean
import Leaner.Core.Types

namespace Leaner.Core.Parser

open Lean
open Lean.Parser
open Lean.Elab
open Leaner.Core

structure ParseOptions where
  fileName : String := "<input>"
  resolveImports : Bool := false
  deriving Inhabited

inductive ParseResult where
  | ok (stx : Syntax) (parserState : ModuleParserState) (env : Environment)
  | error (messages : MessageLog)
  deriving Inhabited

def mkInputContext (content : String) (fileName : String) : InputContext :=
  let fileMap := FileMap.ofString content
  InputContext.mk' content fileName fileMap content.rawEndPos

def initParseEnv : IO Environment := do
  Lean.initSearchPath (← Lean.findSysroot)
  let env ← importModules #[{ module := `Init }] {}
  return env

private def parseAllCommands (inputCtx : InputContext) (pmctx : ParserModuleContext)
    (state : ModuleParserState) (endPos : String.Pos.Raw)
    : Syntax × ModuleParserState × MessageLog := Id.run do
  let mut cmds : Array Syntax := #[]
  let mut state := state
  let mut messages := MessageLog.empty

  while state.pos < endPos do
    let prevPos := state.pos
    let (cmdStx, state', msgs) := parseCommand inputCtx pmctx state messages
    messages := msgs

    if cmdStx.isMissing then
      break

    cmds := cmds.push cmdStx
    state := state'

    if state.pos == prevPos then
      break

  (Syntax.node .none `Lean.Parser.Module.commands cmds, state, messages)

def parseModule (content : String) (opts : ParseOptions := {}) : IO ParseResult := do
  let env ← initParseEnv
  let inputCtx := mkInputContext content opts.fileName

  let (header, parserState, messages) ← parseHeader inputCtx
  let pmctx : ParserModuleContext := {
    env := env
    options := {}
    currNamespace := Name.anonymous
    openDecls := []
  }

  let (cmds, finalState, cmdMessages) := parseAllCommands inputCtx pmctx parserState content.rawEndPos

  let moduleSyntax := (Syntax.node .none `module #[header, cmds]).updateLeading
  let allMessages := messages ++ cmdMessages
  if allMessages.hasErrors && opts.resolveImports then
    return .error allMessages

  return .ok moduleSyntax finalState env

def parseContent (content : String) (opts : ParseOptions := {}) : IO ParseResult := do
  parseModule content opts

def parseFile (path : System.FilePath) : IO ParseResult := do
  let content ← IO.FS.readFile path
  parseContent content { fileName := path.toString }

def syntaxRange (source : String) (stx : Syntax) : Option Range := do
  let head ← stx.getHeadInfo?.bind (·.getPos?)
  let tail ← stx.getTailInfo?.bind (·.getTailPos?)
  some {
    start := Position.fromRawPos source head
    stop := Position.fromRawPos source tail
  }

def syntaxText (source : String) (stx : Syntax) : Option String := do
  let head ← stx.getHeadInfo?.bind (·.getPos?)
  let tail ← stx.getTailInfo?.bind (·.getTailPos?)
  some (Substring.Raw.toString ⟨source, head, tail⟩)

def leadingTrivia (stx : Syntax) : Option Substring.Raw := do
  match stx.getHeadInfo? with
  | some (.original leading _ _ _) => some leading
  | _ => none

def trailingTrivia (stx : Syntax) : Option Substring.Raw := do
  match stx.getTailInfo? with
  | some (.original _ _ trailing _) => some trailing
  | _ => none

def isAtom (stx : Syntax) : Bool :=
  match stx with
  | .atom _ _ => true
  | _ => false

def isNode (stx : Syntax) : Bool :=
  match stx with
  | .node _ _ _ => true
  | _ => false

def kindName (stx : Syntax) : String :=
  stx.getKind.toString

end Leaner.Core.Parser
