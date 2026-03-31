import Lean
import Leaner.Core.Types

namespace Leaner.Core.Visitor

open Lean

inductive VisitResult where
  | continue
  | skip
  | stop
  deriving Inhabited, BEq

structure VisitContext where
  source : String
  depth : Nat := 0
  parents : Array Syntax := #[]
  siblingIndex : Nat := 0
  siblingCount : Nat := 1
  deriving Inhabited

namespace VisitContext

def pushParent (ctx : VisitContext) (stx : Syntax) : VisitContext :=
  { ctx with
    parents := ctx.parents.push stx
    depth := ctx.depth + 1 }

def parent? (ctx : VisitContext) : Option Syntax :=
  ctx.parents.back?

def isRoot (ctx : VisitContext) : Bool :=
  ctx.parents.isEmpty

def isFirstSibling (ctx : VisitContext) : Bool :=
  ctx.siblingIndex == 0

def isLastSibling (ctx : VisitContext) : Bool :=
  ctx.siblingIndex + 1 == ctx.siblingCount

end VisitContext

structure Visitor (σ : Type) where
  pre : VisitContext → Syntax → σ → (VisitResult × σ) := fun _ _ s => (.continue, s)
  post : VisitContext → Syntax → σ → σ := fun _ _ s => s
  deriving Inhabited

partial def visit [Inhabited σ] (v : Visitor σ) (ctx : VisitContext) (stx : Syntax) (state : σ) : σ := Id.run do
  let (result, state) := v.pre ctx stx state
  match result with
  | .stop => return state
  | .skip => return v.post ctx stx state
  | .continue =>
    let mut state := state
    match stx with
    | .node _ _ args =>
      let childCtx := ctx.pushParent stx
      for i in [:args.size] do
        let child := args[i]!
        let ctx' := { childCtx with siblingIndex := i, siblingCount := args.size }
        state := visit v ctx' child state
    | _ => pure ()
    return v.post ctx stx state

def visitRoot [Inhabited σ] (v : Visitor σ) (source : String) (stx : Syntax) (initial : σ) : σ :=
  visit v { source } stx initial

def collectWhere (source : String) (stx : Syntax) (pred : Syntax → Bool) : Array Syntax :=
  let v : Visitor (Array Syntax) := {
    pre := fun _ node acc =>
      if pred node then (.continue, acc.push node)
      else (.continue, acc)
  }
  visitRoot v source stx #[]

def collectKind (source : String) (stx : Syntax) (kind : SyntaxNodeKind) : Array Syntax :=
  collectWhere source stx (·.isOfKind kind)

def findWhere (source : String) (stx : Syntax) (pred : Syntax → Bool) : Option Syntax := Id.run do
  let v : Visitor (Option Syntax) := {
    pre := fun _ node acc =>
      if acc.isSome then (.stop, acc)
      else if pred node then (.stop, some node)
      else (.continue, none)
  }
  visitRoot v source stx none

def mapSyntax (f : Syntax → Syntax) (stx : Syntax) : Syntax :=
  match stx with
  | .node info kind args =>
    let args' := args.map (mapSyntax f)
    f (.node info kind args')
  | other => f other

def foldSyntax [Inhabited σ] (f : σ → Syntax → σ) (init : σ) (stx : Syntax) : σ := Id.run do
  let v : Visitor σ := {
    pre := fun _ node acc => (.continue, f acc node)
  }
  visitRoot v "" stx init

def getDeclarations (stx : Syntax) : Array Syntax :=
  collectWhere "" stx fun node =>
    node.isOfKind `Lean.Parser.Command.declaration ||
    node.isOfKind `Lean.Parser.Command.def ||
    node.isOfKind `Lean.Parser.Command.theorem ||
    node.isOfKind `Lean.Parser.Command.structure ||
    node.isOfKind `Lean.Parser.Command.inductive

def getDefs (stx : Syntax) : Array Syntax :=
  collectWhere "" stx (·.isOfKind `Lean.Parser.Command.def)

def getIdents (stx : Syntax) : Array Syntax :=
  collectWhere "" stx (·.isIdent)

def getAtoms (stx : Syntax) : Array Syntax :=
  collectWhere "" stx fun node =>
    match node with
    | .atom _ _ => true
    | _ => false

end Leaner.Core.Visitor
