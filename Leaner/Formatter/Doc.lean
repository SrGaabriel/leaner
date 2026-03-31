namespace Leaner.Formatter.Doc

inductive Doc where
  | nil
  | text (s : String)
  | line
  | softline
  | hardline
  | concat (a b : Doc)
  | nest (indent : Nat) (doc : Doc)
  | group (doc : Doc)
  | ifFlat (flat broken : Doc)
  | align (doc : Doc)
  | fill (docs : Array Doc)
  deriving Inhabited

namespace Doc

def empty : Doc := .nil

def str (s : String) : Doc := .text s

def space : Doc := .text " "

def lineDoc : Doc := .line

def softlineDoc : Doc := .softline

def hardlineDoc : Doc := .hardline

def append (a b : Doc) : Doc :=
  match a, b with
  | .nil, d => d
  | d, .nil => d
  | _, _ => .concat a b

instance : Append Doc where
  append := append

instance : HAppend Doc Doc Doc where
  hAppend := append

def sepBy (a b : Doc) : Doc := a ++ space ++ b

def lineBreak (a b : Doc) : Doc := a ++ lineDoc ++ b

def mkNest (n : Nat) (d : Doc) : Doc := .nest n d

def mkGroup (d : Doc) : Doc := .group d

def mkAlign (d : Doc) : Doc := .align d

def indent (n : Nat) (d : Doc) : Doc := mkNest n (mkAlign d)

def hang (n : Nat) (d : Doc) : Doc := mkAlign (mkNest n d)

def concatDocs (docs : Array Doc) : Doc :=
  docs.foldl (· ++ ·) .nil

def join (sep : Doc) (docs : Array Doc) : Doc := Id.run do
  if docs.isEmpty then return .nil
  let mut result := docs[0]!
  for i in [1:docs.size] do
    result := result ++ sep ++ docs[i]!
  result

def hsep (docs : Array Doc) : Doc := join space docs

def vsep (docs : Array Doc) : Doc := join lineDoc docs

def fillSep (docs : Array Doc) : Doc := join softlineDoc docs

def vcat (docs : Array Doc) : Doc := join hardlineDoc docs

def bracket (open_ : String) (close : String) (doc : Doc) : Doc :=
  mkGroup (str open_ ++ mkNest 2 (softlineDoc ++ doc) ++ softlineDoc ++ str close)

def parens (doc : Doc) : Doc := bracket "(" ")" doc

def brackets (doc : Doc) : Doc := bracket "[" "]" doc

def braces (doc : Doc) : Doc := bracket "{" "}" doc

def angles (doc : Doc) : Doc := bracket "⟨" "⟩" doc

def enclose (left right : String) (doc : Doc) : Doc :=
  str left ++ doc ++ str right

inductive Mode where
  | flat
  | break_
  deriving Inhabited, BEq

structure RenderState where
  column : Nat := 0
  indent : Nat := 0
  output : String := ""
  deriving Inhabited

structure StackItem where
  indent : Nat
  mode : Mode
  doc : Doc
  deriving Inhabited

partial def fits (width : Nat) (column : Nat) (stack : List StackItem) : Bool := Id.run do
  if column > width then return false
  match stack with
  | [] => true
  | ⟨i, m, d⟩ :: rest =>
    match d with
    | .nil => fits width column rest
    | .text s => fits width (column + s.length) rest
    | .line =>
      match m with
      | .flat => fits width (column + 1) rest
      | .break_ => true
    | .softline =>
      match m with
      | .flat => fits width column rest
      | .break_ => true
    | .hardline => true
    | .concat a b => fits width column (⟨i, m, a⟩ :: ⟨i, m, b⟩ :: rest)
    | .nest n doc => fits width column (⟨i + n, m, doc⟩ :: rest)
    | .group doc => fits width column (⟨i, .flat, doc⟩ :: rest)
    | .ifFlat flat _ =>
      match m with
      | .flat => fits width column (⟨i, m, flat⟩ :: rest)
      | .break_ => fits width column (⟨i, m, flat⟩ :: rest)
    | .align doc => fits width column (⟨column, m, doc⟩ :: rest)
    | .fill docs =>
      if docs.isEmpty then fits width column rest
      else fits width column (⟨i, m, docs[0]!⟩ :: rest)

partial def render (width : Nat) (doc : Doc) : String := Id.run do
  let mut state : RenderState := {}
  let mut stack : List StackItem := [⟨0, .break_, doc⟩]

  while true do
    match stack with
    | [] => break
    | ⟨i, m, d⟩ :: rest =>
      stack := rest
      match d with
      | .nil => pure ()
      | .text s =>
        state := { state with
          output := state.output ++ s
          column := state.column + s.length }
      | .line =>
        match m with
        | .flat =>
          state := { state with
            output := state.output ++ " "
            column := state.column + 1 }
        | .break_ =>
          state := { state with
            output := state.output ++ "\n" ++ "".pushn ' ' i
            column := i }
      | .softline =>
        match m with
        | .flat => pure ()
        | .break_ =>
          state := { state with
            output := state.output ++ "\n" ++ "".pushn ' ' i
            column := i }
      | .hardline =>
        state := { state with
          output := state.output ++ "\n" ++ "".pushn ' ' i
          column := i }
      | .concat a b =>
        stack := ⟨i, m, a⟩ :: ⟨i, m, b⟩ :: stack
      | .nest n doc =>
        stack := ⟨i + n, m, doc⟩ :: stack
      | .group doc =>
        if fits width state.column (⟨i, .flat, doc⟩ :: stack) then
          stack := ⟨i, .flat, doc⟩ :: stack
        else
          stack := ⟨i, .break_, doc⟩ :: stack
      | .ifFlat flat broken =>
        match m with
        | .flat => stack := ⟨i, m, flat⟩ :: stack
        | .break_ => stack := ⟨i, m, broken⟩ :: stack
      | .align doc =>
        stack := ⟨state.column, m, doc⟩ :: stack
      | .fill docs =>
        if docs.isEmpty then pure ()
        else
          for doc in docs do
            stack := ⟨i, m, doc⟩ :: ⟨i, m, softlineDoc⟩ :: stack

  state.output

end Doc

end Leaner.Formatter.Doc
