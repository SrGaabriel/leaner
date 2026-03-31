import Lean
import Leaner.Core.Types

namespace Leaner.Core.Comments

open Leaner.Core

inductive CommentKind where
  | line
  | block
  | doc
  | moduleDoc
  deriving Inhabited, BEq, Repr

instance : ToString CommentKind where
  toString
    | .line => "line"
    | .block => "block"
    | .doc => "doc"
    | .moduleDoc => "moduleDoc"

structure Comment where
  kind : CommentKind
  startPos : Nat
  endPos : Nat
  text : String
  line : Nat
  column : Nat
  deriving Inhabited, Repr

instance : ToString Comment where
  toString c := s!"{c.kind}@{c.line}:{c.column}: {c.text.take 30}..."

structure ScanState where
  pos : Nat := 0
  line : Nat := 1
  column : Nat := 1
  comments : Array Comment := #[]
  deriving Inhabited

def atEnd (source : String) (s : ScanState) : Bool :=
  s.pos >= source.utf8ByteSize

def peek (source : String) (s : ScanState) : Option Char :=
  if atEnd source s then none
  else some (source.get ⟨s.pos⟩)

def peekNext (source : String) (s : ScanState) : Option Char :=
  let nextPos := s.pos + (source.get ⟨s.pos⟩).utf8Size
  if nextPos >= source.utf8ByteSize then none
  else some (source.get ⟨nextPos⟩)

def peekAt (source : String) (s : ScanState) (offset : Nat) : Option Char := Id.run do
  let mut pos := s.pos
  for _ in [:offset] do
    if pos >= source.utf8ByteSize then return none
    pos := pos + (source.get ⟨pos⟩).utf8Size
  if pos >= source.utf8ByteSize then return none
  return some (source.get ⟨pos⟩)

def advanceChar (source : String) (s : ScanState) : ScanState :=
  if atEnd source s then s
  else
    let c := source.get ⟨s.pos⟩
    let nextPos := s.pos + c.utf8Size
    if c == '\n' then
      { s with pos := nextPos, line := s.line + 1, column := 1 }
    else
      { s with pos := nextPos, column := s.column + 1 }

def scanLineComment (source : String) (s : ScanState) : ScanState := Id.run do
  let startPos := s.pos
  let startLine := s.line
  let startCol := s.column
  let mut state := s

  state := advanceChar source state
  state := advanceChar source state
  while !atEnd source state do
    if peek source state == some '\n' then
      break
    state := advanceChar source state

  let endPos := state.pos
  let text := Substring.Raw.toString ⟨source, ⟨startPos⟩, ⟨endPos⟩⟩

  let comment : Comment := {
    kind := .line
    startPos
    endPos
    text
    line := startLine
    column := startCol
  }

  { state with comments := state.comments.push comment }

def scanBlockComment (source : String) (s : ScanState) : ScanState := Id.run do
  let startPos := s.pos
  let startLine := s.line
  let startCol := s.column
  let mut state := s

  let kind := match peekAt source state 2 with
    | some '-' => .doc
    | some '!' => .moduleDoc
    | _ => .block

  state := advanceChar source state
  state := advanceChar source state
  let mut depth : Nat := 1

  while depth > 0 && !atEnd source state do
    let c := peek source state
    let c2 := peekNext source state

    if c == some '/' && c2 == some '-' then
      depth := depth + 1
      state := advanceChar source state
      state := advanceChar source state
    else if c == some '-' && c2 == some '/' then
      depth := depth - 1
      state := advanceChar source state
      state := advanceChar source state
    else
      state := advanceChar source state

  let endPos := state.pos
  let text := Substring.Raw.toString ⟨source, ⟨startPos⟩, ⟨endPos⟩⟩

  let comment : Comment := {
    kind
    startPos
    endPos
    text
    line := startLine
    column := startCol
  }

  { state with comments := state.comments.push comment }

def scanString (source : String) (s : ScanState) : ScanState := Id.run do
  let mut state := s

  state := advanceChar source state

  while !atEnd source state do
    let c := peek source state
    if c == some '"' then
      state := advanceChar source state
      break
    else if c == some '\\' then
      state := advanceChar source state
      if !atEnd source state then
        state := advanceChar source state
    else
      state := advanceChar source state

  state

def scanRawString (source : String) (s : ScanState) : ScanState := Id.run do
  let mut state := s

  let mut hashCount : Nat := 0
  while peek source state == some '#' do
    hashCount := hashCount + 1
    state := advanceChar source state

  if peek source state == some '"' then
    state := advanceChar source state

  while !atEnd source state do
    if peek source state == some '"' then
      let savedState := state
      state := advanceChar source state
      let mut foundHashes : Nat := 0
      while foundHashes < hashCount && peek source state == some '#' do
        foundHashes := foundHashes + 1
        state := advanceChar source state
      if foundHashes == hashCount then
        break
      else
        state := savedState
        state := advanceChar source state
    else
      state := advanceChar source state

  state

def skipDocComment (source : String) (s : ScanState) : ScanState := Id.run do
  let mut state := s

  state := advanceChar source state
  state := advanceChar source state
  let mut depth : Nat := 1

  while depth > 0 && !atEnd source state do
    let c := peek source state
    let c2 := peekNext source state

    if c == some '/' && c2 == some '-' then
      depth := depth + 1
      state := advanceChar source state
      state := advanceChar source state
    else if c == some '-' && c2 == some '/' then
      depth := depth - 1
      state := advanceChar source state
      state := advanceChar source state
    else
      state := advanceChar source state

  state

def extractComments (source : String) : Array Comment := Id.run do
  let mut state : ScanState := {}

  while !atEnd source state do
    let c := peek source state
    let c2 := peekNext source state
    let c3 := peekAt source state 2

    if c == some '/' && c2 == some '-' then
      if c3 == some '-' || c3 == some '!' then
        state := skipDocComment source state
      else
        state := scanBlockComment source state
    else if c == some '-' && c2 == some '-' then
      state := scanLineComment source state
    else if c == some '"' then
      state := scanString source state
    else if c == some '#' || (c == some 'r' && c2 == some '#') then
      if c == some 'r' then
        state := advanceChar source state
      if peek source state == some '#' then
        state := scanRawString source state
      else
        state := advanceChar source state
    else
      state := advanceChar source state

  state.comments

inductive CommentPosition where
  | before
  | after
  | inside
  deriving Inhabited, BEq, Repr

structure AttachedComment where
  comment : Comment
  position : CommentPosition
  nodeStartPos : Nat
  deriving Inhabited

def commentsBefore (comments : Array Comment) (pos : Nat) : Array Comment :=
  comments.filter fun c => c.endPos < pos

def commentsAfterOnLine (comments : Array Comment) (pos : Nat) (line : Nat) : Array Comment :=
  comments.filter fun c => c.startPos > pos && c.line == line

def commentsBetween (comments : Array Comment) (start stop : Nat) : Array Comment :=
  comments.filter fun c => c.startPos >= start && c.endPos <= stop

end Leaner.Core.Comments
