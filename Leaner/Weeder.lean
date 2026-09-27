import Lean
import Leaner.Core
import Leaner.Weeder.Analyzer

namespace Leaner.Weeder

open Lean
open Leaner.Core

structure DeadDecl where
  name : Name
  module : Name
  source : System.FilePath
  range : Leaner.Core.Range
  deriving Inhabited

instance : ToString DeadDecl where
  toString d := s!"{d.source}:{d.range.start}: warning [weed]: unused declaration `{d.name}`"

structure WeedResult where
  modules : Array ProjectModule := #[]
  totalConsts : Nat := 0
  candidates : Nat := 0
  reachableCandidates : Nat := 0
  dead : Array DeadDecl := #[]
  env? : Option Environment := none
  graph? : Option UsesGraph := none
  reached? : Option NameSet := none
  deriving Inhabited

private def buildModuleSourceMap (modules : Array ProjectModule) : Std.HashMap Name System.FilePath :=
  modules.foldl (init := {}) fun m pm => m.insert pm.name pm.source

private def fallbackRange : Leaner.Core.Range :=
  { start := { line := 1, column := 1 }, stop := { line := 1, column := 1 } }

def runWeederMulti (roots : Array System.FilePath) (cfg : RootConfig := {}) : IO WeedResult := do
  initWeederSearchPath
  let allModules ← discoverModulesMulti roots
  let modules ← filterImportable allModules
  if modules.isEmpty then
    return {}
  let modSet := projectModuleSet modules
  let env ← loadProjectEnv (modules.map (·.name))
  let consts := collectProjectConstants env modSet
  let candidates := consts.filter (isCandidate env cfg ·)
  let candidateSet : NameSet :=
    candidates.foldl (init := ({} : NameSet)) fun s e => s.insert e.name
  let g := augmentGraphWithAttrs env (buildUsesGraph consts)
  let rs := buildRootSets env modSet cfg
  let openRefs ← collectOpenReferences env modules modSet
  let baseRoots := collectRoots env cfg rs consts candidateSet
  let roots :=
    openRefs.foldl (init := baseRoots) fun s n => s.insert n
  let reached := reachableFromRoots g roots
  let dead := findDeadCandidates env cfg consts reached
  let modSrc := buildModuleSourceMap modules
  let dead := dead.filterMap fun e => do
    let source := modSrc[e.module]?
    match source with
    | none => none
    | some source =>
      let range := (rangeOf? env e.name).getD fallbackRange
      some { name := e.name, module := e.module, source, range : DeadDecl }
  let dead := dead.qsort fun a b =>
    if a.source.toString == b.source.toString then
      compare a.range.start b.range.start == .lt
    else
      a.source.toString < b.source.toString
  let reachedCandidates := candidates.filter (fun e => reached.contains e.name) |>.size
  return {
    modules
    totalConsts := consts.size
    candidates := candidates.size
    reachableCandidates := reachedCandidates
    dead
    env? := some env
    graph? := some g
    reached? := some reached
  }


def WeedResult.summary (r : WeedResult) : String :=
  let nMod := r.modules.size
  let n := r.candidates
  let live := r.reachableCandidates
  let nDead := r.dead.size
  s!"scanned {nMod} modules, {n} candidates ({live} live, {nDead} dead)"

def WeedResult.formatDead (r : WeedResult) : String :=
  String.intercalate "\n" (r.dead.toList.map toString)

private def declNameOfCommand (cmd : Syntax) : Option Name := Id.run do
  let declIdNode? :=
    Visitor.findWhere "" cmd (·.isOfKind `Lean.Parser.Command.declId)
  match declIdNode? with
  | some node =>
    let identNode? := Visitor.findWhere "" node (·.isIdent)
    identNode?.map (·.getId)
  | none =>
    none

private def commandNamespaceIdent? (cmd : Syntax) : Option Name :=
  match Visitor.findWhere "" cmd (·.isIdent) with
  | some node => some node.getId
  | none => none

private def commandSpan? (cmd : Syntax) : Option (String.Pos.Raw × String.Pos.Raw) := do
  let head ← cmd.getHeadInfo?.bind (·.getPos?)
  let tail ← cmd.getTailInfo?.bind (·.getTailPos?)
  some (head, tail)

private partial def collectInnerDeclNames (stx : Syntax) : Array Name := Id.run do
  let mut acc : Array Name := #[]
  match stx with
  | .node _ kind args =>
    if kind == `Lean.Parser.Command.declaration then
      if let some n := declNameOfCommand stx then
        acc := acc.push n
    else
      for a in args do
        acc := acc ++ collectInnerDeclNames a
  | _ => pure ()
  return acc

private def collectFileDeclSpans (moduleStx : Syntax)
    : Array (Name × String.Pos.Raw × String.Pos.Raw) := Id.run do
  let mut spans : Array (Name × String.Pos.Raw × String.Pos.Raw) := #[]
  let mut nsStack : Array Name := #[]
  let commands := moduleStx.getArg 1
  let cmds :=
    match commands with
    | .node _ _ args => args
    | _ => #[]
  for cmd in cmds do
    let kind := cmd.getKind
    if kind == `Lean.Parser.Command.namespace then
      if let some n := commandNamespaceIdent? cmd then
        nsStack := nsStack.push n
    else if kind == `Lean.Parser.Command.end then
      match commandNamespaceIdent? cmd with
      | none => pure ()
      | some endN =>
        if !nsStack.isEmpty && nsStack.back! == endN then
          nsStack := nsStack.pop
    else if kind == `Lean.Parser.Command.declaration then
      if let some bareName := declNameOfCommand cmd then
        if let some (s, e) := commandSpan? cmd then
          let prefixName := nsStack.foldl (init := Name.anonymous) Name.append
          spans := spans.push (prefixName ++ bareName, s, e)
    else if kind == `Lean.Parser.Command.mutual then
      if let some (s, e) := commandSpan? cmd then
        let prefixName := nsStack.foldl (init := Name.anonymous) Name.append
        for bareName in collectInnerDeclNames cmd do
          spans := spans.push (prefixName ++ bareName, s, e)
  return spans

private def fileDeclMap (env : Environment) (path : System.FilePath)
    : IO (String × Std.HashMap Name (String.Pos.Raw × String.Pos.Raw)) := do
  let raw ← IO.FS.readFile path
  let source := raw.replace "\r\n" "\n"
  let parseResult ← Parser.parseModule source env { fileName := path.toString, resolveImports := false }
  let stx :=
    match parseResult with
    | .ok stx _ _ _ => stx
    | .error _ => Syntax.missing
  let spans := collectFileDeclSpans stx
  let map : Std.HashMap Name (String.Pos.Raw × String.Pos.Raw) :=
    spans.foldl (init := {}) fun m (n, s, e) => m.insert n (s, e)
  return (source, map)

private partial def forwardClosure (g : UsesGraph) (seeds : NameSet) : NameSet := Id.run do
  let mut reached : NameSet := seeds
  let mut work : Array Name := seeds.toArray
  while !work.isEmpty do
    let n := work.back!
    work := work.pop
    let succs := g[n]?.getD ({} : NameSet)
    for s in succs do
      unless reached.contains s do
        reached := reached.insert s
        work := work.push s
  return reached

private partial def computeKeepSet (g : UsesGraph) (reached : NameSet)
    (perFileSpans : Std.HashMap String (Std.HashMap (Nat × Nat) (Array Name)))
    : NameSet := Id.run do
  let mut keep := forwardClosure g reached
  let mut changed := true
  while changed do
    changed := false
    for (_, spanMembers) in perFileSpans.toArray do
      for (_, members) in spanMembers.toArray do
        if members.size > 1 then
          let anyKept := members.any keep.contains
          if anyKept then
            for m in members do
              unless keep.contains m do
                keep := keep.insert m
                changed := true
      if changed then
        keep := forwardClosure g keep
  return keep

def applyDeletions (env : Environment) (g : UsesGraph) (reached : NameSet)
    (dead : Array DeadDecl) : IO (Nat × Array DeadDecl) := do
  let mut byFile : Std.HashMap String (Array DeadDecl) := {}
  for d in dead do
    let key := d.source.toString
    let prev := byFile[key]?.getD #[]
    byFile := byFile.insert key (prev.push d)
  let mut perFileSource : Std.HashMap String String := {}
  let mut perFileDeclMap : Std.HashMap String (Std.HashMap Name (String.Pos.Raw × String.Pos.Raw)) := {}
  let mut perFileSpans : Std.HashMap String (Std.HashMap (Nat × Nat) (Array Name)) := {}
  for path in byFile.toArray.map (·.1) do
    let (source, declMap) ← fileDeclMap env (System.FilePath.mk path)
    perFileSource := perFileSource.insert path source
    perFileDeclMap := perFileDeclMap.insert path declMap
    let mut spanMembers : Std.HashMap (Nat × Nat) (Array Name) := {}
    for (n, sp) in declMap.toArray do
      let key := (sp.1.byteIdx, sp.2.byteIdx)
      spanMembers := spanMembers.insert key ((spanMembers[key]?.getD #[]).push n)
    perFileSpans := perFileSpans.insert path spanMembers
  let keep := computeKeepSet g reached perFileSpans
  let mut filesChanged := 0
  let mut skipped : Array DeadDecl := #[]
  for (path, decls) in byFile.toArray do
    let source := perFileSource[path]!
    let declMap := perFileDeclMap[path]!
    let mut spansToDelete : Array (String.Pos.Raw × String.Pos.Raw) := #[]
    let mut spansSeen : Std.HashSet (Nat × Nat) := {}
    for d in decls do
      if keep.contains d.name then
        skipped := skipped.push d
      else
        match declMap[d.name]? with
        | none => skipped := skipped.push d
        | some (s, e) =>
          let key := (s.byteIdx, e.byteIdx)
          unless spansSeen.contains key do
            spansSeen := spansSeen.insert key
            spansToDelete := spansToDelete.push (s, e)
    if spansToDelete.isEmpty then continue
    let sortedSpans := spansToDelete.qsort fun a b => a.1.byteIdx > b.1.byteIdx
    let mut text := source
    for (startPos, endPos) in sortedSpans do
      let endByte := text.utf8ByteSize
      let actualEnd :=
        if endPos.byteIdx < endByte && String.Pos.Raw.get text endPos == '\n' then
          ⟨endPos.byteIdx + 1⟩
        else
          endPos
      let head := Substring.Raw.toString ⟨text, ⟨0⟩, startPos⟩
      let tail := Substring.Raw.toString ⟨text, actualEnd, text.rawEndPos⟩
      text := head ++ tail
      if text != source then
        IO.FS.writeFile path text
        filesChanged := filesChanged + 1
  return (filesChanged, skipped)

end Leaner.Weeder
