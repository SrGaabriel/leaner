import Lean
import Leaner.Core

namespace Leaner.Weeder

open Lean
open Leaner.Core

structure ProjectModule where
  name : Name
  source : System.FilePath
  deriving Inhabited

private def normalizeSep (s : String) : String :=
  s.replace "\\" "/"

private def stripTrailingSep (s : String) : String :=
  if s.endsWith "/" then (s.dropEnd 1).toString else s

private def splitPath (s : String) : List String :=
  s.splitOn "/" |>.filter (· != "")

private def relPathToModule (relPath : String) : Option Name := do
  let p := normalizeSep relPath
  guard (p.endsWith ".lean")
  let stem := (p.dropEnd ".lean".length).toString
  let parts := splitPath stem
  guard (!parts.isEmpty)
  guard (parts.all fun part => !part.isEmpty && (part.front.isAlpha || part.front == '_'))
  some <| parts.foldl (fun acc s => Name.str acc s) Name.anonymous

private def absRel (root : System.FilePath) (path : System.FilePath) : Option String := do
  let r := stripTrailingSep (normalizeSep root.toString)
  let p := normalizeSep path.toString
  if p == r then none
  else if p.startsWith (r ++ "/") then some (p.drop (r.length + 1)).toString
  else none

private def skipSubdir (name : String) : Bool :=
  name == ".lake" || name == "build" || name == ".git"

partial def discoverModules (root : System.FilePath) : IO (Array ProjectModule) := do
  walk root #[]
where
  walk (path : System.FilePath) (acc : Array ProjectModule) : IO (Array ProjectModule) := do
    if ← path.isDir then
      let mut acc := acc
      for entry in ← path.readDir do
        if skipSubdir entry.fileName then continue
        acc ← walk entry.path acc
      return acc
    if path.extension != some "lean" then
      return acc
    match absRel root path with
    | none => return acc
    | some rel =>
      match relPathToModule rel with
      | none => return acc
      | some mod => return acc.push { name := mod, source := path }

def discoverModulesMulti (roots : Array System.FilePath) : IO (Array ProjectModule) := do
  let mut seen : NameSet := {}
  let mut acc : Array ProjectModule := #[]
  for root in roots do
    let mods ← discoverModules root
    for m in mods do
      unless seen.contains m.name do
        seen := seen.insert m.name
        acc := acc.push m
  return acc

def filterImportable (modules : Array ProjectModule) : IO (Array ProjectModule) :=
  modules.filterM fun m => do
    try
      let path ← Lean.findOLean m.name
      path.pathExists
    catch _ =>
      return false

def initWeederSearchPath : IO Unit := do
  Lean.initSearchPath (← Lean.findSysroot)
  unsafe Lean.enableInitializersExecution

def loadProjectEnv (modules : Array Name) : IO Environment := do
  let imports := modules.map fun m => ({ module := m } : Import)
  importModules imports {} (loadExts := true)

structure ConstantEntry where
  name : Name
  info : ConstantInfo
  module : Name
  deriving Inhabited

def projectModuleSet (modules : Array ProjectModule) : NameSet :=
  modules.foldl (init := ({} : NameSet)) fun s m => s.insert m.name

def collectProjectConstants (env : Environment) (projectMods : NameSet)
    : Array ConstantEntry :=
  env.constants.fold (init := #[]) fun acc name info =>
    match env.getModuleIdxFor? name with
    | none => acc
    | some midx =>
      let modName := env.allImportedModuleNames[midx.toNat]!
      if projectMods.contains modName then
        acc.push { name, info, module := modName }
      else
        acc

abbrev UsesGraph := Std.HashMap Name NameSet

private def mutualSiblings : ConstantInfo → List Name
  | .defnInfo v => v.all
  | .thmInfo v => v.all
  | .opaqueInfo v => v.all
  | .inductInfo v => v.all
  | _ => []

def buildUsesGraph (consts : Array ConstantEntry) : UsesGraph := Id.run do
  let mut g : UsesGraph := Std.HashMap.emptyWithCapacity consts.size
  for e in consts do
    let direct := e.info.getUsedConstantsAsSet
    let withSiblings :=
      mutualSiblings e.info |>.foldl (init := direct) fun acc s =>
        if s == e.name then acc else acc.insert s
    g := g.insert e.name withSiblings
  return g

structure RootConfig where
  entryPoints : Array Name := #[]
  includeMain : Bool := true
  includeExported : Bool := true
  includeExtern : Bool := true
  includeInit : Bool := true
  includeInstances : Bool := true
  includeSimp : Bool := true
  includeExt : Bool := true
  flagTheorems : Bool := false
  flagAxioms : Bool := false
  deriving Inhabited

private def simpRootSet (env : Environment) : NameSet :=
  let s := Lean.Meta.simpExtension.getState env
  s.lemmaNames.fold (init := ({} : NameSet)) fun acc origin =>
    match origin with
    | .decl declName _ _ => acc.insert declName
    | .other declName => acc.insert declName
    | _ => acc

private def extRootSet (env : Environment) : NameSet :=
  let s := Lean.Meta.Ext.extExtension.getState env
  s.tree.values.foldl (init := ({} : NameSet)) fun acc thm =>
    if s.erased.contains thm.declName then acc else acc.insert thm.declName

private def instanceRootSet (env : Environment) : NameSet :=
  let s := Lean.Meta.instanceExtension.getState env
  s.instanceNames.foldl (init := ({} : NameSet)) fun acc n _ => acc.insert n

private def aliasRootSet (env : Environment) (projectMods : NameSet) : NameSet :=
  let s := Lean.aliasExtension.getState env
  s.fold (init := ({} : NameSet)) fun acc _ targets =>
    targets.foldl (init := acc) fun acc t =>
      match env.getModuleIdxFor? t with
      | some midx =>
        let modName := env.allImportedModuleNames[midx.toNat]!
        if projectMods.contains modName then acc.insert t else acc
      | none => acc

private partial def collectIdents (stx : Syntax) : Array Name := Id.run do
  let mut acc : Array Name := #[]
  match stx with
  | .ident _ _ n _ => acc := acc.push n
  | .node _ _ args =>
    for a in args do
      acc := acc ++ collectIdents a
  | _ => pure ()
  return acc

def openReferencesInModule (moduleStx : Syntax) : Array Name := Id.run do
  let mut acc : Array Name := #[]
  let cmds := moduleStx.getArg 1
  let cmdArr :=
    match cmds with
    | .node _ _ args => args
    | _ => #[]
  for cmd in cmdArr do
    if cmd.getKind != `Lean.Parser.Command.open then continue
    for child in cmd.getArgs do
      match child.getKind with
      | `Lean.Parser.Command.openOnly =>
        let idents := collectIdents child
        if h : idents.size > 0 then
          let parent := idents[0]
          for i in [1:idents.size] do
            acc := acc.push (parent ++ idents[i]!)
      | `Lean.Parser.Command.openRenaming =>
        let idents := collectIdents child
        if h : idents.size > 0 then
          let parent := idents[0]
          let mut i := 1
          while i < idents.size do
            acc := acc.push (parent ++ idents[i]!)
            i := i + 2
      | _ => pure ()
  return acc

def collectOpenReferences (env : Environment) (modules : Array ProjectModule)
    (projectMods : NameSet) : IO NameSet := do
  let mut acc : NameSet := {}
  for m in modules do
    let raw ← IO.FS.readFile m.source
    let source := raw.replace "\r\n" "\n"
    let r ← Leaner.Core.Parser.parseModule source env
      { fileName := m.source.toString, resolveImports := false }
    match r with
    | .error _ => pure ()
    | .ok stx _ _ _ =>
      for n in openReferencesInModule stx do
        match env.getModuleIdxFor? n with
        | some midx =>
          let modName := env.allImportedModuleNames[midx.toNat]!
          if projectMods.contains modName then
            acc := acc.insert n
        | none => pure ()
  return acc

structure RootSets where
  simp : NameSet := {}
  ext : NameSet := {}
  inst : NameSet := {}
  aliased : NameSet := {}
  deriving Inhabited

def buildRootSets (env : Environment) (projectMods : NameSet) (cfg : RootConfig) : RootSets :=
  { simp := if cfg.includeSimp then simpRootSet env else {}
    ext := if cfg.includeExt then extRootSet env else {}
    inst := if cfg.includeInstances then instanceRootSet env else {}
    aliased := aliasRootSet env projectMods }

def isRoot (env : Environment) (cfg : RootConfig) (rs : RootSets) (entry : ConstantEntry) : Bool := Id.run do
  let n := entry.name
  if cfg.entryPoints.contains n then return true
  if cfg.includeMain && n == `main then return true
  if cfg.includeExported && (Lean.getExportNameFor? env n).isSome then return true
  if cfg.includeExtern && (Lean.getExternAttrData? env n).isSome then return true
  if cfg.includeInit then
    if (Lean.getRegularInitFnNameFor? env n).isSome then return true
    if (Lean.getBuiltinInitFnNameFor? env n).isSome then return true
  if rs.inst.contains n then return true
  if rs.simp.contains n then return true
  if rs.ext.contains n then return true
  if rs.aliased.contains n then return true
  return false

def collectRoots (env : Environment) (cfg : RootConfig) (rs : RootSets)
    (consts : Array ConstantEntry) (candidates : NameSet) : NameSet :=
  consts.foldl (init := ({} : NameSet)) fun s e =>
    if isRoot env cfg rs e || !candidates.contains e.name then s.insert e.name else s

private def companionSuffixes : Array String :=
  #["_unsafe_rec", "_cstage1", "_cstage2", "_cstage3", "_cstage4", "_sunfold",
    "_redArg", "_eq_def", "_eq_unfold"]

private def companionParent? (n : Name) : Option Name :=
  match n with
  | .str p s =>
    if companionSuffixes.contains s then some p else none
  | _ => none

private def addEdge (g : UsesGraph) (from_ to_ : Name) : UsesGraph :=
  let prev := g[from_]?.getD ({} : NameSet)
  g.insert from_ (prev.insert to_)

def augmentGraphWithAttrs (env : Environment) (g : UsesGraph) : UsesGraph := Id.run do
  let mut g := g
  for (n, _) in g.toArray do
    if let some target := Lean.Compiler.getImplementedBy? env n then
      g := addEdge g n target
    if let some target := Lean.getRegularInitFnNameFor? env n then
      g := addEdge g n target
    if let some target := Lean.getBuiltinInitFnNameFor? env n then
      g := addEdge g n target
    if let some parent := companionParent? n then
      g := addEdge g n parent
      g := addEdge g parent n
  return g

partial def reachableFromRoots (g : UsesGraph) (roots : NameSet) : NameSet := Id.run do
  let mut reached : NameSet := roots
  let mut work : Array Name := roots.toArray
  while !work.isEmpty do
    let n := work.back!
    work := work.pop
    let succs := g[n]?.getD ({} : NameSet)
    for s in succs do
      unless reached.contains s do
        reached := reached.insert s
        work := work.push s
  return reached

def isUserShape : ConstantInfo → Bool
  | .defnInfo _ => true
  | .thmInfo _ => true
  | .axiomInfo _ => true
  | .opaqueInfo _ => true
  | .inductInfo _ => true
  | _ => false

def isReducibleDef (env : Environment) : ConstantInfo → Bool
  | .defnInfo v =>
    v.hints matches .abbrev || (Lean.getReducibilityStatusCore env v.name) == .reducible
  | _ => false

def shapeIsFlaggable (cfg : RootConfig) : ConstantInfo → Bool
  | .thmInfo _ => cfg.flagTheorems
  | .axiomInfo _ => cfg.flagAxioms
  | info => isUserShape info

private partial def hasAutoGenComponent : Name → Bool
  | .anonymous => false
  | .num p _ => hasAutoGenComponent p
  | .str p s =>
    let exact := #["ctorIdx", "toCtorIdx", "ctorElimType", "rec", "recOn", "casesOn",
                   "brecOn", "binductionOn", "below", "ibelow", "noConfusion",
                   "noConfusionType", "noConfusionEnum", "rawCast", "sizeOf_spec",
                   "eq_def", "eq_unfold"]
    if exact.contains s then true
    else if s.startsWith "brecOn_" || s.startsWith "binductionOn_"
         || s.startsWith "below_" || s.startsWith "rec_" then true
    else hasAutoGenComponent p

def isCandidate (env : Environment) (cfg : RootConfig) (e : ConstantEntry) : Bool := Id.run do
  if !shapeIsFlaggable cfg e.info then return false
  if isReducibleDef env e.info then return false
  if e.name.isInternalDetail then return false
  if Lean.isAuxRecursor env e.name then return false
  if Lean.isNoConfusion env e.name then return false
  if env.isProjectionFn e.name then return false
  if hasAutoGenComponent e.name then return false
  if (Lean.declRangeExt.find? (level := .server) env e.name
      |>.orElse fun _ => Lean.declRangeExt.find? (level := .private) env e.name).isNone then
    return false
  return true

def findDeadCandidates (env : Environment) (cfg : RootConfig) (consts : Array ConstantEntry)
    (reached : NameSet) : Array ConstantEntry :=
  consts.filter fun e => isCandidate env cfg e && !reached.contains e.name

def declRange? (env : Environment) (n : Name) : Option DeclarationRanges :=
  Lean.declRangeExt.find? (level := .server) env n
    |>.orElse fun _ => Lean.declRangeExt.find? (level := .private) env n
    |>.orElse fun _ => Lean.declRangeExt.find? (level := .exported) env n

private def posOfLean (p : Lean.Position) : Leaner.Core.Position :=
  { line := p.line, column := p.column + 1 }

def rangeOf? (env : Environment) (n : Name) : Option Leaner.Core.Range := do
  let r ← declRange? env n
  some {
    start := posOfLean r.range.pos
    stop := posOfLean r.range.endPos
  }


end Leaner.Weeder
