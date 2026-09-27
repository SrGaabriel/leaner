import Lean
open Lean

def main : IO Unit := do
  initSearchPath (← findSysroot)
  unsafe enableInitializersExecution
  let env ← importModules #[{module := `Soma.Dependent.Convert}] {} (loadExts := true)
  let names : Array Name := #[
    `Soma.Dependent.convert,
    `Soma.Dependent.convert._unsafe_rec,
    `Soma.Dependent.valueInPropUniverse,
    `Soma.Dependent.valueInPropUniverse._unsafe_rec
  ]
  for n in names do
    match env.find? n with
    | none => IO.println s!"{n}: not found"
    | some info =>
      let kind := match info with
        | .defnInfo _ => "defn"
        | .opaqueInfo _ => "opaque"
        | .axiomInfo _ => "axiom"
        | _ => "other"
      let allList :=
        match info with
        | .defnInfo v => v.all
        | .opaqueInfo v => v.all
        | .thmInfo v => v.all
        | .inductInfo v => v.all
        | _ => []
      IO.println s!"{n} :: kind={kind}"
      IO.println s!"  .all = {allList}"
