import Lake
open Lake DSL

package "leaner" where
  version := v!"0.1.2"

require Cli from git "https://github.com/leanprover/lean4-cli" @ "main"
require kenosis from git "https://github.com/SrGaabriel/kenosis" @ "main"

lean_lib Leaner

@[default_target]
lean_exe leaner where
  root := `Main
  supportInterpreter := true
  moreLinkArgs :=
    if System.Platform.isWindows then #[]
    else if System.Platform.isOSX then #["-Wl,-rpath,@executable_path/../lib/lean"]
    else #["-Wl,-rpath,$ORIGIN/../lib/lean"]

private def leanBinDir : IO System.FilePath := do
  let out ← IO.Process.output { cmd := "lean", args := #["--print-prefix"] }
  return (out.stdout.trimAscii.toString : String) / "bin"

script install do
  let exeFile := if System.Platform.isWindows then "leaner.exe" else "leaner"
  let src : System.FilePath := ".lake" / "build" / "bin" / exeFile
  if !(← src.pathExists) then
    IO.eprintln s!"Binary not found at {src}. Run `lake build` first."
    return 1
  let binDir ← leanBinDir
  let dest := binDir / exeFile
  copyFile src dest
  println! s!"Installed {exeFile} to {dest}"
  if System.Platform.isWindows then
    let home := (← IO.getEnv "USERPROFILE").getD ""
    let shimDest : System.FilePath := home / ".elan" / "bin" / "leaner.cmd"
    let shimContent := s!"@echo off\r\n\"{dest}\" %*\r\n"
    IO.FS.writeFile shimDest shimContent
    println! s!"Created shim at {shimDest}"
  return 0

script link do
  let exeFile := if System.Platform.isWindows then "leaner.exe" else "leaner"
  let src : System.FilePath := ".lake" / "build" / "bin" / exeFile
  if !(← src.pathExists) then
    IO.eprintln s!"Binary not found at {src}. Run `lake build` first."
    return 1
  let dest := (← leanBinDir) / exeFile
  IO.FS.hardLink src dest
  println! s!"Linked leaner to {dest}"
  return 0
