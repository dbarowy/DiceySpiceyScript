open Parser
open Evaluator
open System.IO
open System.Diagnostics
open System.Threading.Tasks

(* executeCommand from alexandru nedelcu https://alexn.org/blog/2020/12/06/execute-shell-command-in-fsharp/*)
type CommandResult =
  { ExitCode: int
    StandardOutput: string
    StandardError: string }
 
let executeCommand executable args =
  async {
    let! ct = Async.CancellationToken

    let startInfo = ProcessStartInfo()
    startInfo.FileName <- executable
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true
    for a in args do
      startInfo.ArgumentList.Add(a)

    use p = new Process()
    p.StartInfo <- startInfo
    p.Start() |> ignore

    let outTask =
      Task.WhenAll([|
        p.StandardOutput.ReadToEndAsync(ct);
        p.StandardError.ReadToEndAsync(ct) |])

    do! p.WaitForExitAsync(ct) |> Async.AwaitTask
    let! out = outTask |> Async.AwaitTask

    return
      { ExitCode = p.ExitCode
        StandardOutput = out.[0]
        StandardError = out.[1] }
  }

let executeShellCommand command =
  executeCommand "/usr/bin/env" [ "-S"; "bash"; "-c"; command ]

[<EntryPoint>]
let main argv : int =
    (* Check for proper usage *)
    if argv.Length <> 1 && argv.Length <> 2 then
        printfn "Usage: dotnet run <file> [debug]"
        exit 1

    (* read in the input file *)
    let file = argv.[0]
    let input = File.ReadAllText file

    (* parser debugger *)
    //let do_debug = if argv.Length = 2 then true else false

    (* Extract the base file name (without extension) *)
    let baseName = Path.GetFileNameWithoutExtension(file)
    let texFileName = baseName + ".tex"

    (* try to parse what they gave us *)
    let ast_maybe = parse input

    (* try to evaluate what we parsed... or not *)
    match ast_maybe with
    | Some ast ->
        let s= (eval ast)
        File.WriteAllText(texFileName,s)

        // Execute pdflatex command
        let pdflatexCommand = sprintf "pdflatex -interaction=nonstopmode %s" texFileName
        let result = executeShellCommand pdflatexCommand |> Async.RunSynchronously

        if result.ExitCode = 0 then
            printfn "PDF generated successfully:\n%s" result.StandardOutput
        else
            eprintfn "Error generating PDF:\n%s" result.StandardError
            exit 1
        0
    | None ->
        printfn "Invalid program."
        1
