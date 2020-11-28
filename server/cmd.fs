module CmdPrompt

open System
open System.Text.RegularExpressions
open System.IO
open System
open System.Diagnostics

let executeCommand args command =
    try
        let currPath = Directory.GetCurrentDirectory()
        let pathReNamed = currPath.Replace("\\","/")
        let gitBashProcess = new System.Diagnostics.Process()
        let cmdInfo = System.Diagnostics.ProcessStartInfo()
        cmdInfo.Arguments <- args // such as "fetch origin"
        cmdInfo.UseShellExecute <- false 
        cmdInfo.WorkingDirectory <- pathReNamed
        cmdInfo.FileName <- command
        cmdInfo.Verb <- "runas" 
        cmdInfo.RedirectStandardOutput <- true
        cmdInfo.RedirectStandardError <- true
        cmdInfo.WindowStyle <- ProcessWindowStyle.Hidden
        gitBashProcess.StartInfo <- cmdInfo
        gitBashProcess.Start()
        |> function
        | processStartedSuccesfully when processStartedSuccesfully ->
                //gitProcess.WaitForExit()
                let allStringinfo = 
                    gitBashProcess.StandardOutput.ReadToEnd()
                
                match allStringinfo.Length with
                | 0 -> 
                    let allStringinfo = 
                        gitBashProcess.StandardError.ReadToEnd()
                    gitBashProcess.Close()
                    allStringinfo
                | _ ->
                    gitBashProcess.Close()
                    allStringinfo
        | _ ->
            let allStringinfo = 
                gitBashProcess.StandardError.ReadToEnd()
            gitBashProcess.Close()
            allStringinfo
    with
    | (e : Exception) ->
        e.Message