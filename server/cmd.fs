module CmdPrompt

open System
open System.Text.RegularExpressions
open System.IO
open System
open System.Diagnostics

let getWorkingpath pathOpt =
    match pathOpt with
        | Some path -> path
        | _ -> "C:/Data"

let executeCommand pathOpt command =
    try
        let pathToBash = Environment.GetEnvironmentVariable("BASHPATH").Replace("\\","/")
        let bashFile = pathToBash + "/bash.exe"
        let gitBashProcess = new System.Diagnostics.Process()
        let cmdInfo = System.Diagnostics.ProcessStartInfo()
        cmdInfo.UseShellExecute <- false 
        cmdInfo.WorkingDirectory <- getWorkingpath pathOpt
        cmdInfo.FileName <- bashFile
        cmdInfo.Verb <- "runas" 
        cmdInfo.RedirectStandardOutput <- true
        cmdInfo.RedirectStandardError <- true
        cmdInfo.RedirectStandardInput <- true
        cmdInfo.WindowStyle <- ProcessWindowStyle.Hidden
        gitBashProcess.StartInfo <- cmdInfo

        gitBashProcess.Start()
        |> function
        | processStartedSuccesfully when processStartedSuccesfully ->
            //gitProcess.WaitForExit()
            using ( gitBashProcess.StandardInput) ( fun sw ->
                if sw.BaseStream.CanWrite
                then
                    sw.WriteLine(command : string)
            )

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
    