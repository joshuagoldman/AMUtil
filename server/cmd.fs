module CmdPrompt

open System
open System.Text.RegularExpressions
open System.IO
open System

let executeCommand command =
    let env = Environment.GetEnvironmentVariable("Path", EnvironmentVariableTarget.Machine)
    let msbuild_path =
        let env_paths =
            env.Split(";")

        env_paths
        |> Array.tryFind (fun path ->
                (path : string).ToLower().Contains("Git")
            )
    match msbuild_path with
    | Some path ->
        let gitBashProcess = new System.Diagnostics.Process()
        let cmdInfo = System.Diagnostics.ProcessStartInfo()
        cmdInfo.Arguments <- command // such as "fetch origin"
        cmdInfo.UseShellExecute <- false; 
        cmdInfo.WorkingDirectory <- "C:\Data"
        cmdInfo.FileName <- path + "git-bash.exe"
        cmdInfo.RedirectStandardOutput <- true
        gitBashProcess.StartInfo <- cmdInfo
        gitBashProcess.Start()
        |> function
        | processStartedSuccesfully when processStartedSuccesfully ->
                //gitProcess.WaitForExit()
                let allStringinfo = 
                    gitBashProcess.StandardOutput.ReadToEnd()
                
                gitBashProcess.Close()


                allStringinfo
        | _ ->
            let allStringinfo = 
                gitBashProcess.StandardError.ReadToEnd()
            gitBashProcess.Close()
            allStringinfo
    | _ -> ""