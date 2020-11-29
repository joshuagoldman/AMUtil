module Upgrade_NuGet.Logic.NugetServerActions

open JsInterop
open Elmish
open Fable.Import
open System
open Upgrade_NuGet.Types
open Global.Types
open Feliz
open Fable.Core.JsInterop
open SharedTypes
open Fable.Remoting.Client

let strResponse prms = async {
    let! responses = Global.Types.apis.Command prms

    let responsesAll =
        responses
        |> Array.map (fun resp ->
                resp.Answer
            )
        |> String.concat "\n"
       
    return responsesAll
}

let performNugetActionToServerAsync proj version dispatch = async {
    do! Async.Sleep 2000

    let nugetPushTemplate projName version =
        let args =
            String.Format(
                "dotnet nuget push \"Ericsson.AM.{0}/bin/Debug/Ericsson.AM.{0}.{1}.nupkg\" -k 875e5930-56d2-4f06-9fe9-f3f5a8c09aa2 -s http://segaeesw04.eipu.ericsson.se/nuget",
                projName,
                version
            )

        let prms = 
            [|
                {
                    SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
                    SharedTypes.CdCommand.ResponseCommand = args
                }
                |> SharedTypes.IsCd
            |]

        prms

    let nugetDeleteTemplate projName version =
        let args =
            String.Format(
                "yes|dotnet nuget delete Ericsson.AM.{0} {1} -k 875e5930-56d2-4f06-9fe9-f3f5a8c09aa2 -s http://segaeesw04.eipu.ericsson.se/nuget",
                projName,
                version
            )

        let prms = 
            [|
                {
                    SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
                    SharedTypes.CdCommand.ResponseCommand = args
                }
                |> SharedTypes.IsCd
            |]

        prms

    let failedMsg msg =
        let newStatus =
            msg |>
            (
                Loading_To_Server_Failed >>
                Loading_Nuget_Status.Loading_Nuget_Info_Is_Done >>
                Loading_Info_To_Server
            )
             
        let newLoadingStatusMsg =
            { proj with Loading_To_Server = newStatus} |>
            (
                Upgrade_NuGet.Types.Change_Project_Info 
            )
            |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch

        newLoadingStatusMsg

    let getErrorMsg txt altMsg =
        JsInterop.Regex.Match "(?<=error:)(.|\n)*?(?=Usage)" txt
        |> fun x ->
            match x with
            | Some m -> m
            | _ -> altMsg

    let pusProcedure pushReqPrms = async {
        let! pushRes = strResponse pushReqPrms
        
        match (pushRes.Contains("Your package was pushed.")) with
        | false ->
            
            let errorMsg =
                getErrorMsg pushRes "couldn't push NuGet package"

            return(failedMsg errorMsg)
    
        | _ ->
            let newStatus =
                Loading_To_Server_Result.Loading_To_Server_Succeeded |>
                (
                    Loading_Nuget_Status.Loading_Nuget_Info_Is_Done >>
                    Loading_Info_To_Server
                )
                 
            let newLoadingStatusMsg =
                { proj with Loading_To_Server = newStatus} |>
                (
                    Upgrade_NuGet.Types.Change_Project_Info 
                )
                |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch

            return(newLoadingStatusMsg)
    }

    match proj.Server_Options with
    | Server_Options.Is_To_Be_Updated ->

        let! deleteResp = request nugetDeleteTemplate
        
        match (deleteResp.responseText.Contains("was deleted successfully.")) with
        | false ->
            
            let errorMsg =
                getErrorMsg deleteResp.responseText "couldn't delete NuGet before pushing"

            return(failedMsg errorMsg)
    
        | _ ->
            let! res = pusProcedure (nugetPushTemplate proj.Name version)

            return(res)
            
    | Server_Options.Push_Nuget ->
        let! res = pusProcedure (nugetPushTemplate proj.Name version)
        
        return(res)
    | Server_Options.Is_To_Be_Deleted ->
        let! deleteResp = request nugetDeleteTemplate
        
        match deleteResp.status with
        | 200.0 ->
            match (deleteResp.responseText.Contains("was deleted successfully.")) with
            | false ->
                
                let errorMsg =
                    getErrorMsg deleteResp.responseText "couldn't delete NuGet"

                return(failedMsg errorMsg)
        
            | _ ->
                let newStatus =
                    Loading_To_Server_Result.Loading_To_Server_Succeeded |>
                    (
                        Loading_Nuget_Status.Loading_Nuget_Info_Is_Done >>
                        Loading_Info_To_Server
                    )
                     
                let newLoadingStatusMsg =
                    { proj with Loading_To_Server = newStatus} |>
                    (
                        Upgrade_NuGet.Types.Change_Project_Info 
                    )
                    |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch

                return(newLoadingStatusMsg)
        | _ ->
            return(failedMsg deleteResp.responseText)
    | _ ->
        return(MsgNone |> Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget)
        
}