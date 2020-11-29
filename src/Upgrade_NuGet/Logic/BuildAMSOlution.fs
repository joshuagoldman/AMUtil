module Upgrade_NuGet.Logic.BuildAMSolution

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

let allisBuild projs =
    projs
    |> Array.forall (fun info ->
        match info.Loading_To_Server with
        | Info_Loaded_Options.Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done alternatives ->
                match alternatives with
                | Loading_To_Nuget_Server_Alternatives.Changing_Nuget_Name _ ->
                    false
                | Loading_To_Nuget_Server_Alternatives.Executing_Nuget_Server_Command ->
                    false
                | Loading_To_Nuget_Server_Alternatives.Building ->
                    true
                    
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Done res ->
                match res with
                | Loading_To_Server_Result.Loading_To_Server_Failed msg ->
                    true
                | Loading_To_Server_Result.Loading_To_Server_Succeeded ->
                    false
        | _ -> false   )

let buildSolution projectsLoading dispatch = async {

    let reqStr = "shellCommand=cd server;cd loganalyzer;dotnet build Ericsson.AM.sln"

    let! res = request reqStr

    let buildFailedMsgs msg =
        projectsLoading
        |> Array.map (fun proj ->
            let newStatus =
                msg |>
                (
                    Loading_To_Server_Failed >>
                    Loading_Nuget_Info_Is_Done >>
                    Loading_Info_To_Server
                )
     
            let newLoadingStatusMsg =
                { proj with Loading_To_Server = newStatus} |>
                (
                    Upgrade_NuGet.Types.Change_Project_Info 
                )
                |> turnIntoSendPopupWithNewState dispatch
            newLoadingStatusMsg
            )

    match res.status with
    | 200.0 ->
        match (res.responseText.Contains("Build succeeded.")) with
        | false ->

            let res =
                "Build failed. Make sure you solution compiles clean!" |>
                (
                    buildFailedMsgs >>
                    Upgrade_NuGet.Types.Batch 
                )
            return(res)

        | _ ->
            let buildSuccededMsgs =
                projectsLoading
                |> Array.map (fun proj ->

                    let performNugetActionMsgAsync =
                        let performNugetActionMsg =
                            match proj.Server_Options with
                            | Server_Options.Push_Nuget ->
                                match proj.Nuget_Names.New_Nuget_Name with
                                | New_Nuget_Name.Has_New_Name validity ->
                                    match validity with
                                    | Nuget_Name_Validity.Nuget_Name_Valid newName ->
                                        (proj,newName,dispatch) |>
                                        (
                                            Perform_Nuget_Action_To_Server
                                        )
                                    | _ ->
                                        MsgNone
                                        |> Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget
                                | _ ->
                                    MsgNone
                                    |> Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget
                            | Server_Options.Is_To_Be_Deleted ->
                                (proj,proj.Nuget_Names.CurrName,dispatch) |>
                                (
                                    Perform_Nuget_Action_To_Server
                                )
                            | Server_Options.Is_To_Be_Updated ->
                                (proj,proj.Nuget_Names.CurrName,dispatch) |>
                                (
                                    Perform_Nuget_Action_To_Server
                                )
                            | _ ->
                                MsgNone
                                |> Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget

                        performNugetActionMsg
                        |> delayedMessage 2000
                        |> Upgrade_Nuget_Async
                    
                    let newStatus =
                        Loading_To_Nuget_Server_Alternatives.Executing_Nuget_Server_Command |>
                        (
                            Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done >>
                            Loading_Info_To_Server
                        )
             
                    let newLoadingStatusMsg =
                        [|
                            { proj with Loading_To_Server = newStatus} |>
                            (
                                Upgrade_NuGet.Types.Change_Project_Info 
                            )
                            |> turnIntoSendPopupWithNewState dispatch
                            

                            performNugetActionMsgAsync

                        |]
                        |> Upgrade_NuGet.Types.Batch
                    newLoadingStatusMsg
                    )
            let res = buildSuccededMsgs |> Upgrade_NuGet.Types.Batch

            return(res)
    | _ ->
        let res =
            res.responseText |>
            (
                buildFailedMsgs >>
                Upgrade_NuGet.Types.Batch 
            )
        return(res)
}

let decideifBuild model dispatch =
    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info projs ->
            let projectsLoading =
                projs
                |> Array.choose (fun proj ->
                    match proj.Loading_To_Server with
                    | Loading_Info_To_Server loading_status ->
                        match loading_status with
                        | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done _ ->
                            Some proj
                        | _ -> None
                    |  _ -> None)

            match (projectsLoading |> Array.length) with
            | 0 ->
                model, Global.Types.MsgNone, []
            | _ ->
                match (allisBuild projectsLoading) with
                | true ->
                    let buildingInformation =
                        "Building Ericsson.AM solution..." |>
                        (
                            Popup.View.getPopupMsgSpinner >>
                            checkingProcessPopupMsg standardPositions
                        )

                    let msg =
                        [|
                            buildingInformation

                            dispatch |>
                            (
                                buildSolution projectsLoading >>
                                Upgrade_NuGet.Types.Upgrade_Nuget_Async
                            )
                        |] |>
                        (
                            Upgrade_NuGet.Types.Batch >>
                            Cmd.ofMsg
                        )
                        
                    model, Global.Types.MsgNone, msg
                | _ ->
                    model, Global.Types.MsgNone, []
        | _ ->
            model, Global.Types.MsgNone, []
    | _ ->
        model, Global.Types.MsgNone, []