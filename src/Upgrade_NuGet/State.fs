module Upgrade_NuGet.State

open Elmish
open Types
open JsInterop
open Fable.Core.JsInterop

let init result =
    {
        Info = Git_Info_Nuget.No_Git_Info_Nuget
        Projects_Table = Loganalyzer_Projects_Table_Status.Info_Not_Loaded
    }


let update msg (model:Model) : Types.Model * Global.Types.GlobalMsg * Cmd<Msg> =
    match msg with
    | Batch msgs ->
        let msgsIntoOneCmd =
            msgs
            |> Array.map (fun msg ->
                msg |> Cmd.ofMsg)
            |> Cmd.batch

        model, Global.Types.MsgNone, msgsIntoOneCmd
    | Batch_Upgrade_Nuget_Async asyncMsgs ->
        let msgsIntoOneCmd =
            asyncMsgs
            |> Array.map (fun msg ->
                msg |> Cmd.fromAsync)
            |> Cmd.batch

        model, Global.Types.MsgNone, msgsIntoOneCmd
    | Popup_Msg_Upgrade_Nuget style ->
        let msg =
            Global.Types.Popup_Msg_Global style

        model, msg, []
        
    | GlobalMsg_Upgrade_Nuget msg ->
        model, msg, []
    | Change_Project_Info newProject ->
        match model.Projects_Table with
        | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded result ->
            match result with
            | Yes_Projects_Table_Info projects ->
                let newProjects =
                    projects
                    |> Array.map (fun project ->
                        if project.Name = newProject.Name
                        then
                            newProject
                        else
                            project)
                    |> Yes_Projects_Table_Info
                    |> Info_Has_Been_Loaded

                { model with Projects_Table = newProjects }, Global.Types.MsgNone, []

            | _ -> model, Global.Types.MsgNone, []
        | _ -> model, Global.Types.MsgNone, []

    | Change_NuGet_Status status ->
        { model with Projects_Table = status}, Global.Types.MsgNone, []
    | Get_Project_Info proj_name ->
        match model.Projects_Table with
        | Loganalyzer_Projects_Table_Status.Info_Is_Loading mix ->
            let msg =
                Logic.monitorEachProjectInfoExtraction mix proj_name
                |> Cmd.fromAsync
            
            model, Global.Types.MsgNone, msg
        | _ ->
            model, Global.Types.MsgNone, []
    | Change_Project_Status project ->
        let newProjInfoOpt =
            Logic.changeProjectStatus
                            model
                            project
        match newProjInfoOpt with
        | Some newProjInfo ->
            let newState =
                newProjInfo |>
                (
                    Loganalyzer_Projects_Table.Yes_Projects_Table_Info >>
                    Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded
                )
            { model with Projects_Table = newState}, Global.Types.MsgNone, []
        | _ ->
            model, Global.Types.MsgNone, []
    | New_Nuget_Name_Change(info_chosen,ev) ->
        let newProjOpt =
            Logic.newNugetNameEvaluation model.Projects_Table info_chosen ev

        match newProjOpt with
        | Some newProj ->
            { model with Projects_Table = newProj }, Global.Types.MsgNone, []
        | _ -> model, Global.Types.MsgNone, []

    | Obtain_New_Nuget_Info(dispatch,activity) ->
        Logic.checkIfDotNetInstalled dispatch
        |> Async.StartImmediate

        let activityMsg =
            activity |> Global.Types.Change_Activity_Global

        model, activityMsg, []

    | Get_All_Projects_Info dispatch ->
        Logic.getNuGetTableInfo dispatch
        |> Async.StartImmediate

        model, Global.Types.MsgNone, []
