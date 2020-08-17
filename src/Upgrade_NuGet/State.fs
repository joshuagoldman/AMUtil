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
    | Get_Project_Info(proj_name,dispatch) ->
        match model.Projects_Table with
        | Loganalyzer_Projects_Table_Status.Info_Is_Loading mix ->
            Logic.monitorEachProjectInfoExtraction mix proj_name dispatch
            |> Async.StartImmediate

            model, Global.Types.MsgNone, []
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
