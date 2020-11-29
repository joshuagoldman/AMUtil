module Upgrade_NuGet.Logic.TableLoadPopup

open Global.Types
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

let getAllMsgs mix =
    mix
    |> Array.map (fun proj ->
        match proj with
        | Loganalyzer_Projects_Table_Mix.Project_Not_Loading proj_not_loading ->
            match proj_not_loading with
            | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull proj_not_loading ->
                let msg =
                    (
                        String.Format(
                            "{0} -> Loading was successfull",
                            proj_not_loading.Name
                        ),
                        Loading_Popup_Options.Spinner_Popup
                    )
                    
                msg
            | Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull (name, msg) ->
                let msg =
                    (
                        String.Format(
                            "{0} -> {1}",
                            name,
                            msg
                        ),
                        Loading_Popup_Options.Spinner_Popup
                    )
                    
                msg

        | Loganalyzer_Projects_Table_Mix.Project_Loading proj_loading ->
            let msg =
                (
                    String.Format(
                        "{0} -> {1}",
                        proj_loading.Project_Name,
                        proj_loading.Loading_Msg
                    ),
                    Loading_Popup_Options.Spinner_Popup
                )
                

            msg)

let anyProjsNugetServerLoading projs =
    projs
    |> Array.exists (fun proj ->
        match proj.Loading_To_Server with
        | Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done _ ->
                true
            | _ -> false
        |  _ -> false)

let getLoadingAlternatives projs =
    projs
    |> Array.choose (fun proj ->
        match proj.Loading_To_Server with
        | Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done alternatives ->
                Some alternatives
            | _ -> None
        |  _ -> None)
    |> function
        | res when (res|>Array.length) <> 0 ->
            true
        | _ -> false

let loadingIntoServerStrMsgs projs =
    projs
    |> Array.choose (fun info ->
        match info.Loading_To_Server with
        | Info_Loaded_Options.Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done alternatives ->
                match alternatives with
                | Loading_To_Nuget_Server_Alternatives.Changing_Nuget_Name progress ->
                    (
                        String.Format(
                            "{0} -> {1}",
                                info.Name,
                                "Changing NuGet version name"
                        ),
                        Progress_Popup(progress)
                    )
                    |> Some
                | Loading_To_Nuget_Server_Alternatives.Executing_Nuget_Server_Command ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            info.Name,
                            "Executing NuGet server command"
                        ),
                        Spinner_Popup
                    )
                    |> Some
                | Loading_To_Nuget_Server_Alternatives.Building ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            info.Name,
                            "Waiting for build"
                        ),
                        Spinner_Popup
                    )
                    |> Some
                        
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Done res ->
                match res with
                | Loading_To_Server_Result.Loading_To_Server_Failed msg ->
                    (
                        String.Format(
                            "{0} -> {1}: {2}",
                            info.Name,
                            "Loading failed",
                            msg
                        ),
                        No_Loading_Popup_Type
                    )
                    |> Some
                | Loading_To_Server_Result.Loading_To_Server_Succeeded ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            info.Name,
                            "Changes to server succeeded!"
                        ),
                        No_Loading_Popup_Type
                    )
                    |> Some
        | _ -> None   )
    |> function
        | res when (res |> Array.length) <> 0 ->
            Some res
        | _ -> None

let projectsNotLoading projs =
    projs
    |> Array.choose (fun proj ->
        match proj.Loading_To_Server with
        | Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done _ ->
                None
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Done res ->
                match res with
                | Loading_To_Server_Succeeded ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            proj.Name,
                            "Changes to server succeeded!"
                        ),
                        proj
                    )
                    |> Some
                    
                | Loading_To_Server_Failed msg ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            proj.Name,
                            msg
                        ),
                        proj
                    )
                    |> Some
                    
        |  _ -> None)
    |> function
        | res when (res|>Array.length) <> 0 ->
            res |> Some
        | _ -> None

let newStatuses projsNotLoadingAnymore dispatch =
    projsNotLoadingAnymore
    |> Array.iter (fun proj ->
             
        let newLoadingStatusMsg =
            { proj with Loading_To_Server = Info_Loaded_Options.Not_Loading_Info_To_Server} |>
            (
                Upgrade_NuGet.Types.Change_Project_Info >>
                dispatch
            )
        newLoadingStatusMsg 
        )

let getTableLoadPopup model dispatch =
    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Is_Loading mix ->
        let allProjectsLoaded = Upgrade_NuGet.Logic.Miscellaneous.haveProjectsBeenLoaded mix

        match allProjectsLoaded with
        | true ->
            let allMsgsNoTypeConsideration =
                mix
                |> getAllMsgs
                |> Array.map (fun (x,_) -> x)
            Upgrade_NuGet.Logic.Miscellaneous.cretateLoadingFinishedPopup allMsgsNoTypeConsideration dispatch
        | _ ->
            mix |>
            (
                getAllMsgs >>
                Upgrade_NuGet.Logic.Miscellaneous.cretateLoadingPopup dispatch
            )
            
    | Info_Has_Been_Loaded table ->
        match table with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info projs ->
            match (anyProjsNugetServerLoading projs) with
            | true ->
                match (loadingIntoServerStrMsgs projs) with
                | Some allMsgs ->
                    match (getLoadingAlternatives projs) with
                    | false ->
                        let allMsgsNoTypeConsideration =
                            allMsgs
                            |> Array.map (fun (x,_) -> x)
                        Upgrade_NuGet.Logic.Miscellaneous.cretateLoadingFinishedPopup allMsgsNoTypeConsideration dispatch
                    | _ ->
                        Upgrade_NuGet.Logic.Miscellaneous.cretateLoadingPopup dispatch allMsgs 
                | _ -> ()
            | _ ->
                match (projectsNotLoading projs) with
                | Some projsNotLoadingAnymore ->
                    let msgsOnly =
                        projsNotLoadingAnymore
                        |> Array.map (fun (x,_) -> x)

                    Upgrade_NuGet.Logic.Miscellaneous.cretateLoadingFinishedPopup msgsOnly dispatch

                    let statusOnly =
                        projsNotLoadingAnymore
                        |> Array.map (fun (_,y) -> y)

                    newStatuses statusOnly dispatch
                | _ -> ()
                
        | _ -> ()
            
    | _ -> ()