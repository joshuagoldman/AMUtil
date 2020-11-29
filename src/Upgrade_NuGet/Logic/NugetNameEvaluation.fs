module Upgrade_NuGet.Logic.NugetNameEvaluation

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

let isWrittenNewNugetNameValid newName project =
            let validNuGetVersionRegex = "\d{1,}.\d{1,}.\d{1,}($|-\w+)"
            let validNugetNameOpt = JsInterop.Regex.IsMatch validNuGetVersionRegex newName
            match validNugetNameOpt with
            | Some validNugetName ->
                match validNugetName with
                | true ->
                    let alreadyExistsInServer =
                        project.Existing_Packages
                        |> Array.exists (fun pckg ->
                            pckg = newName)
               
                    match alreadyExistsInServer with
                    | false ->
                        newName |>
                        (
                            Nuget_Name_Validity.Nuget_Name_Valid >>
                            Some
                        )
                            
                    | _ ->
                        Not_Valid_Nuget_Reason.Nuget_Already_In_Server |>
                        (
                            Nuget_Name_Not_Valid >>
                            Some
                        )
                | _ ->
                    Not_Valid_Nuget_Reason.Has_Wrong_Pattern |>
                    (
                        Nuget_Name_Not_Valid >>
                        Some
                    )
            | _ ->
                Not_Valid_Nuget_Reason.Has_Wrong_Pattern |>
                (
                    Nuget_Name_Not_Valid >>
                    Some
                )

let newNugetNameEvaluation projects_table info_chosen ( ev : Browser.Types.Event )=
    let newName = ev.target?value : string

    let validationRes = isWrittenNewNugetNameValid newName info_chosen

    let newInfo =
        match validationRes with
        | Some res ->
            let newNewNugetName =
                { info_chosen.Nuget_Names with New_Nuget_Name = res |> New_Nuget_Name.Has_New_Name}
    
            let newProjInfo =
                { info_chosen with Nuget_Names = newNewNugetName}
    
            newProjInfo
        | _ ->
            let newNewNugetName =
                { info_chosen.Nuget_Names with New_Nuget_Name = New_Nuget_Name.Has_No_Name}
    
            let newProjInfo =
                { info_chosen with Nuget_Names = newNewNugetName}
    
            newProjInfo
    
    match projects_table with
    | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info infos ->
            let newInfos =
                infos
                |> Array.map (fun info ->
                    if info.Name = info_chosen.Name
                    then
                        newInfo
                    else
                        info)
    
            let newProjectsStatus =
                newInfos |>
                (
                    Loganalyzer_Projects_Table.Yes_Projects_Table_Info >>
                    Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded
                )
    
            Some newProjectsStatus
        | _ -> None   
    | _ -> None

let changeToBuildingMsgs projs dispatch =
    projs
    |> Array.map (fun proj ->
        let newStatus =
            Loading_To_Nuget_Server_Alternatives.Building |>
            (
                Loading_Nuget_Info_Is_Not_Done >>
                Loading_Info_To_Server
            )
         
        let newLoadingStatusMsg =
            { proj with Loading_To_Server = newStatus} |>
            (
                Upgrade_NuGet.Types.Change_Project_Info 
            )
            |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch

        newLoadingStatusMsg
        )
    |> Upgrade_NuGet.Types.Batch

let allProjsLoadingDecisionQuestionPopup projects =
    projects
    |> Array.choose (fun proj ->
        match proj.Server_Options with
        | Server_Options.No_Server_Actions ->
            None
        | Server_Options.Push_Nuget ->
            match proj.Nuget_Names.New_Nuget_Name with
            | New_Nuget_Name.Has_New_Name validity ->
                match validity with
                | Nuget_Name_Validity.Nuget_Name_Valid version ->
                    String.Format(
                        "{0} -> Push NuGet with version {1}",
                        proj.Name,
                        version
                    )
                    |> Some
                | _ -> None
            | _ ->
                None
        | Server_Options.Is_To_Be_Deleted ->
            String.Format(
                "{0} -> version {1} is about to be deleted from NuGet server",
                proj.Name,
                proj.Nuget_Names.CurrName
            )
            |> Some
        | Server_Options.Is_To_Be_Updated ->
            String.Format(
                "{0} -> version {1} is about to be replaced from NuGet server",
                proj.Name,
                proj.Nuget_Names.CurrName
            )
            |> Some)
    |> Array.collect (fun projMsg ->
        Popup.View.getPopupMsg projMsg)

