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
