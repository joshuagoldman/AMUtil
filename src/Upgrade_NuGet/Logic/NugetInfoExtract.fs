module Upgrade_NuGet.Logic.NugetInfoExtract

open JsInterop
open Elmish
open Fable.Import
open System
open Upgrade_NuGet.Types
open Global.Types
open Feliz
open Fable.Core.JsInterop

type ProjectUtils = {
    ProjInfo : string
    NugetServerInfo : string
    ProjectName : string
    Dispatch : Msg -> unit
}

let standardFailMsg = "Loading was not successfull"

let nugetServerRegex ( projectName : string ) =
                String.Format(
                    "(?<=Ericsson.AM.{0}',Version=').*(?=')",
                    projectName
                )

let nugetPackageVersionRegex = "(?<=<Version>).*(?=<\/Version>)"

let partOfNugetServerActions ( utils : ProjectUtils ) = async {


    let existingPackages =
        JsInterop.Regex.Matches (nugetServerRegex utils.ProjectName) utils.NugetServerInfo 
        
    let! newMixItem =
        {
            Name = utils.ProjectName
            Changes = Project_Changes.Project_Has_No_Changes
            Nuget_Names =
                {
                    CurrName =
                        JsInterop.Regex.Match nugetPackageVersionRegex utils.ProjInfo
                        |> fun x -> x.Value

                    New_Nuget_Name = New_Nuget_Name.Has_No_Name
                }
            Existing_Packages =
                if existingPackages.IsSome
                then
                   existingPackages.Value
                else
                    [|""|]
            Server_Options = No_Server_Actions
            Loading_To_Server = Not_Loading_Info_To_Server
        }
        |> Loganalyzer_Projects_Table_Result.Loading_Was_Successfull
        |> fun mix ->
            (mix,utils.Dispatch)
            |> Upgrade_NuGet.Types.Change_LogAnalyzer_Loading_Mix
            |> delayedMessage 2000
        
    return(newMixItem)
}

let nugetVersionFoundActions isPartOfNugetServerOpt ( utils : ProjectUtils ) = async {
    match isPartOfNugetServerOpt with
    | Some isPartOfNugetServer ->
        match isPartOfNugetServer with
        | true ->
            
            return! partOfNugetServerActions utils

        | _ ->
            let newMixItem =
                (
                    (utils.ProjectName,"Project is not part of NuGet server")
                    |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
                )
                |> fun mix ->
                    (mix,utils.Dispatch)
                    |> Upgrade_NuGet.Types.Change_LogAnalyzer_Loading_Mix

            return(newMixItem)
        
    | _ ->
        let newMixItem =
            (
                (utils.ProjectName,"Project is not part of NuGet server")
                |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
            )
            |> fun mix ->
                (mix,utils.Dispatch)
                |> Upgrade_NuGet.Types.Change_LogAnalyzer_Loading_Mix

        return(newMixItem)
}

let evaluateNugetVersion foundNugetVersionOpt ( utils : ProjectUtils ) = async {
    match (foundNugetVersionOpt : bool option) with
    | Some _ ->
        let isPartOfNugetServerOpt =
                if utils.ProjectName.ToUpper().Contains("RCOHANDLER")
                then Some(true)
                else    
                    JsInterop.Regex.IsMatch (nugetServerRegex utils.ProjectName) utils.NugetServerInfo

        return! (nugetVersionFoundActions isPartOfNugetServerOpt utils)
    | _ ->
        let newMixItem =
            (
                (utils.ProjectName,standardFailMsg)
                |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
            )
            |> fun mix ->
                (mix,utils.Dispatch)
                |> Upgrade_NuGet.Types.Change_LogAnalyzer_Loading_Mix

        return(newMixItem) 
}


let monitorEachProjectInfoExtraction ( nugetServerInfo : string )
                                       projectName
                                       dispatch = async {

    let! projInfo = apis.GetProjecInfo projectName

    let foundNugetVersionOpt =
        JsInterop.Regex.IsMatch nugetPackageVersionRegex projInfo

    let utils = 
        {
            ProjInfo =  projInfo
            NugetServerInfo = nugetServerInfo
            ProjectName = projectName
            Dispatch = dispatch
        }

    return! (evaluateNugetVersion foundNugetVersionOpt utils)
}

let getAllAvailablePackageVersions dispatch = async {

    let popupMsg =
        "Getting all NuGet package versions from http://segaeesw04.eipu.ericsson.se/nuget/Packages..."
        |> Popup.View.getPopupMsgSpinner
        |> Upgrade_NuGet.Logic.Miscellaneous.checkingProcessPopupMsg Upgrade_NuGet.Logic.Miscellaneous.standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000

    let! result = Global.Types.apis.NuGetInfo  "http://segaeesw04.eipu.ericsson.se/nuget/Packages"

    match result with
    | Ok okRes ->
        [|
            okRes |>
            (
                Nuget_Server_Is_Available >>
                Change_Nuget_Server_Info
            )

            dispatch |>
            (
                Get_All_Projects_Info
            )
        |]
        |> Array.iter (fun msg -> dispatch msg)
        
    | Error err ->
        let exitMsg =
            err + ". Couldn't reach NuGet server. Please refresh to return"
            |> Popup.View.getPopupMsg

        let button =
            Popup.View.simpleOkButton
                            Upgrade_NuGet.Logic.Miscellaneous.killPopupMsg
                            dispatch

        let kickedOutMsg =
            (button,exitMsg) |>
            (
                GlobalMsg.Go_To_Failed_Page >>
                Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                dispatch
            )

        kickedOutMsg
}

