module Upgrade_NuGet.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Types
open Global.Types
open Feliz
open Fable.Core.JsInterop

let getBranches info =
    match info with
    | Types.No_Git_Info_Nuget -> [|Html.none|]
    | Types.Yes_Git_Info_Nuget repo ->
        repo.Branches
        |> Array.map (fun branch ->
            branch |> Rco_Update.View.branchAlt)

let handleBranchNameChange gitRepo ( ev : Browser.Types.Event ) dispatch =
    let branchName = ev.target?value |> string

    match branchName with
    | "Choose Branch..." ->
        Global.Types.MsgNone |>
        (
            GlobalMsg_Upgrade_Nuget 
        )

    | _ ->
        { gitRepo with CurrBranch = branchName} |>
        (
            Spread_New_Branch_Name >>
            GlobalMsg_Upgrade_Nuget
        )

let changeNugetStatus isCurrentlyChosen =
    match isCurrentlyChosen with
    | Project_Not_Chosen ->
        Project_Chosen
    | _ ->
        Project_Not_Chosen

let haveProjectsBeenLoaded projLoadingMixes =
    projLoadingMixes
    |> Array.forall (fun proj ->
        match proj with
        | Loganalyzer_Projects_Table_Mix.Project_Loading _ -> false
        | _ -> true)

let monitorEachProjectInfoExtraction ( projectsLoadingInfo : Loganalyzer_Projects_Table_Mix [] ) projectName dispatch = async {
    let fullProjectNameFolder = "project=Ericsson.AM." + projectName

    let nugetPackageVersionRegex = "(?<=<Version>).*(?=<\/Version>)"

    let modifyProjectsLoadingInfo newInfo =
        projectsLoadingInfo
        |> Array.map (fun proj ->
            match proj with
            | Loganalyzer_Projects_Table_Mix.Project_Loading proj_loading ->
                if proj_loading.Project_Name = projectName
                then
                    newInfo
                    |> Loganalyzer_Projects_Table_Mix.Project_Not_Loading
                else
                    proj
            | _ -> proj)

    let pickSuccessProjectInfoLoads projLoadingMixes =
        projLoadingMixes
        |> Array.choose (fun proj ->
            match proj with
            | Loganalyzer_Projects_Table_Mix.Project_Loading _ ->
                None
            | Loganalyzer_Projects_Table_Mix.Project_Not_Loading res ->
                match res with
                | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull projInfo ->
                    Some projInfo
                | _ -> None)
        |> function
            | res when res |> Array.length <> 0 ->
                Some res
            | _ -> None

    let checkFinishLoading newInfo =
        let newMix = newInfo |> modifyProjectsLoadingInfo
        
        match (haveProjectsBeenLoaded newMix) with
        | true ->
            let successInfo =
                newMix
                |> pickSuccessProjectInfoLoads
            match successInfo with
            | Some info ->
                info |>
                (
                    Yes_Projects_Table_Info >>
                    Info_Has_Been_Loaded >>
                    Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                    dispatch
                )
            | _ ->
                No_Projects_Table_Info |>
                (
                    Info_Has_Been_Loaded >>
                    Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                    dispatch
                )
                                
        | _ ->
            newMix |>
            (
                Info_Is_Loading >>
                Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                dispatch
            )
        

    let! res = request fullProjectNameFolder

    match res.status with
    | 200.0 ->
        let foundNugetVersionOpt =
            JsInterop.Regex.IsMatch nugetPackageVersionRegex res.responseText

        match foundNugetVersionOpt with
        | Some foundNugetVersion ->
            match foundNugetVersion with
            | true ->
                let newInfo =
                    {
                        Name = projectName
                        Is_Chosen = Project_Chosen_option.Project_Not_Chosen
                        Changes = Project_Changes.Project_Has_No_Changes
                        Nuget_Names =
                            {
                                CurrName =
                                    JsInterop.Regex.Match nugetPackageVersionRegex res.responseText
                                    |> fun x -> x.Value
                                New_Nuget_Name = New_Nuget_Name.Has_No_Name
                            }
                    }
                    |> Loganalyzer_Projects_Table_Result.Loading_Was_Successfull
                    

                checkFinishLoading newInfo
                
            | _ ->
                checkFinishLoading Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
        | _ ->
            checkFinishLoading Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
    | _ ->
        checkFinishLoading Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
        
    return ()
}

let cretateLoadingFinishedPopup msgs dispatch  =
    let killPopupMsg =
        Popup.Types.PopupStyle.Popup_Is_Dead |>
        (
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget
        )

    let entireOkButton =
        Popup.View.simpleOkButton
                        killPopupMsg
                        dispatch

    let allMsgsCollected =
        msgs
        |> Array.collect (fun msg ->
            Popup.View.getPopupMsg msg)
    let msgAsPopup =
        (entireOkButton,allMsgsCollected)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok 

    let popupMsg2Dispatch =
        (msgAsPopup,Popup.Types.standardPositions) |>
        (
            Popup.Types.Has_Alternatives >>
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
            dispatch
        )

    popupMsg2Dispatch

let cretateLoadingPopup msgs dispatch  =

    let allMsgsCollected =
        msgs
        |> Array.collect (fun msg ->
            Popup.View.getPopupMsgSpinner msg)

    let popupMsg2Dispatch =
        (allMsgsCollected,Popup.Types.standardPositions) |>
        (
            Popup.Types.Has_No_Alternatives >>
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
            dispatch
        )

    popupMsg2Dispatch

let changeProjectStatus model project =
    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info projects ->
            let newInfo =
                projects
                |> Array.map (fun proj ->
                    if proj.Name = project.Name
                    then
                        project
                    else proj)
            Some newInfo
        | _ -> None
    | _ -> None
        
