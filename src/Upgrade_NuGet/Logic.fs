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

let monitorEachProjectInfoExtraction ( projectsLoadingInfo : Loganalyzer_Projects_Table_Mix [] ) projectName = async {
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

        let pickNewLoadingProject =
            projectsLoadingInfo
            |> Array.tail
            |> Array.choose (fun proj ->
                match proj with
                | Loganalyzer_Projects_Table_Mix.Project_Loading proj_loading ->
                    Some proj_loading
                | Loganalyzer_Projects_Table_Mix.Project_Not_Loading res ->
                    match res with
                    | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull projInfo ->
                        None
                    | _ -> None)
            |> function
                | res when res |> Array.length <> 0 ->
                    Some (res |> Array.head)
                | _ -> None
        
        match pickNewLoadingProject with
        | None ->
            let successInfo =
                newMix
                |> pickSuccessProjectInfoLoads

            match successInfo with
            | Some info ->
                [|
                    info |>
                    (
                        Yes_Projects_Table_Info >>
                        Info_Has_Been_Loaded >>
                        Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                        delayedMessage 500
                    )
                |]
                |> Batch_Upgrade_Nuget_Async
                
            | _ ->
                [|
                    No_Projects_Table_Info |>
                    (
                        Info_Has_Been_Loaded >>
                        Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                        delayedMessage 500
                    )
                |]
                |> Batch_Upgrade_Nuget_Async
                
                                
        | Some proj_to_load ->
            [|
                newMix |>
                (
                    Info_Is_Loading >>
                    Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                    delayedMessage 500
                )

                proj_to_load.Project_Name |>
                (
                    Upgrade_NuGet.Types.Get_Project_Info >>
                    delayedMessage 500
                )
            |]
            |> Batch_Upgrade_Nuget_Async
            
        

    let! res = requestCustom "http://localhost:3001/projectInfo" fullProjectNameFolder

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
                    
                return(checkFinishLoading newInfo)
                
            | _ ->
                let asyncMsg2Dispatch = checkFinishLoading (projectName|> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull)

                return(asyncMsg2Dispatch)
        | _ ->
            let asyncMsg2Dispatch = checkFinishLoading (projectName|> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull)

            return(asyncMsg2Dispatch)
    | _ ->
        let asyncMsg2Dispatch = checkFinishLoading (projectName|> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull)

        return(asyncMsg2Dispatch)
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

let newNugetNameEvaluation projects_table info_chosen ( ev : Browser.Types.Event )=
    let newName = ev.target?value : string
    
    let NugetVersionValidRegex = "\d{1,}\.\d{1,}\.\d{1,}(-.*|$)"
    
    let isNameValidOpt = JsInterop.Regex.IsMatch NugetVersionValidRegex newName
    
    let newInfo =
        match isNameValidOpt with
        | Some isNameValid ->
            match isNameValid with
            | true ->
                let newNameAsType =
                    JsInterop.Regex.Match NugetVersionValidRegex newName |>
                    (
                        (fun x -> x.Value) >>
                        Nuget_Name_Valid >>
                        New_Nuget_Name.Has_New_Name
                    )
                let newNewNugetName =
                    { info_chosen.Nuget_Names with New_Nuget_Name = newNameAsType}
    
                let newProjInfo =
                    { info_chosen with Nuget_Names = newNewNugetName}
    
                newProjInfo
            | false ->
                let newNewNugetName =
                    { info_chosen.Nuget_Names with New_Nuget_Name =
                                                    Nuget_Name_Not_Valid
                                                    |> New_Nuget_Name.Has_New_Name }
    
                let newProjInfo =
                    { info_chosen with Nuget_Names = newNewNugetName}
    
                newProjInfo
        | _ ->
            let newNewNugetName =
                { info_chosen.Nuget_Names with New_Nuget_Name =
                                                    Nuget_Name_Not_Valid
                                                    |> New_Nuget_Name.Has_New_Name }
    
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
