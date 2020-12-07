module Upgrade_NuGet.Logic.Miscellaneous

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
open SharedTypes.NuGetChange
        
let turnIntoSendPopupWithNewState dispatch msg =
    (msg,dispatch)
    |> Send_Popup_With_New_State

let handleBranchNameChange branchName gitRepo =

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

let cretateLoadingFinishedPopup msgs dispatch =
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

let cretateLoadingPopup dispatch msgs =

    let allMsgsCollected =
        msgs
        |> Array.collect (fun (msg,msgType) ->
            match msgType with
            | Global.Types.Spinner_Popup ->
                Popup.View.getPopupMsgSpinnerBeside msg
            | Global.Types.No_Loading_Popup_Type ->
                Popup.View.getPopupMsg msg
            | Global.Types.Progress_Popup progress ->
                Popup.View.getPopupMsgProgress msg progress
            )

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

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Upgrade_NuGet.Types.Popup_Msg_Upgrade_Nuget
    )

let standardPositions = {
    Popup.Types.PosX = 250.0
    Popup.Types.PosY = 250.0
}

let killPopupMsg =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Upgrade_NuGet.Types.Popup_Msg_Upgrade_Nuget
    )

let kickedOutTemplate dispatch msg =
    let exitMsg =
        msg + ". Please refresh to return"
        |> Popup.View.getPopupMsg

    let button =
        Popup.View.simpleOkButton
                        killPopupMsg
                        dispatch

    let kickedOutMsg =
        (button,exitMsg) |>
        (
            GlobalMsg.Go_To_Failed_Page >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
            dispatch
        )

    kickedOutMsg

let changeToNuGetCommandMsgs (projs : Project_Info array ) dispatch =
    let hasAValidName proj =
        match proj.Nuget_Names.New_Nuget_Name with
        | New_Nuget_Name.Has_New_Name validity ->
            match validity with
            | Nuget_Name_Valid newName ->
                newName |> Some
            | Upgrade_NuGet.Types.Nuget_Name_Not_Valid reason -> 
                match reason with
                | Upgrade_NuGet.Types.Not_Valid_Nuget_Reason.Nuget_Already_In_Server newName ->
                    newName |> Some
                | _ -> None
        | _ -> None

    let anyReadyMsgs =
        projs
        |> Array.choose (fun proj ->
            match ( hasAValidName proj ) with
            | Some validName ->
                let deleteNuGetMsg =
                    Perform_Nuget_Action_To_Server(proj,validName,dispatch)
                let newStatus =
                    Loading_To_Nuget_Server_Alternatives.Executing_Nuget_Server_Command |>
                    (
                        Loading_Nuget_Info_Is_Not_Done >>
                        Loading_Info_To_Server
                    )
                 
                let newLoadingStatusMsg =
                    { proj with Loading_To_Server = newStatus} |>
                    (
                        Upgrade_NuGet.Types.Change_Project_Info 
                    )
                    |> turnIntoSendPopupWithNewState dispatch

                [|
                    newLoadingStatusMsg
                    deleteNuGetMsg
                |]
                |> Upgrade_NuGet.Types.Batch
                |> Some
            | _ -> None)
        |> function
            | res when (res |> Array.length) <> 0 ->
                res |> Some
            | _ -> None

    anyReadyMsgs

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
            |> turnIntoSendPopupWithNewState dispatch

        newLoadingStatusMsg
        )
    |> Upgrade_NuGet.Types.Batch

let nameToChose proj =
    match proj.Nuget_Names.New_Nuget_Name with
    | New_Nuget_Name.Has_New_Name newNameValidity ->
        match newNameValidity with
        | Nuget_Name_Validity.Nuget_Name_Valid newName ->
            newName
        | Nuget_Name_Validity.Nuget_Name_Not_Valid reason ->
            match reason with
            | Nuget_Already_In_Server newName ->
                newName
            | _ -> proj.Nuget_Names.CurrName
    | _ -> proj.Nuget_Names.CurrName

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
                nameToChose proj
            )
            |> Some
        | Server_Options.Is_To_Be_Updated ->
            String.Format(
                "{0} -> version {1} is about to be replaced from NuGet server",
                proj.Name,
                nameToChose proj
            )
            |> Some)
    |> Array.collect (fun projMsg ->
        Popup.View.getPopupMsg projMsg)

