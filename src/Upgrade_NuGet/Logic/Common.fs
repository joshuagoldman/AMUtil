module Upgrade_NuGet.Logic.Common

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

let turnIntoSendPopupWithNewState dispatch msg =
    (msg,dispatch)
    |> Send_Popup_With_New_State


let getBranches info =
    match info with
    | Upgrade_NuGet.Types.No_Git_Info_Nuget -> [|Html.none|]
    | Upgrade_NuGet.Types.Yes_Git_Info_Nuget repo ->
        repo.Branches
        |> Array.map (fun branch ->
            branch |> Rco_Update.View.branchAlt)

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
