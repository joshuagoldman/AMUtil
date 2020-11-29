module Upgrade_NuGet.Logic.GitBranchActions

open JsInterop
open Elmish
open Fable.Import
open System
open Upgrade_NuGet.Types
open Global.Types
open Feliz
open Fable.Core.JsInterop

let checkoutNewBranch ( newBranch : string ) dispatch positions = async{

    do! Async.Sleep 2000

    let prms =
        String.Format(
            "shellCommand=cd server;cd loganalyzer;git checkout {0}",
            newBranch
        )

    let! res = request prms

    match res.status with
    | 200.0 -> ()
            
    | _ ->
        let popupMsg =
            res.responseText |>
            (
                Popup.View.getPopupMsg >>
                Upgrade_NuGet.Logic.Common.checkingProcessPopupMsg positions
            )

        let exitMsg =
            "You've been thrown out due to some error. Please refresh to return"
            |> Popup.View.getPopupMsg

        let button =
            Popup.View.simpleOkButton
                            Upgrade_NuGet.Logic.Common.killPopupMsg
                            dispatch
                        
        let! kickedOutMsg =
            (button,exitMsg) |>
            (
                GlobalMsg.Go_To_Failed_Page >>
                Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                delayedMessage 3000
            )

        let msgsCombined =
            [|
                popupMsg
                kickedOutMsg
            |]

        msgsCombined
        |> Array.iter (fun msg -> msg |> dispatch)
}

let changeBranchNugetUpgrade model dispatch ( ev : Browser.Types.Event ) =
    match model.Info with
    | Yes_Git_Info_Nuget repo ->
        let branchName = ev.target?value |> string

        let msgs =
            [|
                (branchName, Global.Types.getPositions ev,dispatch) |>
                (
                     Change_Current_Branch_UpgradeNuget >>
                     Global.Types.AsynSyncMix.Is_Not_Async
                )

                Upgrade_NuGet.Logic.Common.handleBranchNameChange branchName repo
                |> Global.Types.AsynSyncMix.Is_Not_Async

                "Updating table after branch change..." |>
                (
                    Popup.View.getPopupMsgSpinner >>
                    Upgrade_NuGet.Logic.Common.checkingProcessPopupMsg Popup.Types.standardPositions >>
                    Global.Types.AsynSyncMix.Is_Not_Async
                )

                (dispatch,Global.Types.App_Activity.NugetUpgrade) |>
                (
                    Obtain_New_Nuget_Info >>
                    Global.Types.delayedMessage 2000 >>
                    Global.Types.AsynSyncMix.Is_Async
                )
                
                
            |]

        msgs
        |> Array.map (fun msg ->
            let asyncAction =
                async {
                    match msg with
                    | Global.Types.AsynSyncMix.Is_Async asynMsg ->
                        let! asyncToSync = asynMsg

                        asyncToSync |> dispatch
                    | Global.Types.AsynSyncMix.Is_Not_Async msg ->
                        msg |> dispatch
                }

            asyncAction)
        |> Array.iter (fun action -> action |> Async.StartImmediate)
            
    | _ -> ()