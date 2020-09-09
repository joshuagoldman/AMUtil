module Criteria_Changes.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Global.Types
open Feliz

let tu =
    let ArgsRegex = "(?<=Arg(.|\n)*(is\s|=))(.|\n)*?(?=((,|\n)(\s|\n)*Arg|\sand))"

    ArgsRegex

let killPopupMsg  =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg_Criteria_Changes
    )

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg_Criteria_Changes
    )

let checkoutNewBranch ( newBranch : string ) dispatch positions = async{

    do! Async.Sleep 2000

    let prms =
        String.Format(
            "shellCommand=cd server;cd loganalyzer;git checkout {0}",
            newBranch
        )

    let! res = request prms

    match res.status with
    | 200.0 ->
        newBranch |> 
        (
            Types.Update_Current_Branch_Name >>
            dispatch
        )
            
    | _ ->
        let popupMsg =
            res.responseText |>
            (
                Popup.View.getPopupMsg >>
                checkingProcessPopupMsg positions
            )

        let exitMsg =
            "You've been thrown out due to some error. Please refresh to return"
            |> Popup.View.getPopupMsg

        let button =
            Popup.View.simpleOkButton
                            killPopupMsg
                            dispatch
                        
        let! kickedOutMsg =
            (button,exitMsg) |>
            (
                GlobalMsg.Go_To_Failed_Page >>
                Types.Global_Msg_Criteria_Changes >>
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

