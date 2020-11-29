module Rco_Update.Logic.Miscellaneous

open JsInterop
open Elmish
open Fable.Import
open System
open Global.Types
open Feliz
open Fable.Remoting.Client
open SharedTypes

let alternaitvesPopup msg buttonSettings dispatch positions =

    let buttons = Popup.View.createButtons dispatch buttonSettings

    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let popupStyle =
        (buttons,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

    (popupStyle,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Rco_Update.Types.Global_Msg >>
        delayedMessage 3000
    )

let QuestionPopup msg yesMsg noMsg dispatch positions =

    let questionToAsk =
        msg
        |> Popup.View.getPopupMsg

    let yesNoButtons =
        Popup.View.yesNoButtons yesMsg noMsg dispatch
        |> List.toArray

    let popupStyle =
        (yesNoButtons,questionToAsk)
        |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

    (popupStyle,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Rco_Update.Types.Global_Msg >>
        delayedMessage 3000
    )

let errorPopupMsg msg msgToDispatch dispatch positions =
    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let simpleOkButton =
        Popup.View.simpleOkButton msgToDispatch dispatch

    let allComponentsSimleOk =
        (simpleOkButton,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok

    (allComponentsSimleOk,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Rco_Update.Types.Global_Msg >>
        delayedMessage 3000
    )

let errorPopupMsgNotAsync msg msgToDispatch dispatch positions =
    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let simpleOkButton =
        Popup.View.simpleOkButton msgToDispatch dispatch

    let allComponentsSimleOk =
        (simpleOkButton,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok

    (allComponentsSimleOk,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Rco_Update.Types.Global_Msg
    )

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Rco_Update.Types.Global_Msg
    )

let killPopupMsg =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Global.Types.Popup_Msg_Global >>
        Rco_Update.Types.Global_Msg
    )

let getWriteRCOInfo ( rcos : RCOTabs ) rcoType =
    match (rcoType : Rco_Update.Types.RCO_File_Type) with
    | Rco_Update.Types.RCO_File_Type.ERS -> rcos.ERS
    | Rco_Update.Types.RCO_File_Type.RBS_6000 -> rcos.RBS6000

let checkoutNewBranch ( newBranch : string ) dispatch positions = async{

    do! Async.Sleep 2000

    let prms = [|
        {
            SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
            SharedTypes.CdCommand.ResponseCommand =
                String.Format(
                    "checkout {0}",
                    newBranch
                )
        }
        |> IsCd
    |]

    let! responses = Global.Types.apis.Command prms

    let responsesAll =
        responses
        |> Array.map (fun resp ->
                resp.Answer
            )
        |> String.concat "\n"

    match (responsesAll.Contains("Switched to branch")) with
    | true -> ()
            
    | _ ->
        let popupMsg =
            responsesAll |>
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
                Rco_Update.Types.Global_Msg >>
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


   

