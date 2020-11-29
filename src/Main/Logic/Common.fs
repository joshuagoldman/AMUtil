module Main.Logic.Common

open JsInterop
open Elmish
open Fable.Import
open System
open Global.Types
open Feliz
open SharedTypes
open Fable.Remoting.Client

let standardPositions = {
        Popup.Types.PosX = 250.0
        Popup.Types.PosY = 250.0
    }

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
        delayedMessage 3000
    )

let errorPopupMsg dispatch msgToDispatch positions msg =
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
        Main.Types.Popup_Msg
    )

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Main.Types.Popup_Msg 
    )

let killPopupMsg =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Main.Types.Popup_Msg
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
            Main.Types.GlobalMsg_Main >>
            dispatch
        )

    kickedOutMsg

type GitUserDataType<'a> = {
    UserValue : 'a
    TypeName : string
}

type StringOrStringArray =
    | IsString of GitUserDataType<string option> 
    | IsStringArray of GitUserDataType<string[] option> 

type Commands = {
    ShellCommand : string
} 


    
