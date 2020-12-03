module App.Types

open Main.Types

type Msg =
    | BatchAsync of Async<Msg> array
    | Global of Global.Types.GlobalMsg
    | MainMsg of  Main.Types.Msg
    | SocketMsg of SharedTypes.Shared.ClientMsg
    | AddDispatch of (Msg -> unit)

type Model = {
    Git : Global.Types.GitDecision
    CurrentPage : Global.Types.Page
    Main : Main.Types.Model
    VerifyStrMsg : Global.Types.VerifyFailedMsgOptions
    Popup : Popup.Types.PopupStyle
    Dispatch : (Msg -> unit) option
}
