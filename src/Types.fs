module App.Types

open Main.Types

type Msg =
    | Global of Global.Types.GlobalMsg
    | MainMsg of  Main.Types.Msg
    | SocketMsg of SharedTypes.Shared.BridgeMsg

type Model = {
    Git : Global.Types.GitDecision
    CurrentPage : Global.Types.Page
    Main : Main.Types.Model
    VerifyStrMsg : Global.Types.VerifyFailedMsgOptions
    Popup : Popup.Types.PopupStyle
    Socket : SharedTypes.Shared.BridgeModel
}
