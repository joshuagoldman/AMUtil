module Popup.Types

open Fable.React

type Popup_Button<'msg> = {
    Name : string
    Msg : 'msg
}

type PopupPosition = {
    PosX : float
    PosY : float
}

type Alternative_Popup_Otpions =
    | Simple_Ok of Button : ReactElement * Message : ReactElement[]
    | Several_Alternatives of Buttons : ReactElement[] * Message : ReactElement[]

type PopupStyle =
    | Popup_Is_Dead
    | Has_Alternatives of Alternative_Popup_Otpions * PopupPosition
    | Has_No_Alternatives of Message : ReactElement[] * PopupPosition
