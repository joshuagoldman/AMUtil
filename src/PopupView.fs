module Popup.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Feliz
open Feliz.style
open Fable.React
open Fable.React.Props
open Popup.Types

let getPopupMsgProgress ( msg : string ) ( percentage : float ) =
    [|
        Html.div[
            prop.className "columns is-centered"
            prop.style[
                Feliz.style.margin 10
            ]
            prop.children[
                Html.div[
                    prop.className "column"
                    prop.text msg
                ]
            ]
        ]
        Html.div[
            prop.className "columns is-centered"
            prop.children[
                Html.div[
                    prop.className "column is-1"
                    prop.style[
                        Feliz.style.margin 10
                    ]
                    prop.children[
                        Html.i[
                            prop.className "fa fa-cog fa-spin fa-2x"
                        ]
                    ]
                ]
            ]
        ]
        Html.div[
            prop.className "columns is-centered"
            prop.children[
                Html.div[
                    prop.className "column"
                    prop.style[
                        Feliz.style.margin 10
                    ]
                    prop.children[
                        Html.progress[
                            prop.className "progress is-primary"
                            prop.value percentage
                            prop.max 100
                        ]
                    ]
                ]
            ]
        ]
    |]

let getPopupMsgSpinner ( msg : string ) =
    [|
        Html.div[
            prop.className "columns is-centered"
            prop.style[
                Feliz.style.margin 10
            ]
            prop.children[
                Html.div[
                    prop.className "column"
                    prop.text msg
                ]
            ]
        ]
        Html.div[
            prop.className "columns is-centered"
            prop.children[
                Html.div[
                    prop.className "column is-1"
                    prop.style[
                        Feliz.style.margin 10
                    ]
                    prop.children[
                        Html.i[
                            prop.className "fa fa-cog fa-spin fa-2x"
                        ]
                    ]
                ]
            ]
        ]
    |]

let getPopupMsgSpinnerBeside ( msg : string ) =
    [|
        Html.div[
            prop.className "columns is-centered"
            prop.style[
                Feliz.style.margin 10
            ]
            prop.children[
                Html.div[
                    prop.className "column is-4"
                    prop.text msg
                    prop.style[
                        Feliz.style.fontSize 15
                    ]
                ]
                Html.div[
                    prop.className "column is-4"
                    prop.style[
                        Feliz.style.margin 10
                    ]
                    prop.children[
                        Html.i[
                            prop.className "fa fa-cog fa-spin fa-2x"
                        ]
                    ]
                ]
            ]
        ]
    |]

let getPopupMsg ( msg : string ) =
    [|
        Html.div[
            prop.className "columns is-centered"
            prop.style[
                Feliz.style.margin 10
            ]
            prop.children[
                Html.div[
                    prop.className "column"
                    prop.text msg
                ]
            ]
        ]
    |]

let createButtons dispatch buttons =
    buttons
    |> Array.map (fun button ->
        Html.div[
            prop.className "column"
            prop.style[
                Feliz.style.margin 10
            ]
            prop.children[
                Html.div[
                    prop.className "button"
                    prop.text button.Name
                    prop.onClick (fun _ ->
                        button.Msg |> dispatch)
                ]
            ]
        ]
    )

let simpleOkButton msg dispatch =
    Html.div[
        prop.children[
            Html.div[
                prop.className "button"
                prop.text "OK"
                prop.onClick (fun _ -> msg |> dispatch)
            ]
        ]
    ]

let yesNoButtons yesMsg noMsg dispatch =
    [
        Html.div[
            prop.className "column is-2"
            prop.style[
                Feliz.style.margin 10
            ]
            prop.children[
                Html.div[
                    prop.className "button"
                    prop.text "Yes"
                    prop.onClick (fun _ -> yesMsg |> dispatch )
                ]
            ]
        ]
        Html.div[
            prop.className "column is-2"
            prop.style[
                Feliz.style.margin 10
            ]
            prop.children[
                Html.div[
                    prop.className "button"
                    prop.text "No"
                    prop.onClick (fun _ -> noMsg |> dispatch)
                ]
            ]
        ]
    ]

let popup_view popup_Style =
    let standardPopupAppearance position =
        [
            Feliz.style.zIndex 1
            Feliz.style.left ( position.PosX |> int )
            Feliz.style.top ( position.PosY |> int )
            Feliz.style.position.absolute
            Feliz.style.backgroundColor "white"
            Feliz.style.fontSize 18
            Feliz.style.borderRadius 20
            Feliz.style.margin 5
        ]
    match popup_Style with
    | Popup_Is_Dead ->
        Html.none
    | Has_Alternatives(options,position) ->
        
        match options with
        | Simple_Ok(button,msgs) ->
            Html.div[
                position |>
                (
                    standardPopupAppearance >>
                    prop.style
                )
                prop.children(
                    Html.div[
                        prop.className "columns is-centered"
                        prop.children[
                            Html.div[
                                prop.className "column is-1"
                                prop.style[
                                    Feliz.style.margin 10
                                ]
                                prop.children[
                                    button
                                ]
                                
                            ]
                        ]
                    ]
                    |> fun x -> [|x|]
                    |> Array.append msgs
                )
            ]
        | Several_Alternatives(buttons,msg) ->
            Html.div[
                position |>
                (
                    standardPopupAppearance >>
                    prop.style
                )

                Html.div[
                    prop.className "columns is-centered"
                    prop.children buttons
                ] |>
                (
                    (fun x -> [|x|]) >>
                    Array.append msg >>
                    prop.children 
                )

            ]
    | Has_No_Alternatives(msgs,position) ->
        Html.div[
            position |>
            (
                standardPopupAppearance >>
                prop.style
            )
            prop.children(
                Html.div[
                    prop.className "columns is-centered"
                ]
                |> fun x -> [|x|]
                |> Array.append msgs
            )
            
        ]

let generalPopupCreation ( utils : Popup_Utils<Global.Types.GlobalMsg,'a> ) =

    let buttons = createButtons utils.Dispatch utils.Buttons_With_Msg

    let reactElementMsgs =
        utils.Str_Msg
        |> getPopupMsg

    let popupStyle =
        (buttons,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

    (popupStyle,Global.Types.getPositions utils.positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        utils.global_msg
    )

let checkingProcessPopupMsg positions  ( msgType : Global.Types.GlobalMsg -> 'a  ) msg  =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        msgType
    )

let questionButton yesMsg noMsg =
    [|
        {
            Popup.Types.Name = "Yes"
            Popup.Types.Msg = yesMsg
        }
        {
            Popup.Types.Name = "No"
            Popup.Types.Msg = noMsg
        }
    |]

