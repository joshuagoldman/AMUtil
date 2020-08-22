module Rco_Update.RcoFaultTable

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open Feliz
open Feliz.style

let prepareForNewRcoMsg ( ev : Browser.Types.Event ) fault rcoObjArr dispatch =
    let newVal = ev.target?value : string

    let newFaultObj =
        { fault with Correction = Correction(newVal)}

    newFaultObj |>
    (
        Types.Msg.Change_RCO_Fault_Arr >>
        dispatch
    )

let newRstatInInput fault rcoObjArr dispatch =
    Html.div[
        prop.className "field"
        prop.children[
            Html.div[
                prop.className "control"
                prop.children[
                    Html.input[
                        prop.className "input is-primary"
                        prop.type' "text"
                        prop.placeholder "Insert new Rstate-In value"
                        prop.onChange (fun ev ->
                            dispatch
                            |> prepareForNewRcoMsg
                                        ev
                                        fault
                                        rcoObjArr)
                        prop.value(
                            match fault.Correction with
                            | No_Correction _ -> ""
                            | Correction v -> v)
                    ]
                ]
            ]
        ]
    ]

let tableRow rcoObjArr dispatch ( fault : RcoFaultInfo ) =
    Html.tr[
        prop.children[
            Html.th[
                prop.text fault.Line
            ]
            Html.th[
                prop.text fault.LineInfo.RStateIn
            ]
            Html.th[
                newRstatInInput
                    fault
                    rcoObjArr
                    dispatch
            ]
        ]
    ]

let root ( faults : RcoFaultInfo[] ) rcoObjArr dispatch =

    Html.table[
        prop.className "table is-fullwidth is-scrollable"
        prop.style[
            Feliz.style.maxHeight 300
        ]
        prop.children[
            Html.thead[
                prop.children[
                    Html.tr[
                        prop.children[
                            Html.th[
                                prop.text "Row"
                            ]
                            Html.th[
                                prop.text "Rstate In"
                            ]
                            Html.th[
                                prop.text "New Rstate In"
                            ]
                        ]
                    ]
                ]
            ]
            Html.tbody[
                prop.children(
                    faults
                    |> Array.map (fun fault ->
                        fault
                        |> tableRow rcoObjArr dispatch)
                )
            ]
        ]
    ]

