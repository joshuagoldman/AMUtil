module Upgrade_NuGet.RepoStateTable

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open Feliz
open Feliz.style

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
                    ]
                ]
            ]
        ]
    ]

let tableRow project dispatch =
    Html.tr[
        prop.children[
            Html.th[
                prop.text ""
            ]
            Html.th[
                prop.text ""
            ]
            Html.th[
                prop.text ""
            ]
        ]
    ]

let root ( projects_table : Loganalyzer_Projects_Table ) dispatch =
    match projects_table with
    | Yes_Projects_Table_Info projects ->
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
                                    prop.text "Project Name"
                                ]
                                Html.th[
                                    prop.text "Create NuGet"
                                ]
                                Html.th[
                                    prop.text "Changes"
                                ]
                            ]
                        ]
                    ]
                ]
                Html.tbody[
                    prop.children(
                        projects
                        |> Array.map (fun project ->
                            tableRow project dispatch)
                    )
                ]
            ]
        ]
    | _ ->
        Html.none

