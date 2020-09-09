module Criteria_Changes.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Feliz
open Feliz.style
open Types

let fileUpload model dispatch =
    Html.form[
        prop.children[
            Html.div[
                prop.className "form-group"
                prop.style[
                    style.margin 40
                ]
                prop.children[
                    Html.label[
                        prop.for' "exampleFormControlFile1"
                        prop.style[
                            Feliz.style.color "black"
                            style.fontWeight.bold
                            Feliz.style.fontSize 18
                        ]
                        prop.text "Choose RCO List file"

                    ]
                    Html.input[
                        prop.type'.file
                        prop.style[
                            Feliz.style.color "black"
                            style.fontWeight.bold
                            Feliz.style.fontSize 16
                        ]
                        prop.onChange ( fun ev ->
                            ev |>
                            (
                                Logic.changeFileHandle dispatch >>
                                Array.iter (fun msg -> msg |> dispatch)
                            ))
                        prop.className "form-control-file"
                    ]
                ]
            ]
        ]
    ]

let branchAlt ( name : string ) =
    Html.option[
        prop.text name
        prop.style[
            Feliz.style.backgroundColor "violet"
            Feliz.style.color "black"
            fontWeight.bold
        ]
    ]

let releaseOptions ( infos : Log_Search_Criteria_Info [] ) =
    infos
    |> Array.map (fun info ->
        info.Excel_Info.Released |> branchAlt)

let chooseReleaseDropdown model dispatch =
    match model.ExcelInfo with
    | Yes_Rel_Plan_Log_Analysis(infos,_) ->
        Html.div[
            prop.className "field"
            prop.children[
                Html.div[
                    prop.className "control"
                    prop.children[
                        Html.div[
                            prop.className "select is-rounded is-primary"
                            prop.children[
                                Html.select[
                                    prop.style[
                                        style.backgroundColor.azure
                                    ]
                                    prop.onChange ( fun ev ->
                                        Logic.changeReleaseInfoToShow ev infos dispatch)
                                    prop.id "inlineFormCustomSelect"

                                    releaseOptions infos
                                    |> prop.children
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    | _ -> Html.none

let getNewInfoButton model dispatch =
    match model.CurrFile with
    | Curr_Rel_Plan_Log_Analysis_File.Yes_Log_Analysis_File file ->
        Html.div[
            prop.className "button"
            prop.text "Get Excel Info"
            prop.onClick (fun _ ->
                let msg =
                    Logic.parseCriteriaChangesFile file dispatch
                    |> Types.Async_Msg_Criteria_Changes

                msg |> dispatch
                )
        ]
    | _ ->
        Html.none
    
let root model dispatch =
    Html.div[
        prop.children[
            Html.div[
                prop.className "columns is-centered"
                prop.style[
                    style.marginTop 20
                ]
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.children[
                            fileUpload model dispatch
                        ]
                    ]
                    Html.div[
                        prop.className "column"
                        prop.children[
                            getNewInfoButton model dispatch
                        ]
                    ]
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.children[
                            chooseReleaseDropdown model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
