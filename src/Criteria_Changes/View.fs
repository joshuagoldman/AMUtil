module Criteria_Changes.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Feliz
open Feliz.style
open Types

let standardBranchAlt =
    [|
        Html.option[
            prop.selected true
            prop.text "Choose Branch..."
            prop.style[
                Feliz.style.backgroundColor "violet"
                Feliz.style.color "black"
                fontWeight.bold
            ]
        ]
    |]

let branchAlt ( name : string ) =
    Html.option[
        prop.text name
        prop.style[
            Feliz.style.backgroundColor "violet"
            Feliz.style.color "black"
            fontWeight.bold
        ]
    ]

let getBranches model dispatch =
    match model.Info with
    | No_Git_Info_Criteria_Changes -> [|Html.none|]
    | Yes_Git_Info_Criteria_Changes repo ->
        repo.Branches
        |> Array.map (fun branch ->
            branch |> branchAlt)

let handleBranchNameChange ( ev : Browser.Types.Event ) dispatch =
    let branchName = ev.target?value |> string

    match branchName with
    | "Choose Branch..." ->
        ()
    | _ ->
        let positions =
            {
                Popup.Types.PosX = ( ev?pageX : float )
                Popup.Types.PosY = ( ev?pageY : float )
            }
        [|(branchName,positions,dispatch) |> Types.Msg.Change_Current_Branch_Criteria_Changes|]
        |> Array.iter (fun msg -> msg |> dispatch)

let branchDropDown model dispatch =
    match model.Info with
    | Yes_Git_Info_Criteria_Changes _ ->
        Html.form[
            prop.children[
                Html.div[
                    prop.className "form-row align-items-center"
                    prop.children[
                        Html.div[
                            prop.className "col-auto my-1"
                            prop.children[
                                Html.label[
                                    prop.className "mr-sm-2 sr-only"
                                    prop.for' "inlineFormCustomSelect"
                                    prop.text "Preference"
                                ]
                                Html.select[
                                    prop.className "custom-select mr-sm-2"
                                    prop.style[
                                        Feliz.style.backgroundColor "violet"
                                        Feliz.style.color "black"
                                        fontWeight.bold
                                    ]
                                    prop.onChange ( fun ev ->
                                        dispatch
                                        |> handleBranchNameChange ev)
                                    prop.id "inlineFormCustomSelect"
                                    prop.children(
                                        getBranches model dispatch
                                        |> Array.append standardBranchAlt    
                                    )
                                ]
                            ]
                        ]
                    ]
                ]

            ]
        ]
    | _ -> Html.none

let currentBranchInfo ( model : Types.Model ) =
        match model.Info with
        | Yes_Git_Info_Criteria_Changes info ->
            Html.div[
                prop.text ("Current branch: " + info.CurrBranch)
                prop.style[
                    style.fontSize 20
                    style.fontWeight.bold
                    style.color "black"
                ]
            ]
        | _ -> Html.none
    
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
                            branchDropDown model dispatch
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
                            currentBranchInfo model
                        ]
                    ]
                ]
            ]
        ]
    ]
