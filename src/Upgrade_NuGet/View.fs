module Upgrade_NuGet.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open Feliz
open Feliz.style
open System

let branchDropDown model dispatch =
    match model.Info with
    | Yes_Git_Info_Nuget _ ->
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
                                        Logic.changeBranchNugetUpgrade model dispatch ev)
                                    prop.id "inlineFormCustomSelect"
                                    prop.children(
                                        Logic.getBranches model.Info
                                        |> Array.append Rco_Update.View.standardBranchAlt    
                                    )
                                ]
                            ]
                        ]
                    ]
                ]

            ]
        ]
    | _ -> Html.none

let currentBranchInfo model =
    match model.Info with

    | Yes_Git_Info_Nuget info ->
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
    Logic.getTableLoadPopup model dispatch
    Html.div[
        prop.children[
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.style[
                            style.marginTop 30
                        ]
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
                    Logic.updateNugetTable model dispatch
                    Logic.saveChanges model dispatch
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.children[
                            match model.Projects_Table with
                            | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
                                RepoStateTable.root res dispatch
                            | _ -> Html.none
                        ]
                    ]
                ]
            ]
        ]
    ]

