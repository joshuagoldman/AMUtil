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

let getTableLoadPopup model dispatch =
    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Is_Loading mix ->
        let allProjectsLoaded = Logic.haveProjectsBeenLoaded mix

        let allMsgs =
            mix
            |> Array.map (fun proj ->
                match proj with
                | Loganalyzer_Projects_Table_Mix.Project_Not_Loading proj_not_loading ->
                    match proj_not_loading with
                    | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull proj_not_loading ->
                        let msg =
                            String.Format(
                                "{0} -> Loading was successfull",
                                proj_not_loading.Name
                            )
                        msg
                    | Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull name ->
                        let msg =
                            String.Format(
                                "{0} -> Loading was not successfull",
                                name
                            )
                        msg

                | Loganalyzer_Projects_Table_Mix.Project_Loading proj_loading ->
                    let msg =
                        String.Format(
                            "{0} -> {1}",
                            proj_loading.Project_Name,
                            proj_loading.Loading_Msg
                        )

                    msg)

        match allProjectsLoaded with
        | true ->
            Logic.cretateLoadingFinishedPopup allMsgs dispatch
        | _ ->
            Logic.cretateLoadingPopup allMsgs dispatch
            
    | _ -> ()

let root model dispatch =
    getTableLoadPopup model dispatch
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

