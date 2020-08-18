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

let isUpdateRow project is_chosen dispatch =
    let isChecked =
        match is_chosen with
        | Project_Chosen -> true
        | _ -> false

    Html.label[
        prop.className "checkbox"
        prop.children[
            Html.input[
                prop.isChecked isChecked
                prop.type'.checkbox
                prop.onClick (fun _ ->
                    let newStatus =
                        Logic.changeNugetStatus is_chosen
                    { project with Is_Chosen = newStatus } |>
                    (
                        
                        Change_Project_Status >>
                        dispatch
                    )
                )
            ]
        ]
    ]

let newNugetNameInput project dispatch =
    let colorBasedOnCorrectNugetName =
        match project.Nuget_Names.New_Nuget_Name with
        | New_Nuget_Name.Has_New_Name res ->
            match res with
            | Nuget_Name_Valid ->
                "black"
            | _ -> "red"
        | _ -> "black"

    Html.div[
        prop.className "field"
        prop.children[
            Html.html[
                prop.className "control"
                prop.children[
                    Html.input[
                        prop.className "input is-primary"
                        prop.style[
                            Feliz.style.color colorBasedOnCorrectNugetName
                        ]
                        prop.type' "text"
                        prop.placeholder "Enter new NuGet name"
                        prop.onChange (fun ev ->
                            (project,ev) |>
                            (
                                Types.New_Nuget_Name_Change >>
                                dispatch
                            ))
                    ]
                ]
            ]
        ]
    ]

let tableRow project dispatch =
    Html.tr[
        prop.children[
            Html.th[
                prop.text project.Name
            ]
            Html.th[
                isUpdateRow project project.Is_Chosen dispatch
            ]
            Html.th[
                prop.text project.Nuget_Names.CurrName
            ]

            Html.th[
                newNugetNameInput project dispatch
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
                            Html.th[
                                prop.text "Project Name"
                            ]
                            Html.th[
                                prop.text "Update"
                            ]
                            Html.th[
                                prop.text "Current NuGet Name"
                            ]
                            Html.th[
                                prop.text "New NuGet Name"
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

