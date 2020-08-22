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

let isUpdateRow ( project : Project_Info ) dispatch =
    let isChecked =
        match project.Is_Chosen with
        | Project_Chosen -> true
        | _ -> false

    match project.Nuget_Names.New_Nuget_Name with
    | New_Nuget_Name.Has_New_Name res ->
        match res with
        | Nuget_Name_Validity.Nuget_Name_Not_Valid reason ->
            match reason with
            | Not_Valid_Nuget_Reason.Has_Wrong_Pattern ->
                Html.div[
                    prop.text "NuGet version has wrong format."
                ]
            | Not_Valid_Nuget_Reason.Has_Same_Nuget_Name ->
                Html.div[
                    prop.style[
                        Feliz.style.maxWidth 200
                    ]
                    prop.text "Can't use same NuGet version name as the current."
                ]
            | Not_Valid_Nuget_Reason.Nuget_Already_In_Server ->
                Html.div[
                    prop.text "This name already exists in NuGet server."
                    prop.style[
                        Feliz.style.maxWidth 200
                    ]
                ]
        | Nuget_Name_Validity.Nuget_Name_Valid _ ->
            Html.label[
                prop.className "checkbox"
                prop.children[
                    Html.input[
                        prop.isChecked isChecked
                        prop.type'.checkbox
                        prop.onClick (fun _ ->
                            let newStatus =
                                Logic.changeNugetStatus project.Is_Chosen
                            { project with Is_Chosen = newStatus } |>
                            (
                        
                                Change_Project_Status >>
                                dispatch
                            )
                        )
                    ]
                ]
            ]
    | _ ->
        Html.div[
            prop.text "NuGet version has wrong format."
        ]

let newNugetNameInput project dispatch =

    Html.div[
        prop.className "field"
        prop.children[
            Html.html[
                prop.className "control"
                prop.children[
                    Html.input[
                        prop.className "input is-primary"
                        prop.style[
                            Feliz.style.color(
                                match project.Nuget_Names.New_Nuget_Name with
                                | New_Nuget_Name.Has_New_Name res ->
                                    match res with
                                    | Nuget_Name_Validity.Nuget_Name_Valid _ ->
                                        "black"
                                    | _ -> "red"
                                | _ -> "red"
                            )
                        ]
                        prop.type' "text"
                        prop.placeholder "Enter new NuGet version"
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
                isUpdateRow project dispatch
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
            prop.className "table is-fullwidth is-scrollable is-striped"
            prop.style[
                Feliz.style.height 300
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

