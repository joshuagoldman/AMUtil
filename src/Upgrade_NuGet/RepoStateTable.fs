module Upgrade_NuGet.RepoStateTable

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open Feliz
open Feliz.style
      
let currentServerActionItem ( name : string ) =
    Html.option[
        prop.selected true
        prop.text name
    ]

let nugetServerOptionsViewSelectedItem project =
    let spanWhenNugetNameInvalid = 
        match project.Server_Options with
        | Server_Options.No_Server_Actions ->
            currentServerActionItem "No Actions"
        | Server_Options.Is_To_Be_Deleted ->
            currentServerActionItem "Delete"
        | Server_Options.Is_To_Be_Updated ->
            currentServerActionItem "Replace"
        | Server_Options.Push_Nuget ->
            currentServerActionItem "No Actions"

    match project.Nuget_Names.New_Nuget_Name with
    | New_Nuget_Name.Has_New_Name validity ->
        match validity with
        | Nuget_Name_Validity.Nuget_Name_Valid _ ->
            match project.Server_Options with
            | Server_Options.No_Server_Actions ->
                currentServerActionItem "No Actions"
            | Server_Options.Is_To_Be_Deleted ->
                currentServerActionItem "Delete"
            | Server_Options.Is_To_Be_Updated ->
                currentServerActionItem "Replace"
            | Server_Options.Push_Nuget ->
                currentServerActionItem "Push"
        | _ ->
            spanWhenNugetNameInvalid
    | _ -> 
        spanWhenNugetNameInvalid

let nugetServerOptionsView project dispatch =
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
                                    style.backgroundColor.saddleBrown
                                    style.color.white
                                ]
                                prop.onChange (fun ev ->
                                    Logic.serverActionChanged project ev dispatch)
                                Logic.nugetServerOptionsViewItems project |>
                                (
                                    prop.children
                                )
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]
let isInfoRow ( project : Project_Info ) =

    match project.Nuget_Names.New_Nuget_Name with
    | New_Nuget_Name.Has_New_Name res ->
        match res with
        | Nuget_Name_Validity.Nuget_Name_Not_Valid reason ->
            match reason with
            | Not_Valid_Nuget_Reason.Has_Wrong_Pattern ->
                Html.div[
                    prop.text "NuGet version has wrong format."
                ]
            | Not_Valid_Nuget_Reason.Nuget_Already_In_Server _ ->
                Html.div[
                    prop.text "This name already exists in NuGet server."
                    prop.style[
                        Feliz.style.maxWidth 200
                    ]
                ]
        | Nuget_Name_Validity.Nuget_Name_Valid _ ->
            Html.div[
                prop.text "Change option to 'Push Nuget' if you'd like to push NuGet to server!"
                prop.style[
                    Feliz.style.maxWidth 200
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
                            [|

                                (project,project.Server_Options)
                                |> Types.Change_Server_Action_Option

                                (project,ev) |>
                                (
                                    Types.New_Nuget_Name_Change
                                )
                            |]
                            |> Array.iter (fun msg -> msg |> dispatch)
                            )
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
                isInfoRow project
            ]
            Html.th[
                nugetServerOptionsView project dispatch
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
            prop.className "table is-fullwidth"
            prop.style[
                Feliz.style.height 300
                style.backgroundColor.beige
            ]
            prop.children[
                Html.thead[
                    prop.children[
                        Html.tr[
                            Html.th[
                                prop.text "Project Name"
                            ]
                            Html.th[
                                prop.text "Info"
                            ]
                            Html.th[
                                prop.text "Options"
                            ]
                            Html.th[
                                prop.text "Current NuGet Version"
                            ]
                            Html.th[
                                prop.text "New NuGet Version"
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

