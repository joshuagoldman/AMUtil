module Upgrade_NuGet.RepoStateTable

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open Feliz
open Feliz.style

let currChoiceWithName =
    [|
        {
            Global.Types.ObjType = Server_Options.No_Server_Actions
            Global.Types.ObjString = "No Actions"
        }
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Deleted
            Global.Types.ObjString = "Delete"
        }
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Updated
            Global.Types.ObjString = "Replace"
        }
        {
            Global.Types.ObjType = Server_Options.Push_Nuget
            Global.Types.ObjString = "Push"
        }
    |]

let getServerActionButton project ( option : Global.Types.TypeString<Server_Options>) dispatch =
    Html.div[
        prop.className "dropdown-item"
        prop.children[
            Html.div[
                prop.className "button"
                prop.text option.ObjString
                prop.onClick (fun _ ->
                    (project,option.ObjType) |>
                    (
                        Change_Server_Action_Option >>
                        dispatch
                    ))
            ]
        ]
        
    ]

let getActionsList project
                   ( arr : Global.Types.TypeString<Server_Options> [])
                   ( obj : Global.Types.TypeString<Server_Options> ) =

        match project.Nuget_Names.New_Nuget_Name with
           | New_Nuget_Name.Has_New_Name validity ->
               match validity with
               | Nuget_Name_Validity.Nuget_Name_Valid _ ->
                   arr
                   |> Array.choose (fun opt ->
                       match (opt.ObjString = obj.ObjString) with
                       | true ->
                           None
                       | _ ->
                           opt
                           |> Some)
               | _ ->
                    match obj.ObjType with
                    | Push_Nuget ->
                       arr
                       |> Array.choose (fun opt ->
                           match opt.ObjType with
                           | Server_Options.Push_Nuget ->
                               None
                           | _ ->
                               match opt.ObjType with
                               | Server_Options.No_Server_Actions ->
                                   None
                               | _ ->
                                   opt
                                   |> Some)
                    | _ ->
                        arr
                        |> Array.choose (fun opt ->
                            match opt.ObjType with
                            | Server_Options.Push_Nuget ->
                                None
                            | _ ->
                                match (opt.ObjString = obj.ObjString) with
                                | true ->
                                    None
                                | _ ->
                                    opt
                                    |> Some)
            | _ ->
                arr
                |> Array.choose (fun opt ->
                    match opt.ObjType with
                    | Server_Options.Push_Nuget ->
                        None
                    | _ ->
                        match (opt.ObjString = obj.ObjString) with
                        | true ->
                            None
                        | _ ->
                            opt
                            |> Some)
        
let currentServerActionItem ( name : string ) =
    Html.span[
        prop.text name
    ]

let nugetServerOptionsViewItems project dispatch =
    match project.Server_Options with
    | Server_Options.No_Server_Actions ->
        {
            Global.Types.ObjType = Server_Options.No_Server_Actions
            Global.Types.ObjString = "No Actions"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option ->
                getServerActionButton
                                    project
                                    option
                                    dispatch)
        )
    | Server_Options.Is_To_Be_Deleted ->
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Deleted
            Global.Types.ObjString = "Delete"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option ->
                getServerActionButton
                                    project
                                    option
                                    dispatch)
        )
    | Server_Options.Is_To_Be_Updated ->
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Updated
            Global.Types.ObjString = "Replace"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option ->
                getServerActionButton
                                    project
                                    option
                                    dispatch)
        )
    | Server_Options.Push_Nuget ->
        {
            Global.Types.ObjType = Server_Options.Push_Nuget
            Global.Types.ObjString = "Push"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option ->
                getServerActionButton
                                    project
                                    option
                                    dispatch)
        )

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
        prop.className "dropdown is-active"
        prop.children[
            Html.div[
                prop.className "dropdown-trigger"
                prop.children[
                    Html.button[
                        prop.className "button"
                        prop.ariaHasPopup true
                        prop.ariaControls "dropdown-menu2"
                        prop.children[
                            nugetServerOptionsViewSelectedItem project
                            Html.span[
                                prop.className "icon is-small"
                                prop.children[
                                    Html.i[
                                        prop.className "fas fa-angle-down"
                                        prop.ariaHidden true
                                    ]
                                ]

                            ]
                        ]
                    ]
                ]
            ]
            Html.div[
                prop.className "dropdown-menu"
                prop.id "dropdown-menu2"
                prop.role.menu
                prop.children[
                    Html.div[
                        prop.className "dropdown-content"

                        dispatch |>
                        (
                            nugetServerOptionsViewItems project >>
                            prop.children
                        )
                        
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
            prop.className "table is-fullwidth is-scrollable is-striped"
            prop.style[
                Feliz.style.height 900
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

