module Main.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open Feliz
open Feliz.style

open Fable.React
open Fable.React.Props

type OptionPageWName = {
    Activity : Global.Types.App_Activity
    Activity_name : string
}

let activityChangeDispatch model activity dispatch =
    match activity with
    | Global.Types.App_Activity.NugetUpgrade _ ->
        match model.Upgrade_NuGet.Projects_Table with
        | Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Status.Info_Not_Loaded ->
            (Main.Types.Upgrade_NuGet_Msg >> dispatch,activity) |>
            (
                Upgrade_NuGet.Types.Obtain_New_Nuget_Info >>
                Main.Types.Upgrade_NuGet_Msg
            )
        | Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
            match res with
            | Upgrade_NuGet.Types.Loganalyzer_Projects_Table.No_Projects_Table_Info -> 
                (Main.Types.Upgrade_NuGet_Msg >> dispatch,activity) |>
                (
                    Upgrade_NuGet.Types.Obtain_New_Nuget_Info >>
                    Main.Types.Upgrade_NuGet_Msg
                )
            | _ ->
                activity
                |> Change_Activity
        | _ ->
            activity
            |> Change_Activity
    | _ ->
        activity
        |> Change_Activity

let dropDownOptionItem model dispatch ( appOption : OptionPageWName )=
    Html.div[
        prop.className "dropdown-item"
        prop.children[
            Html.div[
                prop.className "button"
                prop.text appOption.Activity_name
                prop.onClick (fun _ ->
                    activityChangeDispatch model appOption.Activity dispatch
                    |> dispatch
                )
                prop.style[
                    Feliz.style.color "black"
                    Feliz.style.margin 5
                    fontWeight.bold
                    Feliz.style.backgroundImage "linear-gradient(to bottom, green, white)"
                ]
            ]
        ]
    ]
    

let listOfPages =
    [|
        { Activity = Global.Types.App_Activity.Activity_None ; Activity_name = "Home" }
        { Activity = Global.Types.App_Activity.RCOUpdate ; Activity_name = "Update RCO List" }
        { Activity = Global.Types.App_Activity.NugetUpgrade ; Activity_name = "Upgrade NuGet" }
        { Activity = Global.Types.App_Activity.Criteria_Changes ; Activity_name = "Criteria Changes Sprint" }
    |]
    

let menuButton model dispatch =
    Html.div[
        prop.className "dropdown is-hoverable"
        prop.children[
            Html.div[
                prop.className "dropdown-trigger"
                prop.children[
                    Html.div[
                        prop.children[
                            Html.div[
                                prop.className "button"
                                prop.ariaHasPopup true
                                prop.ariaControls "dropdown-menu4"
                                prop.style[
                                    Feliz.style.backgroundImage "linear-gradient(to bottom, green, white)"
                                    Feliz.style.color "black"
                                    fontWeight.bold
                                ]
                                prop.children[
                                    Html.span "Options"
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
                ]
            ]
            Html.div[
                prop.className "dropdown-menu"
                prop.style[
                    Feliz.style.backgroundColor "black"
                ]
                prop.role.menu
                prop.children[
                    Html.div[
                        prop.className "dropdown-content"
                        prop.style[
                            Feliz.style.backgroundImage "linear-gradient(to bottom, violet, black)"
                        ]
                        prop.children(
                            listOfPages
                            |> Array.map (fun optionName ->
                                dropDownOptionItem model dispatch optionName)
                        )
                    ]
                ]
            ]
        ]
    ]

let homePage =
    Html.div[
        prop.className "column is-3"
        prop.text "Welcome to RCO-Handler!"
        prop.style[
            Feliz.style.marginTop 150
            Feliz.style.marginLeft 10
            Feliz.style.color "white"
            Feliz.style.fontSize 23
        ]
    ]

let root ( model : Model ) dispatch =
    Html.div[
        prop.children[
            Html.div[
                prop.className "columns"
                prop.style[
                    Feliz.style.backgroundColor "black"
                    Feliz.style.opacity 0.9
                    Feliz.style.minHeight 70
                    Feliz.style.borderColor "red"
                    borderStyle.initial
                ]
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.style[
                            Feliz.style.marginTop 10
                            Feliz.style.marginLeft 10
                        ]
                        prop.children[
                            menuButton model dispatch
                        ]
                    ]
                    Html.div[
                        prop.className "column is-7"
                        prop.text "RCO-Handler"
                        prop.style[
                            Feliz.style.marginTop 10
                            Feliz.style.marginLeft 10
                            Feliz.style.fontSize 23
                            Feliz.style.color "green"
                        ]
                    ]  
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.style[
                    Feliz.style.backgroundImage "linear-gradient(to bottom, green, white)"
                    backgroundPosition.fixedNoScroll
                    Feliz.style.minHeight 600
                ]
                prop.children[
                    match model.Activity with
                    | Global.Types.Activity_None ->
                        homePage
                    | Global.Types.RCOUpdate ->
                        Rco_Update.View.root model.Rco_Update (Types.Msg.Rco_Update_Msg >> dispatch)
                    | Global.Types.NugetUpgrade ->
                        Upgrade_NuGet.View.root model.Upgrade_NuGet (Types.Msg.Upgrade_NuGet_Msg >> dispatch)
                    | Global.Types.Criteria_Changes ->
                        Criteria_Changes.View.root model.Criteria_Changes (Types.Msg.Criteria_Changes_Msg >> dispatch)
                ]
            ] 
        ]
    ]
