module App.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open App.State
open Feliz
open Feliz.style
open Fable.React
open Elmish.Bridge

importAll "../sass/main.sass"

let emitCheckGitInstalledMsg ( dispatch : Msg -> unit ) =
    let newDispatch =
        Types.MainMsg >> dispatch

    let msg =
        newDispatch |>
        (
            Global.Types.CheckProcessStarted >>
            Main.Types.Msg.Check_If_Git_Installed_Msg >>
            Global.Types.delayedMessage 3000 >>
            Main.Types.Async_Msg_Main >>
            Types.MainMsg
        )

    msg
    
let mainPage model ( dispatch : Msg -> unit ) =
    match model.Git with
    | Global.Types.Git_Installed_Check_Performed result->
        match result with
        | Global.Types.Git_Installed_Result.Git_Not_Installed ->
            [
                Html.p "Git is not installed on this computer"
                Html.br[]
                Html.a[
                    prop.style[
                        Feliz.style.marginLeft 0
                        Feliz.style.whitespace.nowrap
                    ]
                    prop.href "https://gitforwindows.org/"
                    prop.children[
                        str "link to Git download"
                    ]
                ]
            ]
            |> Global.View.verifyFailedPage
        | Global.Types.Git_Installed_Result.Git_Installed_Result_Error error ->
            seq[
                Html.p(error)
                Html.br[]
                Html.a[
                    prop.style[
                        Feliz.style.marginLeft 0
                        Feliz.style.whitespace.nowrap
                    ]
                ]
            ]
            |> Global.View.verifyFailedPage
        
        | Global.Types.Git_Installed_Result.Git_Installed options ->
            match options with
            | Global.Types.Origin_Accessibility_Has_Been_Checked result ->
                match result with
                | Global.Types.Origin_Access_Result.Origin_accessbile opt ->
                    match opt with
                    | Global.Types.Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked result ->
                        match result with
                        | Global.Types.Git_Repo_Cloned_Result.Repository_Cloned opt ->
                            match opt with
                            | Global.Types.Parsing_Has_Been_Performed result ->
                                match result with
                                | Global.Types.Git_Repo_Parsing_Result.Parsing_Succeded ->
                                    Main.View.root model.Main (Types.Msg.MainMsg >> dispatch)
                                | Global.Types.Git_Repo_Parsing_Result.Parsing_Failed _ ->
                                    [Html.none]
                                    |> Global.View.verifyFailedPage
                            | Global.Types.Parsing_Has_Not_Been_Performed ->
                                [Html.none]
                                |> Global.View.verifyFailedPage
                        | Global.Types.Git_Repo_Cloned_Result.Repository_Not_Cloned ->
                            [Html.none]
                            |> Global.View.verifyFailedPage
                        | Global.Types.Git_Repo_Cloned_Result.Git_Error ->
                            [Html.none]
                            |> Global.View.verifyFailedPage
                    | Global.Types.Git_Repo_Cloned_Options.Repo_Cloned_Has_Not_Been_Checked ->
                        [Html.none]
                        |> Global.View.verifyFailedPage
                | Global.Types.Origin_Access_Result.Origin_Not_Accessible ->
                    [Html.none]
                    |> Global.View.verifyFailedPage
                    
            | Global.Types.Origin_Accessibility_Has_Not_Been_Checked ->
                [Html.none]
                |> Global.View.verifyFailedPage

    | Global.Types.Git_Installed_Check_Not_Performed ->
        let checkingGitMsg =
                Global.Types.GitDecision.Git_Installed_Check_Performing |>
                (
                    Global.Types.Change_Git_Installed_Status_Msg >>
                    Global
                )

        let msgs = [|
            checkingGitMsg
            emitCheckGitInstalledMsg dispatch
        |]

        msgs
        |> Array.iter (fun msg -> msg |> dispatch)

        Global.View.verifyPage

    | Global.Types.Git_Installed_Check_Performing ->
        Global.View.verifyPage

let root (model: Model) dispatch =
    Html.div[
        prop.children[
            mainPage model dispatch
            Popup.View.popup_view model.Popup
        ]
    ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update root
|> Program.toNavigable (parseHash pageParser) urlUpdate
|> Program.withBridge SharedTypes.Shared.endpoint
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
