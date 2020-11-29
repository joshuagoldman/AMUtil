module Upgrade_NuGet.Logic.NugetTable

open Elmish
open Fable.Import
open System
open Upgrade_NuGet.Types
open Global.Types
open Feliz
open Fable.Core.JsInterop
open SharedTypes
open Fable.Remoting.Client

let getNuGetTableInfo dispatch = async {

    let popupMsg =
        "Locating all relevant AM.LogAnalyzer projects..."
        |> Popup.View.getPopupMsgSpinner
        |> Common.checkingProcessPopupMsg Upgrade_NuGet.Logic.Common.standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000
    
    let commandStr = "shellCommand=cd server;cd loganalyzer;ls"

    let! res = request commandStr

    match res.status with
    | 200.0 ->
        let regexStr = "(?<=Ericsson\.AM\.(?!sln))\w+(?!.*\.)"
        
        let matchesOpt = JsInterop.Regex.Matches regexStr res.responseText

        match matchesOpt with
        | Some matches ->
            let projectNotTest =
                matches
                |> Array.choose (fun matchStr ->
                    let isTestProjectOpt =
                        JsInterop.Regex.IsMatch ".Test" matchStr

                    match isTestProjectOpt with
                    | Some isTestProject ->
                        match isTestProject with
                        | true -> None
                        | _ -> matchStr |> Some
                    | _ -> None)
                |> Array.map (fun proj ->
                    {
                        Upgrade_NuGet.Types.Project_Name = proj
                        Upgrade_NuGet.Types.Loading_Msg = "loading info for " + proj + "."
                    }
                    |> Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Mix.Project_Loading)

            let getProjectsmsgs =
                matches
                |> Array.map (fun proj ->
                    (proj,dispatch) |>
                    (
                        Upgrade_NuGet.Types.Get_Project_Info >>
                        delayedMessage 2000
                    ))
                    |> Batch_Upgrade_Nuget_Async

            let changeStatusMsgWithPopuWrapped =
                (
                    projectNotTest |>
                    (
                        Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Status.Info_Is_Loading >>
                        Upgrade_NuGet.Types.Change_NuGet_Status
                    ),
                    dispatch
                )
                |> Send_Popup_With_New_State
                
            let msgs =
                [|
                    changeStatusMsgWithPopuWrapped
                    getProjectsmsgs
                |]

            msgs
            |> Array.iter (fun msg -> msg |> dispatch)

        | _ ->
            Upgrade_NuGet.Logic.Common.kickedOutTemplate dispatch res.responseText
    | _ ->
        Upgrade_NuGet.Logic.Common.kickedOutTemplate dispatch res.responseText

}

let updateOnly model dispatch =
    match model.Info with
    | Yes_Git_Info_Nuget _ ->

        let msgs =
            [|

                "Updating table..." |>
                (
                    Popup.View.getPopupMsgSpinner >>
                    Upgrade_NuGet.Logic.Common.checkingProcessPopupMsg Popup.Types.standardPositions >>
                    Global.Types.AsynSyncMix.Is_Not_Async
                )

                (dispatch,Global.Types.App_Activity.NugetUpgrade) |>
                (
                    Obtain_New_Nuget_Info >>
                    Global.Types.delayedMessage 2000 >>
                    Global.Types.AsynSyncMix.Is_Async
                )
                
            |]

        msgs
        |> Array.map (fun msg ->
            let asyncAction =
                async {
                    match msg with
                    | Global.Types.AsynSyncMix.Is_Async asynMsg ->
                        let! asyncToSync = asynMsg

                        asyncToSync |> dispatch
                    | Global.Types.AsynSyncMix.Is_Not_Async msg ->
                        msg |> dispatch
                }

            asyncAction)
        |> Array.iter (fun action -> action |> Async.StartImmediate)
            
    | _ -> ()

let updateNugetTable model dispatch =
    match model.Info with
    | Upgrade_NuGet.Types.Git_Info_Nuget.Yes_Git_Info_Nuget repo ->
        match model.Projects_Table with
        | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
            match res with
            |Loganalyzer_Projects_Table.Yes_Projects_Table_Info _ ->
                Html.div[
                    prop.className "column is-2"
                    prop.children[
                        Html.div[
                            prop.className "button"
                            prop.text "Update table"
                            prop.onClick (fun _ ->
                                updateOnly model dispatch)
                        ]
                    ]
                ]
            | _ -> Html.none
        | _ -> Html.none
    | _ -> Html.none
