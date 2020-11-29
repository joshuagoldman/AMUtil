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

let getServerActionButton ( option : Global.Types.TypeString<Server_Options>) =
    Html.option[
        prop.text option.ObjString
    ]

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

let serverActionChanged project ( ev : Browser.Types.Event ) dispatch =
    let name = ev.target?value : string

    currChoiceWithName
    |> Array.tryFind (fun o ->
        o.ObjString = name)
    |> function
        | res when res.IsSome ->
            (project,res.Value.ObjType) |>
            (
                Change_Server_Action_Option >>
                dispatch
            )
        | _ ->
            ()

let getActionsList project
                   ( arr : Global.Types.TypeString<Server_Options> [])
                   ( obj : Global.Types.TypeString<Server_Options> ) =

    let standardAnswer =
        arr
        |> Array.choose (fun opt ->
            match opt.ObjType with
            | Server_Options.Push_Nuget ->
                None
            | _ ->
                opt
                |> Some)

    match project.Nuget_Names.New_Nuget_Name with
    | New_Nuget_Name.Has_New_Name validity ->
        match validity with
        | Nuget_Name_Validity.Nuget_Name_Valid _ ->
            arr
        | _ ->
            standardAnswer
    | _ ->
        standardAnswer

let nugetServerOptionsViewItems project =
    match project.Server_Options with
    | Server_Options.No_Server_Actions ->
        {
            Global.Types.ObjType = Server_Options.No_Server_Actions
            Global.Types.ObjString = "No Actions"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )
    | Server_Options.Is_To_Be_Deleted ->
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Deleted
            Global.Types.ObjString = "Delete"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )
    | Server_Options.Is_To_Be_Updated ->
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Updated
            Global.Types.ObjString = "Replace"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )
    | Server_Options.Push_Nuget ->
        {
            Global.Types.ObjType = Server_Options.Push_Nuget
            Global.Types.ObjString = "Push"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )

let getNuGetTableInfo dispatch = async {

    let popupMsg =
        "Locating all relevant AM.LogAnalyzer projects..."
        |> Popup.View.getPopupMsgSpinner
        |> Upgrade_NuGet.Logic.Miscellaneous.checkingProcessPopupMsg Upgrade_NuGet.Logic.Miscellaneous.standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000

    let prms = [|
        {
            SharedTypes.CommandInfo.Command = "cd"
            SharedTypes.CommandInfo.Arg = "server"
        }
        {
            SharedTypes.CommandInfo.Command = "cd"
            SharedTypes.CommandInfo.Arg = "loganalyzer"
        }
        {
            SharedTypes.CommandInfo.Command = "ls"
            SharedTypes.CommandInfo.Arg = ""
        }
    |]

    let! responses = Global.Types.apis.Command prms

    let responsesAll =
        responses
        |> Array.map (fun resp ->
                resp.Answer
            )
        |> String.concat "\n"
    
    let regexStr = "(?<=Ericsson\.AM\.(?!sln))\w+(?!.*\.)"
    
    let matchesOpt = JsInterop.Regex.Matches regexStr responsesAll

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
        Upgrade_NuGet.Logic.Miscellaneous.kickedOutTemplate dispatch responsesAll

}

let updateOnly model dispatch =
    match model.Info with
    | Yes_Git_Info_Nuget _ ->

        let msgs =
            [|

                "Updating table..." |>
                (
                    Popup.View.getPopupMsgSpinner >>
                    Upgrade_NuGet.Logic.Miscellaneous.checkingProcessPopupMsg Popup.Types.standardPositions >>
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
