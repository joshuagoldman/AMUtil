module Main.Logic.DownloadRepo

open JsInterop
open Elmish
open Fable.Import
open System
open Main.Types
open Global.Types
open Feliz
open SharedTypes
open Fable.Remoting.Client
open Main

let apis =
    Remoting.createApi()
    |> Remoting.buildProxy<SharedTypes.IApis>

let downloadRepo dispatch = async {

    let popupMsg =
        "Downloading loganalyzer repo"
        |> Popup.View.getPopupMsgSpinner
        |> Logic.Common.checkingProcessPopupMsg Logic.Common.standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 3000

    let prms = [|
        {
            SharedTypes.CdCommand.MoveCommand = "server"
            SharedTypes.CdCommand.ResponseCommand = "git clone ssh://git@segaeesl01.eipu.ericsson.se:8081/nodetest/loganalyzer.gi"
        }
        |> IsCd
    |]

    let! responses = apis.Command prms

    let responsesCombined = 
        responses
        |> Array.map (fun response ->
                response.Answer
            )
        |> String.concat "/n"

    let repoClonedSuccesfully =
        "Make sure"
        |> responsesCombined.Contains
        |> not

    match repoClonedSuccesfully with
    | true ->
        let! checkParsingMsg =
            dispatch |>
            (
                CheckProcess.CheckProcessStarted >>
                Types.Check_Repo_Parsing_Msg >>
                delayedMessage 2000
            )

        let RepoIsClonedMsg =
            Git_Repo_Parsing_Options.Parsing_Has_Not_Been_Performed |>
            (
                Repository_Cloned >>
                CheckProcess.CheckProcessEnded >>
                Types.Check_If_Repo_Cloned_Msg
            )

        let msgCombined =
            [|
                checkParsingMsg
                RepoIsClonedMsg
            |]

        msgCombined
        |> Array.iter (fun msg -> msg |> dispatch )
        
    | false ->
        let popupMsg =
            responsesCombined
            |> Logic.Common.errorPopupMsg
                    dispatch
                    Logic.Common.killPopupMsg
                    Logic.Common.standardPositions

        let originNotAccessibleMsg =
            Git_Repo_Cloned_Result.Git_Error |>
            (
                CheckProcess.CheckProcessEnded >>
                Types.Check_If_Repo_Cloned_Msg
            )

        let msgsCombined =
            [|
                popupMsg
                originNotAccessibleMsg
            |]

        msgsCombined
        |> Array.iter (fun msg -> msg |> dispatch)
}