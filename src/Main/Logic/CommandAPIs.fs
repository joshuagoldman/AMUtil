module Logic.CommandApis

open JsInterop
open Elmish
open Fable.Import
open System
open Main.Types
open Global.Types
open Feliz
open SharedTypes
open Fable.Remoting.Client
open Main.Logic
open Main

let apis =
    Remoting.createApi()
    |> Remoting.buildProxy<SharedTypes.IApis>

let isGitInstalledMsgsToDispatch dispatch 
                                ( firstResp : string ) = 
    Async.FromContinuations <| fun (resolve,_,_) ->
        let isInstalled =
                "bin/git"
                |> firstResp.Contains

        match isInstalled with
        | true ->

            let checkOriginAccessibilityMsg =
                dispatch |>
                (
                    CheckProcess.CheckProcessStarted >>
                    Main.Types.Msg.Check_Origin_Accessibility_Msg >>
                    Global.Types.delayedMessage 1000 >>
                    Async_Msg_Main
                )

            let resultGitInstalledMsg =
                Origin_Accessibility_Has_Not_Been_Checked |>
                (
                    Git_Installed >>
                    CheckProcess.CheckProcessEnded >>
                    Main.Types.Msg.Check_If_Git_Installed_Msg
                )

            let msgCombined =
                [|
                    checkOriginAccessibilityMsg
                    resultGitInstalledMsg
                |]
                |> Msg.Batch_Main

            msgCombined |> resolve
        | false ->
            match firstResp.Length > 0 with
            | true ->
                let resultGitInstalledMsg =
                    firstResp |>
                    (
                        Git_Installed_Result.Git_Installed_Result_Error  >>
                        CheckProcess.CheckProcessEnded >>
                        Main.Types.Msg.Check_If_Git_Installed_Msg
                    )

                resultGitInstalledMsg |> resolve
            | false ->
                let resultGitInstalledMsg =
                    Git_Installed_Result.Git_Not_Installed |>
                    (
                        CheckProcess.CheckProcessEnded >>
                        Main.Types.Msg.Check_If_Git_Installed_Msg
                    )

                resultGitInstalledMsg |> resolve

let checkIfGitInstalledAsync dispatch = async {
    let prms = [|
        {
            SharedTypes.CommandInfo.Command = "which"
            SharedTypes.CommandInfo.Arg = "git"
        }
    |]

    let! resp = apis.Command prms

    let firstResp = 
        resp
        |> Array.head
        |> fun x -> x.Answer

    let! msgs = isGitInstalledMsgsToDispatch dispatch firstResp

    return(msgs)
}

let checkOriginAccessibility dispatch = async{
    let popupMsg =
        "Checking accesibility to server"
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 3000

    let prms = [|
        {
            SharedTypes.CommandInfo.Command = "git"
            SharedTypes.CommandInfo.Arg = "ls-remote ssh://git@segaeesl01.eipu.ericsson.se:8081/nodetest/loganalyzer.git HEAD"
        }
    |]

    let! resp = apis.Command prms

    let firstResp = 
        resp
        |> Array.head
        |> fun x -> x.Answer


    let isOriginAccessible =
        "HEAD"
        |> firstResp.Contains

    match isOriginAccessible with
    | true ->

        let! repoExistsMsg =
            dispatch |>
            (
                CheckProcess.CheckProcessStarted >>
                Types.Check_If_Repo_Cloned_Msg >>
                Global.Types.delayedMessage 3000
            )

        let repoIsClonedMsg =
            Git_Repo_Cloned_Options.Repo_Cloned_Has_Not_Been_Checked |>
            (
                Origin_Access_Result.Origin_accessbile >>
                CheckProcess.CheckProcessEnded >>
                Types.Check_Origin_Accessibility_Msg
            )

        let msgCombined =
            [|
                repoExistsMsg
                repoIsClonedMsg
            |]

        msgCombined
        |> Array.iter (fun msg -> msg |> dispatch )
        
    | false ->

        let popupMsg =
            "Origin is not accessible!"
            |> errorPopupMsg
                    dispatch
                    killPopupMsg
                    standardPositions

        let originNotAccessibleMsg =
            Origin_Access_Result.Origin_Not_Accessible |>
            (
                CheckProcess.CheckProcessEnded >>
                Types.Check_Origin_Accessibility_Msg
            )

        let msgsCombined =
            [|
                popupMsg
                originNotAccessibleMsg
            |]

        msgsCombined
        |> Array.iter (fun msg -> msg |> dispatch)  
}

let checkRepositoryDownloaded dispatch = async{
    let popupMsg =
        "Checking if loganalyzer repo cloned"
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 3000

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
            SharedTypes.CommandInfo.Command = "git"
            SharedTypes.CommandInfo.Arg = "remote -v"
        }
    |]

    let! responses = apis.Command prms

    let responsesCombined = 
        responses
        |> Array.map (fun response ->
                response.Answer
            )
        |> String.concat "/n"

    let doesRepoExist =
        "ssh://git@segaeesl01.eipu.ericsson.se:8081/nodetest/loganalyzer.git"
        |> responsesCombined.Contains

    match doesRepoExist with
    | true ->
        let! checkParsingMsg =
            dispatch |>
            (
                CheckProcess.CheckProcessStarted >>
                Types.Check_Repo_Parsing_Msg >>
                Global.Types.delayedMessage 3000
            )

        let RepoIsClonedMsg =
            Git_Repo_Parsing_Options.Parsing_Has_Not_Been_Performed |>
            (
                Repository_Cloned >>
                Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked  >>
                Origin_Access_Result.Origin_accessbile >>
                CheckProcess.CheckProcessEnded >>
                Types.Check_Origin_Accessibility_Msg
            )

        let msgCombined =
            [|
                checkParsingMsg
                RepoIsClonedMsg
            |]

        msgCombined
        |> Array.iter (fun msg -> msg |> dispatch)
        
    | false ->

        let! startDownloadRepoMsg =
            dispatch |>
            (
                CheckProcess.CheckProcessStarted >>
                Types.Clone_Repo_Msg >>
                delayedMessage 2000
            )
            
        let repoNotClonedMsg =
            Git_Repo_Cloned_Result.Repository_Not_Cloned |>
            (
                CheckProcess.CheckProcessEnded >>
                Types.Check_If_Repo_Cloned_Msg
            )

        let msgCombined =
            [|
                startDownloadRepoMsg
                repoNotClonedMsg
            |]

        msgCombined
        |> Array.iter (fun msg -> msg |> dispatch )
}