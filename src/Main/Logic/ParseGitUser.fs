module Main.Logic.ParseGitUser

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

let popupKillMsg =
        Popup.Types.PopupStyle.Popup_Is_Dead |>
        (
            Types.Popup_Msg >>
            delayedMessage 1000
        )

let usrName responsesCombined =
        responsesCombined
        |> Regex.Match "\w.*"
        |> fun res ->
            {
                Logic.Common.UserValue = res
                Logic.Common.TypeName = "user name"
            }

let email responsesCombined =
    responsesCombined
    |> Regex.Matches "\w.*"
    |> function
        | res when res.IsSome ->
            if res.Value.Length > 1
            then 
                res.Value
                |> Array.item 1
                |> Some
                |> fun res ->
                    {
                        Logic.Common.UserValue = res
                        Logic.Common.TypeName = "email"
                    }
            else
                {
                    Logic.Common.UserValue = None
                    Logic.Common.TypeName = "email"
                }
        | _ ->
            {
                Logic.Common.UserValue = None
                Logic.Common.TypeName = "email"
            }

let branches responsesCombined =
    responsesCombined
    |> Regex.Matches "(?<=origin\/).*"
    |> fun res ->
        {
            Logic.Common.UserValue = res
            Logic.Common.TypeName = "branches"
        }

let currBranch responsesCombined =
    responsesCombined
    |> Regex.Match "(?<=Your branch is up to date with\s+'origin\/).*(?=\')|(?<=On branch\s+).*"
    |> fun res ->
        {
            Logic.Common.UserValue = res
            Logic.Common.TypeName = "branch"
        }

let getMessages responseText dispatch infoNotParsedCorrectlyRes =
    match infoNotParsedCorrectlyRes with
    | Some infoNotParsedCorrectly ->
            let errorText =
                infoNotParsedCorrectly
                |> Array.map (fun info ->
                    match info with
                    | Logic.Common.StringOrStringArray.IsString x ->
                        x.TypeName
                    | Logic.Common.StringOrStringArray.IsStringArray x ->
                        x.TypeName)
                |> String.concat ",\n"
                |> fun str ->
                    String.Format(
                        "the following info could not be parsed:
{0}.
The info was parsed from the string:
{1}",
                        str,
                        responseText
                    )


            let popupMsg =
                responseText
                |> Logic.Common.errorPopupMsg
                    dispatch
                    Logic.Common.killPopupMsg
                    Logic.Common.standardPositions

            let originIsAccessibleMsg =
                errorText |>
                (
                    Git_Repo_Parsing_Result.Parsing_Failed >>
                    Git_Repo_Parsing_Options.Parsing_Has_Been_Performed >>
                    Repository_Cloned >>
                    Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked  >>
                    Origin_Access_Result.Origin_accessbile >>
                    CheckProcess.CheckProcessEnded >>
                    Types.Check_Origin_Accessibility_Msg
                )

            let msgsCombined =
                [|
                    popupMsg
                    originIsAccessibleMsg
                |]

            msgsCombined
    | _ ->

        let repo = {
            UserName = (usrName responseText).UserValue.Value
            Email = (email responseText).UserValue.Value
            Branches = (branches responseText).UserValue.Value
            CurrBranch = (currBranch responseText).UserValue.Value
        }

        let newGitInfoMsg =
            repo |> Spread_New_Git_Repo_Main 

        let originIsAccessibleMsg =
            Git_Repo_Parsing_Result.Parsing_Succeded |>
            (
                Git_Repo_Parsing_Options.Parsing_Has_Been_Performed >>
                Repository_Cloned >>
                Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked  >>
                Origin_Access_Result.Origin_accessbile >>
                CheckProcess.CheckProcessEnded >>
                Types.Check_Origin_Accessibility_Msg
            )

        let msgCombined =
            [|
                popupKillMsg |> Main.Types.Async_Msg_Main

                newGitInfoMsg
                originIsAccessibleMsg
            |]

        msgCombined



let getInfoNotParsedCorrectly resList =
    resList
    |> Array.choose (fun itm ->
        match itm with
        | Logic.Common.StringOrStringArray.IsString str ->
            if str.UserValue.IsNone
            then str |> (Logic.Common.IsString >> Some) 
            else None
        | Logic.Common.StringOrStringArray.IsStringArray strArr ->
            if strArr.UserValue.IsNone
            then strArr |> (Logic.Common.IsStringArray >> Some) 
            else None)
    |> function
        | res when res.Length <> 0 ->
            res |> Some
        | _ ->
            None

let parseGitUser dispatch = async {

    let popupMsg =
        "Getting repo info"
        |> Popup.View.getPopupMsgSpinner
        |> Logic.Common.checkingProcessPopupMsg Logic.Common.standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000

    let prms = [|
        {
            SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
            SharedTypes.CdCommand.ResponseCommand = "git branch -r"
        }
        |> SharedTypes.IsCd
        {
            SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
            SharedTypes.CdCommand.ResponseCommand = "git config --global user.name"
        }
        |> SharedTypes.IsCd
        {
            SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
            SharedTypes.CdCommand.ResponseCommand = "git config --global user.email"
        }
        |> SharedTypes.IsCd
        {
            SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
            SharedTypes.CdCommand.ResponseCommand = "git status"
        }
        |> SharedTypes.IsCd
        {
            SharedTypes.CdCommand.MoveCommand = "server/loganalyzer"
            SharedTypes.CdCommand.ResponseCommand = "git status"
        }
        |> SharedTypes.IsCd
    |]

    let! responses = apis.Command prms

    let responsesCombined = 
        responses
        |> Array.map (fun response ->
                response.Answer
            )
        |> String.concat "/n"

    let resList =
        [|
            Logic.Common.IsString(usrName responsesCombined)
            Logic.Common.IsString(email responsesCombined)
            Logic.Common.IsStringArray(branches responsesCombined)
            Logic.Common.IsString(currBranch responsesCombined)
        |]

    let msgsAll =
        resList |>
        (
            getInfoNotParsedCorrectly >>
            getMessages responsesCombined dispatch
        ) 

    msgsAll
    |> Array.iter (fun msg -> msg |> dispatch)

}