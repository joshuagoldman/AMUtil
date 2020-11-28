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
open Main.Logic.Common
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
                UserValue = res
                TypeName = "user name"
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
                        UserValue = res
                        TypeName = "email"
                    }
            else
                {
                    UserValue = None
                    TypeName = "email"
                }
        | _ ->
            {
                UserValue = None
                TypeName = "email"
            }

let branches responsesCombined =
    responsesCombined
    |> Regex.Matches "(?<=origin\/).*"
    |> fun res ->
        {
            UserValue = res
            TypeName = "branches"
        }

let currBranch responsesCombined =
    responsesCombined
    |> Regex.Match "(?<=Your branch is up to date with\s+'origin\/).*(?=\')|(?<=On branch\s+).*"
    |> fun res ->
        {
            UserValue = res
            TypeName = "branch"
        }

let getMessages responseText dispatch infoNotParsedCorrectlyRes =
    match infoNotParsedCorrectlyRes with
    | Some infoNotParsedCorrectly ->
            let errorText =
                infoNotParsedCorrectly
                |> Array.map (fun info ->
                    match info with
                    | StringOrStringArray.IsString x ->
                        x.TypeName
                    | StringOrStringArray.IsStringArray x ->
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
                |> errorPopupMsg
                    dispatch
                    killPopupMsg
                    standardPositions

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
        | StringOrStringArray.IsString str ->
            if str.UserValue.IsNone
            then str |> (IsString >> Some) 
            else None
        | StringOrStringArray.IsStringArray strArr ->
            if strArr.UserValue.IsNone
            then strArr |> (IsStringArray >> Some) 
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
        |> checkingProcessPopupMsg standardPositions
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
            SharedTypes.CommandInfo.Command = "git"
            SharedTypes.CommandInfo.Arg = "config --global user.name"
        }
        {
            SharedTypes.CommandInfo.Command = "git"
            SharedTypes.CommandInfo.Arg = "config --global user.email"
        }
        {
            SharedTypes.CommandInfo.Command = "git"
            SharedTypes.CommandInfo.Arg = "status"
        }
        {
            SharedTypes.CommandInfo.Command = "git"
            SharedTypes.CommandInfo.Arg = "branch -r"
        }
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
            IsString(usrName responsesCombined)
            IsString(email responsesCombined)
            IsStringArray(branches responsesCombined)
            IsString(currBranch responsesCombined)
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