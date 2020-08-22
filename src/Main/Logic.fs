module Main.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Types
open Global.Types
open Feliz

let standardPositions = {
        Popup.Types.PosX = 250.0
        Popup.Types.PosY = 250.0
    }

let QuestionPopup msg yesMsg noMsg dispatch positions =

    let questionToAsk =
        msg
        |> Popup.View.getPopupMsg

    let yesNoButtons =
        Popup.View.yesNoButtons yesMsg noMsg dispatch
        |> List.toArray

    let popupStyle =
        (yesNoButtons,questionToAsk)
        |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

    (popupStyle,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        delayedMessage 3000
    )

let errorPopupMsg dispatch msgToDispatch positions msg =
    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let simpleOkButton =
        Popup.View.simpleOkButton msgToDispatch dispatch

    let allComponentsSimleOk =
        (simpleOkButton,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok

    (allComponentsSimleOk,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Main.Types.Popup_Msg
    )

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Main.Types.Popup_Msg 
    )

let killPopupMsg =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Main.Types.Popup_Msg
    )

let kickedOutTemplate dispatch msg =
    let exitMsg =
        msg + ". Please refresh to return"
        |> Popup.View.getPopupMsg

    let button =
        Popup.View.simpleOkButton
                        killPopupMsg
                        dispatch

    let kickedOutMsg =
        (button,exitMsg) |>
        (
            GlobalMsg.Go_To_Failed_Page >>
            Main.Types.GlobalMsg_Main >>
            dispatch
        )

    kickedOutMsg

type GitUserDataType<'a> = {
    UserValue : 'a
    TypeName : string
}

type StringOrStringArray =
    | IsString of GitUserDataType<string option> 
    | IsStringArray of GitUserDataType<string[] option>  

let checkIfProcessExists dispatch = async{

    let prms = "shellCommand=which git"

    let! req = request prms

    match req.status with
    | 200.0 ->
        let isInstalled =
            "which: no"
            |> req.responseText.Contains
            |> not

        match isInstalled with
        | true ->

            let! checkOriginAccessibilityMsg =
                dispatch |>
                (
                    CheckProcess.CheckProcessStarted >>
                    Main.Types.Msg.Check_Origin_Accessibility_Msg >>
                    Global.Types.delayedMessage 1000
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

            msgCombined
            |> Array.iter (fun msg -> msg |>  dispatch )
        | false ->
            let resultGitInstalledMsg =
                Git_Installed_Result.Git_Not_Installed |>
                (
                    CheckProcess.CheckProcessEnded >>
                    Main.Types.Msg.Check_If_Git_Installed_Msg >>
                    dispatch
                )

            resultGitInstalledMsg

    | _ ->

        let resultGitInstalledMsg =
            req.responseText |>
            (
                Git_Installed_Result.Git_Installed_Result_Error >>
                CheckProcess.CheckProcessEnded >>
                Main.Types.Msg.Check_If_Git_Installed_Msg >>
                dispatch
            )

        resultGitInstalledMsg
}

let checkOriginAccessibility dispatch = async{

    let prms = "shellCommand=git ls-remote ssh://git@segaeesl01.eipu.ericsson.se:8081/nodetest/loganalyzer.git HEAD"

    let popupMsg =
        "Checking accesibility to server"
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 3000

    let! res = request prms

    match res.status with
    | 200.0 ->
        let isOriginAccessible =
            "HEAD"
            |> res.responseText.Contains

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
            
    | _ ->
        let popupMsg =
            res.responseText
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

    let prms = "shellCommand=cd server;cd loganalyzer;git remote -v"

    let! res = request prms

    match res.status with
    | 200.0 ->
        let doesRepoExist =
            "ssh://git@segaeesl01.eipu.ericsson.se:8081/nodetest/loganalyzer.git"
            |> res.responseText.Contains

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
            
    | _ ->
        let popupMsg =
            res.responseText
            |> errorPopupMsg
                        dispatch
                        killPopupMsg
                        standardPositions

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

let downloadRepo dispatch = async {

    let popupMsg =
        "Downloading loganalyzer repo"
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 3000
    
    let prms = "shellCommand=cd server;git clone ssh://git@segaeesl01.eipu.ericsson.se:8081/nodetest/loganalyzer.git"

    let! res = request prms

    match res.status with
    | 200.0 ->
        let repoClonedSuccesfully =
            "Make sure"
            |> res.responseText.Contains
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
                res.responseText
                |> errorPopupMsg
                        dispatch
                        killPopupMsg
                        standardPositions

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
            
    | _ ->

        let popupMsg =
            res.responseText
            |> errorPopupMsg
                        dispatch
                        killPopupMsg
                        standardPositions

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

let parseGitUser dispatch = async {

    let popupMsg =
        "Getting repo info"
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    let! popupKillMsg =
        Popup.Types.PopupStyle.Popup_Is_Dead |>
        (
            Types.Popup_Msg >>
            delayedMessage 1000
        )

    do! Async.Sleep 2000
    
    let repoInfoCommands =
        [|
            "cd server"
            "cd loganalyzer"
            "git config --global user.name"
            "git config --global user.email"
            "git status"
            "git branch -r"
        |]
        |> String.concat ";"
        |> fun x -> "shellCommand=" + x

    let! res = request repoInfoCommands

    match res.status with
    | 200.0 ->
        let usrName =
            res.responseText
            |> Regex.Match "\w.*"
            |> fun res ->
                {
                    UserValue = res
                    TypeName = "user name"
                }
        let email =
            res.responseText
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

        let branches =
            res.responseText
            |> Regex.Matches "(?<=origin\/).*"
            |> fun res ->
                {
                    UserValue = res
                    TypeName = "branches"
                }

        let currBranch =
            res.responseText
            |> Regex.Match "(?<='origin\/).*(?=')"
            |> fun res ->
                {
                    UserValue = res
                    TypeName = "branch"
                }

        let resList =
            [|
                IsString(usrName)
                IsString(email)
                IsStringArray(branches)
                IsString(currBranch)
            |]

        let infoNotParsedCorrectly =
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

        ()
        |> function
            | _ when infoNotParsedCorrectly.IsSome ->

                let errorText =
                    infoNotParsedCorrectly.Value
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
                            res.responseText
                        )


                let popupMsg =
                    res.responseText
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
                |> Array.iter (fun msg -> msg |> dispatch)
            | _ ->

                let repo = {
                    UserName = usrName.UserValue.Value
                    Email = email.UserValue.Value
                    Branches = branches.UserValue.Value
                    CurrBranch = currBranch.UserValue.Value
                }

                let newGitInfoMsg =
                    repo |>
                    (
                        Spread_New_Git_Repo_Main >>
                        dispatch
                    )

                let originIsAccessibleMsg =
                    Git_Repo_Parsing_Result.Parsing_Succeded |>
                    (
                        Git_Repo_Parsing_Options.Parsing_Has_Been_Performed >>
                        Repository_Cloned >>
                        Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked  >>
                        Origin_Access_Result.Origin_accessbile >>
                        CheckProcess.CheckProcessEnded >>
                        Types.Check_Origin_Accessibility_Msg >>
                        dispatch
                    )

                let msgCombined =
                    [|
                        ( popupKillMsg |> dispatch )
                        newGitInfoMsg
                        originIsAccessibleMsg
                    |]

                msgCombined
                |> Array.iter (fun msg -> msg )
    | _ ->
        let originIsAccessibleMsg =
            res.responseText|>
            (
                Git_Repo_Parsing_Result.Parsing_Failed >>
                Git_Repo_Parsing_Options.Parsing_Has_Been_Performed >>
                Repository_Cloned >>
                Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked  >>
                Origin_Access_Result.Origin_accessbile >>
                CheckProcess.CheckProcessEnded >>
                Types.Check_Origin_Accessibility_Msg >>
                dispatch
            )

        originIsAccessibleMsg
    
}

    
