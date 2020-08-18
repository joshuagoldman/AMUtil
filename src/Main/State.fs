module Main.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser.Dom
open Types
open JsInterop
open Global.Types

let updateGitRepo model git_Repo =
    let newRcoUpdateState =
        git_Repo |> Rco_Update.Types.Yes_Git_info
        |> fun info ->
            { model.Rco_Update with Info = info}

    let newUpgradeNugetState =
        git_Repo |> Upgrade_NuGet.Types.Yes_Git_Info_Nuget
        |> fun info ->
            { model.Upgrade_NuGet with Info = info}

    {
        model with
            Rco_Update = newRcoUpdateState
            Upgrade_NuGet = newUpgradeNugetState
    },
    GlobalMsg.MsgNone, []

let init =
    {
        CurrentRcoFile = ""
        CurrentOption = Home
        Activity = Activity_None
        Rco_Update = Rco_Update.State.init ()
        Upgrade_NuGet = Upgrade_NuGet.State.init ()
    }


let update msg (model:Model) : Model * GlobalMsg * Cmd<Msg> =
    match msg with
    | Batch_Main msgs ->
        let batchedMsgs =
            msgs
            |> Array.map (fun msg -> msg |> Cmd.ofMsg)
            |> Cmd.batch
        model, MsgNone, batchedMsgs
    | MsgNone_Main ->
        model, MsgNone,[]
    | Check_If_Git_Installed_Msg stage ->
        match stage with
        | CheckProcessStarted dispatch ->
            Logic.checkIfProcessExists dispatch
            |> Async.StartImmediate
            model, MsgNone, []
        | CheckProcess.CheckProcessEnded result ->

            let changeInstalledStatusGlobalMsg =
                result |>
                (
                    GitDecision.Git_Installed_Check_Performed >>
                    Change_Git_Installed_Status_Msg
                )

            let newMsgsIntoOne =
                [|
                    changeInstalledStatusGlobalMsg
                |]
                |> GlobalMsg.Batch
              
            model, newMsgsIntoOne, []
    | Check_Origin_Accessibility_Msg stage ->
        match stage with
        | CheckProcess.CheckProcessStarted dispatch  ->
            dispatch |>
            (
                Logic.checkOriginAccessibility >>
                Async.StartImmediate
            )
            model, MsgNone, []
        | CheckProcess.CheckProcessEnded result ->

            let changeOriginCheckedStatusMsg = 
                result |>
                (
                    Origin_Accessibility_Has_Been_Checked >>
                    Change_Git_Origin_Access_Msg
                
                )
            model, changeOriginCheckedStatusMsg, []
            
    | Check_If_Repo_Cloned_Msg stage ->
        match stage with
        | CheckProcess.CheckProcessStarted dispatch  ->
            dispatch |>
            (
                Logic.checkRepositoryDownloaded >>
                Async.StartImmediate
            )
            model, MsgNone, []
        | CheckProcess.CheckProcessEnded result ->

            let changeOriginCheckedStatusMsg = 
                result |> Change_Repo_Cloned_Msg

            model, changeOriginCheckedStatusMsg, []

    | Clone_Repo_Msg stage ->
        match stage with
        | CheckProcess.CheckProcessStarted dispatch  ->
            dispatch |>
            (
                Logic.downloadRepo >>
                Async.StartImmediate
            )
            model, MsgNone, []
        | CheckProcess.CheckProcessEnded result ->

            let changeOriginCheckedStatusMsg = 
                result |> Change_Repo_Cloned_Msg

            model, changeOriginCheckedStatusMsg, []

    | Check_Repo_Parsing_Msg stage ->
        match stage with
        | CheckProcess.CheckProcessStarted dispatch ->
             dispatch |>
             (
                 Logic.parseGitUser >>
                 Async.StartImmediate
             )
             model, MsgNone, []
        | CheckProcess.CheckProcessEnded result ->
             let changeOriginCheckedStatusMsg = 
                 result |> Change_Repo_Parsing_Result

             model, changeOriginCheckedStatusMsg, []
    | Popup_Msg style ->
        let popupMsg =
            style |> Popup_Msg_Global

        model, popupMsg, []

    | Change_Activity activity ->
        { model with Activity = activity }, MsgNone, []

    | Obtain_New_Nuget_Info(dispatch,activity) ->
        Logic.checkIfDotNetInstalled dispatch
        |> Async.StartImmediate

        { model with Activity = activity }, MsgNone, []

    | Rco_Update_Msg rco_update_msg ->
        let (rco_update_model, global_msg, rco_msg_cmd) = Rco_Update.State.update rco_update_msg model.Rco_Update

        { model with Rco_Update = rco_update_model }, global_msg, Cmd.map Rco_Update_Msg rco_msg_cmd
    | Upgrade_NuGet_Msg upgrade_nuget_msg ->
        let (upgrade_nuget_model, global_msg, rco_msg_cmd) = Upgrade_NuGet.State.update upgrade_nuget_msg model.Upgrade_NuGet

        { model with Upgrade_NuGet = upgrade_nuget_model }, global_msg, Cmd.map Upgrade_NuGet_Msg rco_msg_cmd

    | Spread_New_Branch_Name_Main git_Repo ->
        git_Repo
        |> updateGitRepo model
    | Spread_New_Git_Repo_Main git_Repo ->
        git_Repo
        |> updateGitRepo model

    | GlobalMsg_Main global_msg ->
        model,global_msg,[]

    | Get_All_Projects_Info dispatch ->
        Logic.getNuGetTableInfo dispatch
        |> Async.StartImmediate

        model,MsgNone,[]



