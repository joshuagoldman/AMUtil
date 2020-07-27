module Main.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser.Dom
open Types
open JsInterop
open Global.Types

let init =
    {
        CurrentRcoFile = ""
        CurrentOption = Home
        Activity = Activity_None
        Rco_Update = Rco_Update.State.init ()
    }


let update msg (model:Model) : Model * GlobalMsg * Cmd<Msg> =
    match msg with
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

    | Change_Activity actibity ->
        { model with Activity = actibity }, MsgNone, []

    | Rco_Update_Msg rco_update_msg ->
        let (rco_update_model, global_msg, rco_msg_cmd) = Rco_Update.State.update rco_update_msg model.Rco_Update

        { model with Rco_Update = rco_update_model }, global_msg, Cmd.map Rco_Update_Msg rco_msg_cmd



