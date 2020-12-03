module App.State

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Browser.Dom
open Types
open Global.Types

let pageParser: Parser<Page->Page,Page> =
    oneOf [
      map (HomePage(RcoUpdate)) (s "home")
      map VerificationPage (s "verify")
      map (HomePage(RcoUpdate)) top 
    ]


let urlUpdate (result: Option<Page>) model =
    match result with
    | None ->
        console.error("Error parsing url: " + window.location.href)
        model, Navigation.modifyUrl (toHash model.CurrentPage)

    | Some page ->
        { model with CurrentPage = page }, Cmd.none

let init result =
    {
        Git = Git_Installed_Check_Not_Performed
        CurrentPage = VerificationPage
        Main = Main.State.init 
        VerifyStrMsg = Verify_Str_Msg_Not_Determined
        Popup = Popup.Types.PopupStyle.Popup_Is_Dead
        Dispatch = None
    }, Cmd.none


let update msg (model:Model) : Types.Model * Cmd<Types.Msg> =
    match msg with
    | MainMsg mainMsg ->
        let (mainModel,globalMsg,main_msg_cmd) = Main.State.update mainMsg model.Main

        let global_msg_cmd =
            globalMsg |>
            (
                Global >>
                Cmd.ofMsg
            )

        let app_cmd = Cmd.map MainMsg main_msg_cmd

        let msgsCombined =
            [|
                global_msg_cmd
                app_cmd
            |]

        { model with Main = mainModel}, Cmd.batch msgsCombined
        
    | Global globalMsg ->
        match globalMsg with
        | Batch msgs ->
            let msgsIntoGlobal =
                msgs
                |> Array.map (fun globalMsg ->
                    globalMsg |>
                    (
                        Global >>
                        Cmd.ofMsg
                    ))
            model, Cmd.batch msgsIntoGlobal
        | GlobalMsg.MsgNone ->
            model, []
        | GlobalMsg.SetNewPage page ->
            { model with CurrentPage = page }, []

        | VerifyFailesMsg strMsgOption ->
            { model with VerifyStrMsg = strMsgOption  }, []
        | Change_Git_Installed_Status_Msg git_Decision ->
           { model with Git = git_Decision }, []
        | Change_Git_Origin_Access_Msg origin_access_options ->
            let git_decision =
                origin_access_options |>
                (
                    Git_Installed >>
                    GitDecision.Git_Installed_Check_Performed
                )

            { model with Git = git_decision }, []

        | Change_Repo_Cloned_Msg repo_cloned_result ->
            let git_decision =
                repo_cloned_result |>
                (
                    Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked >>
                    Origin_accessbile >>
                    Origin_Accessibility_Has_Been_Checked >>
                    Git_Installed >>
                    GitDecision.Git_Installed_Check_Performed
                )

            { model with Git = git_decision }, []

        | Change_Repo_Parsing_Result repo_parsing_result ->
            let git_decision =
                repo_parsing_result |>
                (
                    Git_Repo_Parsing_Options.Parsing_Has_Been_Performed >>
                    Git_Repo_Cloned_Result.Repository_Cloned >>
                    Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked >>
                    Origin_accessbile >>
                    Origin_Accessibility_Has_Been_Checked >>
                    Git_Installed >>
                    GitDecision.Git_Installed_Check_Performed
                )

            { model with Git = git_decision }, []
        | Popup_Msg_Global style ->
            { model with Popup = style }, []
        | Go_To_Failed_Page(button,msgs) ->
            let popupOption =
                (button,msgs)
                |> Popup.Types.Simple_Ok

            let popupStyle =
                (popupOption,Main.Logic.Common.standardPositions)
                |> Popup.Types.Has_Alternatives

            let newGitDecisionState =
                Git_Error |>
                (
                    Git_Repo_Cloned_Options.Repo_Cloned_Has_Been_Checked >>
                    Origin_accessbile >>
                    Origin_Accessibility_Has_Been_Checked >>
                    Git_Installed >>
                    GitDecision.Git_Installed_Check_Performed
                )

            { model with Popup = popupStyle
                         Git = newGitDecisionState}, []
        | Spread_New_Branch_Name git_repo ->
            let main_model_msg_version =
                git_repo |>
                (
                    Main.Types.Msg.Spread_New_Branch_Name_Main >>
                    Types.Msg.MainMsg >>
                    Cmd.ofMsg
                )
            model, main_model_msg_version

        | Spread_New_Git_Repo git_repo ->
            let main_model_msg_version =
                git_repo |>
                (
                    Main.Types.Msg.Spread_New_Git_Repo_Main >>
                    Types.Msg.MainMsg >>
                    Cmd.ofMsg
                )
            model, main_model_msg_version
        | Change_Activity_Global activity ->
            {
                model with Main = {
                        model.Main with Activity = activity
                }
            }, []
    | SocketMsg socketMsg ->
        match socketMsg with
        | SharedTypes.Shared.ClientMsg.ChangeAction action ->
            match action with
            | SharedTypes.Shared.BridgeAction.None ->
                model, []
            | SharedTypes.Shared.BridgeAction.ChangeNuGet prcs->
                match prcs with
                | SharedTypes.Shared.Process.OnGoing info ->
                    let msg = "Changing NuGet name (" + (info.Uploaded |> int |> string) + " written)"

                    let popupElement =
                        (msg) |>
                        (
                            float >>
                            Popup.View.getPopupMsgProgress msg
                        )

                    let popupType =
                        (popupElement, Popup.Types.standardPositions)
                        |> Popup.Types.PopupStyle.Has_No_Alternatives

                    { model with Popup = popupType }, []

                | SharedTypes.Shared.Process.Finished result ->
                    match result with
                    | Ok (projInfo,resMsg) ->
                        match model.Main.Upgrade_NuGet.Projects_Table with
                        | Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
                            match res with
                            | Upgrade_NuGet.Types.Loganalyzer_Projects_Table.Yes_Projects_Table_Info table ->
                                let foundProjInfoOpt =
                                    table
                                    |> Seq.tryFind (fun projInfo ->
                                            projInfo.Name.Replace(" ","") = projInfo.Name
                                        )
                                
                                match foundProjInfoOpt with
                                | Some foundProjInfo ->
                                    let newStatus =
                                        Upgrade_NuGet.Types.Loading_To_Nuget_Server_Alternatives.Building |>
                                        (
                                            Upgrade_NuGet.Types.Loading_Nuget_Info_Is_Not_Done >>
                                            Upgrade_NuGet.Types.Loading_Info_To_Server
                                        )

                                    let newLoadingStatusMsg =
                                        [|
                                            { foundProjInfo with Loading_To_Server = newStatus} |>
                                            (
                                                Upgrade_NuGet.Types.Change_Project_Info >>
                                                delayedMessage 3000
                                            )

                                            Elmish.Dispatch.FromConverter 

                                            dispatch |>
                                            (
                                                Build_Solution_If_Ready_Msg >>
                                                delayedMessage 3000
                                            )
                                        |]
                                        |> Upgrade_NuGet.Types.Batch_Upgrade_Nuget_Async
                                        |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch

                                    model, []
                                | _ ->
                                    model,[]
                            | _ -> 
                                model, []
                        | _ ->
                            model, []
    | AddDispatch dispatch ->
        {model with Dispatch = dispatch |> Some},[]

