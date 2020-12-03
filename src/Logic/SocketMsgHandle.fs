module App.Logic.SocketMsgHandle

open SharedTypes.Shared

type SocketDecision =
    | NoSocketDecision
    | PopupChange of Popup.Types.PopupStyle
    | MsgsToDispatch of Upgrade_NuGet.Types.Msg

type NugetChangeUtils = {
    Dispatch : (Upgrade_NuGet.Types.Msg -> unit)
    TableProjInfo : Upgrade_NuGet.Types.Project_Info 
}

let processFinishSuccessHandle ( foundProjInfoOpt : Result<NuGetInfo * string,string> ) 
                               ( nugetUtils : NugetChangeUtils ) =
    match foundProjInfoOpt with
    | Ok _ ->
        let newStatus =
            Upgrade_NuGet.Types.Loading_To_Nuget_Server_Alternatives.Building |>
            (
                Upgrade_NuGet.Types.Loading_Nuget_Info_Is_Not_Done >>
                Upgrade_NuGet.Types.Loading_Info_To_Server
            )

        let newLoadingStatusMsg =
            [|
                { nugetUtils.TableProjInfo with Loading_To_Server = newStatus} |>
                (
                    Upgrade_NuGet.Types.Change_Project_Info >>
                    Global.Types.delayedMessage 3000
                )


                nugetUtils.Dispatch |>
                (
                    Upgrade_NuGet.Types.Build_Solution_If_Ready_Msg >>
                    Global.Types.delayedMessage 3000
                )
            |] |>
            (
                Upgrade_NuGet.Types.Batch_Upgrade_Nuget_Async >>
                Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState nugetUtils.Dispatch
            )

        newLoadingStatusMsg
        |> MsgsToDispatch
    | Error err ->
        let newStatus =
            err |>
            (
                Upgrade_NuGet.Types.Loading_To_Server_Result.Loading_To_Server_Failed >>
                Upgrade_NuGet.Types.Loading_Nuget_Info_Is_Done >>
                Upgrade_NuGet.Types.Loading_Info_To_Server 
            )
             
        let newLoadingStatusMsg =
            { nugetUtils.TableProjInfo with Loading_To_Server = newStatus} |>
            (
                Upgrade_NuGet.Types.Change_Project_Info >>
                Global.Types.delayedMessage 2000
            )
            |> Upgrade_NuGet.Types.Upgrade_Nuget_Async
            |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState nugetUtils.Dispatch

        newLoadingStatusMsg
        |> MsgsToDispatch

let processStateDecision (prcs : Process<NuGetInfo, NuGetInfo * string> ) nugetUtils =
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

        popupType
        |> Popup.Types.PopupStyle.Has_No_Alternatives
        |> PopupChange

    | SharedTypes.Shared.Process.Finished result ->
        processFinishSuccessHandle result nugetUtils
        

let handleActions socketMsg nugetUtils =
    match socketMsg with
    | SharedTypes.Shared.ClientMsg.ChangeAction action ->
        match action with
        | SharedTypes.Shared.BridgeAction.None ->
            NoSocketDecision
        | SharedTypes.Shared.BridgeAction.ChangeNuGet prcs ->
            processStateDecision prcs nugetUtils

let handleSocketMsgs ( model : App.Types.Model ) socketMsg dispatch =
    match model.Main.Upgrade_NuGet.Projects_Table with
    | Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Upgrade_NuGet.Types.Loganalyzer_Projects_Table.Yes_Projects_Table_Info table ->
            let foundTableProjInfoOpt =
                table
                |> Seq.tryFind (fun projInfo ->
                        projInfo.Name.Replace(" ","") = projInfo.Name
                    )

            match foundTableProjInfoOpt with
            | Some foundTableProjInfo ->
                let nugetUtils = 
                    {
                        Dispatch  = dispatch
                        TableProjInfo = foundTableProjInfo
                    }
                handleActions socketMsg nugetUtils
            | _ ->
                NoSocketDecision
        | _ -> 
            NoSocketDecision
    | _ ->
        NoSocketDecision