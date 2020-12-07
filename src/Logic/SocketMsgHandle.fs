module App.Logic.SocketMsgHandle

open SharedTypes

type SocketDecision =
    | NoSocketDecision
    | MsgsToDispatch of Upgrade_NuGet.Types.Msg

type NugetChangeUtils = {
    Dispatch : (Upgrade_NuGet.Types.Msg -> unit)
    TableProjInfo : Upgrade_NuGet.Types.Project_Info 
}

let changeStatusFailed nugetUtils errMsg =
    let newStatus =
        errMsg |>
        (
            Upgrade_NuGet.Types.Loading_To_Server_Failed >>
            Upgrade_NuGet.Types.Loading_Nuget_Info_Is_Done >>
            Upgrade_NuGet.Types.Loading_Info_To_Server
        )

    let newLoadingStatusMsg =
        [|
            { nugetUtils.TableProjInfo with Loading_To_Server = newStatus} |>
            (
                Upgrade_NuGet.Types.Change_Project_Info
            )


            nugetUtils.Dispatch |>
            (
                Upgrade_NuGet.Types.Build_Solution_If_Ready_Msg
            )
        |] |>
        (
            Upgrade_NuGet.Types.Batch>>
            Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState nugetUtils.Dispatch
        )

    newLoadingStatusMsg
    |> MsgsToDispatch

let changeStatusFinished nugetUtils =
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
                Upgrade_NuGet.Types.Change_Project_Info
            )


            nugetUtils.Dispatch |>
            (
                Upgrade_NuGet.Types.Build_Solution_If_Ready_Msg
            )
        |] |>
        (
            Upgrade_NuGet.Types.Batch >>
            Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState nugetUtils.Dispatch
        )

    newLoadingStatusMsg
    |> MsgsToDispatch

let changeStatus nugetUtils progress additionalMsg =
    let newStatus =
            (progress,additionalMsg) |>
            (
                Upgrade_NuGet.Types.Loading_To_Nuget_Server_Alternatives.Changing_Nuget_Name >>
                Upgrade_NuGet.Types.Loading_Nuget_Info_Is_Not_Done >>
                Upgrade_NuGet.Types.Loading_Info_To_Server
            )

    let newLoadingStatusMsg =
        { nugetUtils.TableProjInfo with Loading_To_Server = newStatus} |>
        (
            Upgrade_NuGet.Types.Change_Project_Info >>
            Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState nugetUtils.Dispatch
        )

    newLoadingStatusMsg
    |> MsgsToDispatch

let projectTableExists ( model : App.Types.Model ) dispatch projName =
    match model.Main.Upgrade_NuGet.Projects_Table with
    | Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Upgrade_NuGet.Types.Loganalyzer_Projects_Table.Yes_Projects_Table_Info table ->
            let foundTableProjInfoOpt =
                table
                |> Seq.tryFind (fun tableProjInfo ->
                        tableProjInfo.Name.Replace(" ","") = (projName : string ).Replace(" ","")
                    )

            match foundTableProjInfoOpt with
            | Some foundTableProjInfo ->
                let nugetUtils = 
                    {
                        Dispatch  = dispatch
                        TableProjInfo = foundTableProjInfo
                    }

                nugetUtils
                |> Some
            | _ ->
                None
        | _ ->
            None
    | _ ->  
        None
        

let handleActions ( model : App.Types.Model ) socketMsg dispatch =
    match socketMsg with
    | SharedTypes.Shared.ClientMsg.ChangeAction action ->
        match action with
        | SharedTypes.Shared.BridgeAction.None ->
            NoSocketDecision
        | SharedTypes.Shared.BridgeAction.ChangeNuGet prcs ->
            match prcs with
            | Shared.Process.Finished res ->
                match res with
                | Ok nugetInfo ->
                    match (projectTableExists model dispatch nugetInfo.ProjectName) with
                    | Some nugetUtils ->
                        changeStatusFinished nugetUtils
                    | _ ->
                        NoSocketDecision
                | Error (nugetInfo,errMsg) ->
                    match (projectTableExists model dispatch nugetInfo.ProjectName) with
                    | Some nugetUtils ->
                        changeStatusFailed nugetUtils errMsg
                    | _ ->
                        NoSocketDecision
            | Shared.OnGoing nugetInfo ->
                match (projectTableExists model dispatch nugetInfo.ProjectName) with
                | Some nugetUtils ->
                    changeStatus nugetUtils nugetInfo.Uploaded nugetInfo.AddMsg
                | _ ->
                    NoSocketDecision
