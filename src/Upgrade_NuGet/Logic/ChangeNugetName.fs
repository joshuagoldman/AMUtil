module Upgrade_NuGet.Logic.ChangeNugetName

open JsInterop
open Elmish
open Fable.Import
open System
open Upgrade_NuGet.Types
open Global.Types
open Feliz
open Fable.Core.JsInterop
open SharedTypes.NuGetChange

let saveChanges model dispatch =
    match model.Info with
    | Upgrade_NuGet.Types.Git_Info_Nuget.Yes_Git_Info_Nuget _ ->
        match model.Projects_Table with
        | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
            match res with
            |Loganalyzer_Projects_Table.Yes_Projects_Table_Info proj_infos ->
                let areAnyProjsReady4Change =
                    proj_infos
                    |> Array.choose (fun proj ->
                        match proj.Server_Options with
                        | Server_Options.No_Server_Actions ->
                            None
                        | _ ->
                            Some proj)
                    |> function
                        | res when (res |> Array.length) <> 0 ->
                            res |> Some
                        | _ -> None

                match areAnyProjsReady4Change with
                | Some projsReady4Change ->
                    Html.div[
                        prop.className "column is-2"
                        prop.children[
                            Html.div[
                                prop.className "button"
                                prop.text "Save NuGet changes"
                                prop.onClick (fun _ ->
                                    (projsReady4Change,dispatch) |>
                                    (
                                        Upgrade_NuGet.Types.Msg.Save_Nuget_Info_To_Server >>
                                        dispatch
                                    ))
                            ]
                        ]
                    ]
                | _ ->
                    Html.none
            | _ -> Html.none
        | _ -> Html.none
    | _ -> Html.none

let getNewStatus progress project dispatch = 

    let newStatus =
        progress |>
        (
            Loading_To_Nuget_Server_Alternatives.Changing_Nuget_Name >>
            Loading_Nuget_Info_Is_Not_Done >>
            Loading_Info_To_Server
        )

    let newLoadingStatusMsg =
        { project with Loading_To_Server = newStatus} |>
        (
            Upgrade_NuGet.Types.Change_Project_Info
        ) |>
        (
            Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch >>
            dispatch
        )

    newLoadingStatusMsg

type Project = {
    ProjectName : string
    ProjectNamePure : string
}

let changeNameAsync project ( version : string ) dispatch = async {

    let reqBody =
        {
            Project = {
                ProjectName = project.Name
                ProjectNamePure = project.Name.Replace("Ericsson.AM.","")
            }
            NuGetVersionName  = version
            Paths = {
                SpecificPath = ""
                GeneralPath = ""
            }
            Socket = {
                Port = 3001
                URL = "localhost"
            }
            Rate = 1
            
        }

    let url = "http://localhost:8086/api/ChangeName"

    let! request =
        Async.FromContinuations <| fun (resolve,_,_) ->

            let xhr = Browser.XMLHttpRequest.Create()
            xhr.``open``(method = "POST", url = url)
            xhr.setRequestHeader("Content-Type","application/x-www-form-urlencoded")
            xhr.timeout <- 10000.0

            let socket = Browser.WebSocket.Create("http://localhost:3001")

            socket.onmessage <- fun x ->

                let eventResult = (x.data :?> string)

                let progress =JsInterop.Regex.Match "(?<=progress=).*?(?=;)*" eventResult
                let id =JsInterop.Regex.Match "(?<=id=).*?(?=;)*" eventResult
                let isMsg =JsInterop.Regex.IsMatch "@message:" eventResult 

                match (id.Value = project.Name) with
                    | true ->
                        match isMsg.Value with
                        | true ->
                            getNewStatus (progress.Value |> float) project dispatch
                        | _ ->
                            let response = (x.data :?> string)
                        
                            socket.close()

                            getNewStatus 100.0 project dispatch

                            resolve 
                                {
                                    Status = 500
                                    Msg = response
                                }
                    | _ ->
                        ()

            xhr.ontimeout <- fun _ ->
                let error =
                    "Connection timed out."

                resolve
                    {
                        Status = 404
                        Msg = error
                    }

            xhr.send(reqBody) |> fun  _ -> ()

    return(request)

}

let reqModel version ( proj : Project_Info ) =
    let paths = {
                SpecificPath = ""
                GeneralPath = ""
    }
    let socketInfo = {
        Port = 3001
        URL = "127.0.0.1"
    }

    let project =  {
        SharedTypes.NuGetChange.ProjectName = "Ericsson.AM." + proj.Name
        SharedTypes.NuGetChange.ProjectNamePure = proj.Name
    }

    let changeNugetNameModel = {
        SharedTypes.NuGetChange.Project = project
        NuGetVersionName = version
        Paths = paths
        Socket = socketInfo
        Rate = 1
    }

    changeNugetNameModel

let changeNameRequest ( socket : Browser.WebSocket ) changeNugetNameModel dispatch =
    Async.FromContinuations <| fun (resolve,_,_) ->
        socket.onmessage <- fun socketMsg ->
            let msg = (socketMsg.data :?> string)

            let isMsg = JsInterop.Regex.IsMatch "@message:" msg 
            match isMsg.Value with
            | true ->

                let msgMatch = (JsInterop.Regex.Match "@message:" msg).Value
                let msg = "Changing NuGet name (" + (msgMatch |> int |> string) + " written)"

                let popupInfoStr =
                    (msg) |>
                    (
                        float >>
                        Popup.View.getPopupMsgProgress msg >>
                        Upgrade_NuGet.Logic.Miscellaneous.checkingProcessPopupMsg Upgrade_NuGet.Logic.Miscellaneous.standardPositions >>
                        dispatch
                    )

                popupInfoStr
            | _ ->
                let response = (socketMsg.data :?> string)
                socket.close()

                resolve 
                    {
                        Status = 500
                        Msg = response.ToLower().Replace("@finished","")
                    }

        Global.Types.apis.ChangeNuGet changeNugetNameModel
        |> Async.StartImmediate

let changeNameRequestToMsgArray projectsWithNewNames dispatch =
    
    let socket = Browser.WebSocket.Create("ws://127.0.0.1:3001")

    projectsWithNewNames
    |> Array.map (fun (proj : Project_Info,version) ->
        async {

            let changeNugetModel = reqModel version proj

            let! res = changeNameRequest socket changeNugetModel dispatch

            match res.Status with
            | 200 ->
                let newStatus =
                    Loading_To_Nuget_Server_Alternatives.Building |>
                    (
                        Loading_Nuget_Info_Is_Not_Done >>
                        Loading_Info_To_Server
                    )
                     
                let newLoadingStatusMsg =
                    [|
                        { proj with Loading_To_Server = newStatus} |>
                        (
                            Upgrade_NuGet.Types.Change_Project_Info >>
                            delayedMessage 3000
                        )

                        dispatch |>
                        (
                            Build_Solution_If_Ready_Msg >>
                            delayedMessage 3000
                        )
                    |]
                    |> Upgrade_NuGet.Types.Batch_Upgrade_Nuget_Async
                    |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch
                    
                return(newLoadingStatusMsg)
            | _ ->
                let newStatus =
                    res.Msg |>
                    (
                        Loading_To_Server_Result.Loading_To_Server_Failed >>
                        Loading_Nuget_Info_Is_Done >>
                        Loading_Info_To_Server 
                    )
                     
                let newLoadingStatusMsg =
                    { proj with Loading_To_Server = newStatus} |>
                    (
                        Upgrade_NuGet.Types.Change_Project_Info >>
                        delayedMessage 2000
                    )
                    |> Upgrade_NuGet.Types.Upgrade_Nuget_Async
                    |> Upgrade_NuGet.Logic.Miscellaneous.turnIntoSendPopupWithNewState dispatch

                return(newLoadingStatusMsg)
        })

let ChangeNugetNameAndBuildSolution projects dispatch = 
    let killPopup =
        Popup.Types.Popup_Is_Dead |>
        (
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget
        )
        
    let yesNoPopupMsg yesMsg =
        let yesNoPopupButtons =
            Popup.View.yesNoButtons yesMsg killPopup dispatch

        let popupType =
            (yesNoPopupButtons |> List.toArray,Upgrade_NuGet.Logic.Miscellaneous.allProjsLoadingDecisionQuestionPopup projects)
            |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

        let yesNoButton =
            (
                (popupType,Popup.Types.standardPositions)
                |> Popup.Types.Has_Alternatives
            )

        yesNoButton |>
        (
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget
        )

    let existsProjectsWithNewNames =
        projects
        |> Array.choose (fun proj ->
            match proj.Server_Options with
            | Server_Options.Push_Nuget ->
                match proj.Nuget_Names.New_Nuget_Name with
                | New_Nuget_Name.Has_New_Name validity ->
                    match validity with
                    | Nuget_Name_Valid newName ->
                        (proj,newName) |> Some
                    | _ -> None
                | _ -> None
            | _ -> None)
        |> function
            | res when (res |> Array.length) <> 0 ->
                res |> Some
            | _ -> None

    match existsProjectsWithNewNames with
    | Some projectsWithNewNames ->
        let msgWithRequests =
            dispatch |>
            (
                changeNameRequestToMsgArray projectsWithNewNames >>
                Batch_Upgrade_Nuget_Async
            )
            
        let loadingProjsToChangeToBuild =
            projects
            |> Array.choose (fun proj ->
                match proj.Server_Options with
                | Server_Options.Push_Nuget ->
                    None
                | _ -> proj |> Some)
            |> function
                | res when (res |> Array.length) <> 0 ->
                    res |> Some
                | _ -> None

        match loadingProjsToChangeToBuild with
        | Some buildProjs ->

            let buildAndChangeNameMsgs =
                [|
                    Upgrade_NuGet.Logic.Miscellaneous.changeToBuildingMsgs buildProjs dispatch
                    msgWithRequests
                |]
                |> Upgrade_NuGet.Types.Batch

            yesNoPopupMsg buildAndChangeNameMsgs
                    
        | _ -> 
            yesNoPopupMsg msgWithRequests
    | _ ->
        let buildMsg =
            dispatch |>
            (
                Build_Solution_If_Ready_Msg >>
                delayedMessage 3000 >>
                Upgrade_NuGet.Types.Upgrade_Nuget_Async
            )
        let buildAndChangeNameMsgs =
            [|
                Upgrade_NuGet.Logic.Miscellaneous.changeToBuildingMsgs projects dispatch
                buildMsg
            |]
            |> Upgrade_NuGet.Types.Batch

        yesNoPopupMsg buildAndChangeNameMsgs