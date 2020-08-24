module Upgrade_NuGet.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Types
open Global.Types
open Feliz
open Fable.Core.JsInterop

let getBranches info =
    match info with
    | Types.No_Git_Info_Nuget -> [|Html.none|]
    | Types.Yes_Git_Info_Nuget repo ->
        repo.Branches
        |> Array.map (fun branch ->
            branch |> Rco_Update.View.branchAlt)

let handleBranchNameChange branchName gitRepo =

    match branchName with
    | "Choose Branch..." ->
        Global.Types.MsgNone |>
        (
            GlobalMsg_Upgrade_Nuget 
        )

    | _ ->
        { gitRepo with CurrBranch = branchName} |>
        (
            Spread_New_Branch_Name >>
            GlobalMsg_Upgrade_Nuget
        )

let changeNugetStatus isCurrentlyChosen =
    match isCurrentlyChosen with
    | Project_Not_Chosen ->
        Project_Chosen
    | _ ->
        Project_Not_Chosen

let haveProjectsBeenLoaded projLoadingMixes =
    projLoadingMixes
    |> Array.forall (fun proj ->
        match proj with
        | Loganalyzer_Projects_Table_Mix.Project_Loading _ -> false
        | _ -> true)

let monitorEachProjectInfoExtraction ( projectsLoadingInfo : Loganalyzer_Projects_Table_Mix [] )
                                     ( nugetServerInfo : string )
                                       projectName = async {
    let fullProjectNameFolder = "project=Ericsson.AM." + projectName

    let nugetPackageVersionRegex = "(?<=<Version>).*(?=<\/Version>)"

    let modifyProjectsLoadingInfo newInfo =
        projectsLoadingInfo
        |> Array.map (fun proj ->
            match proj with
            | Loganalyzer_Projects_Table_Mix.Project_Loading proj_loading ->
                if proj_loading.Project_Name = projectName
                then
                    newInfo
                    |> Loganalyzer_Projects_Table_Mix.Project_Not_Loading
                else
                    proj
            | _ -> proj)

    let pickSuccessProjectInfoLoads projLoadingMixes =
        projLoadingMixes
        |> Array.choose (fun proj ->
            match proj with
            | Loganalyzer_Projects_Table_Mix.Project_Loading _ ->
                None
            | Loganalyzer_Projects_Table_Mix.Project_Not_Loading res ->
                match res with
                | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull projInfo ->
                    Some projInfo
                | _ -> None)
        |> function
            | res when res |> Array.length <> 0 ->
                Some res
            | _ -> None

    let checkFinishLoading newInfo =
        let newMix = newInfo |> modifyProjectsLoadingInfo

        let pickNewLoadingProject =
            projectsLoadingInfo
            |> Array.tail
            |> Array.choose (fun proj ->
                match proj with
                | Loganalyzer_Projects_Table_Mix.Project_Loading proj_loading ->
                    Some proj_loading
                | Loganalyzer_Projects_Table_Mix.Project_Not_Loading res ->
                    match res with
                    | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull projInfo ->
                        None
                    | _ -> None)
            |> function
                | res when res |> Array.length <> 0 ->
                    Some (res |> Array.head)
                | _ -> None
        

        let successInfo =
            newMix
            |> pickSuccessProjectInfoLoads

        match successInfo with
        | Some info ->
            [|
                info |>
                (
                    Yes_Projects_Table_Info >>
                    Info_Has_Been_Loaded >>
                    Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                    delayedMessage 500
                )
            |]
            |> Batch_Upgrade_Nuget_Async
                
        | _ ->
            [|
                No_Projects_Table_Info |>
                (
                    Info_Has_Been_Loaded >>
                    Upgrade_NuGet.Types.Msg.Change_NuGet_Status >>
                    delayedMessage 500
                )
            |]
            |> Batch_Upgrade_Nuget_Async
            
    let! res = requestCustom "http://localhost:3001/projectInfo" fullProjectNameFolder

    let standardFailMsg = "Loading was not successfull"

    match res.status with
    | 200.0 ->
        let foundNugetVersionOpt =
            JsInterop.Regex.IsMatch nugetPackageVersionRegex res.responseText

        match foundNugetVersionOpt with
        | Some foundNugetVersion ->
            match foundNugetVersion with
            | true ->
                let nugetServerRegex =
                    String.Format(
                        "(?<=Ericsson.AM.{0}',Version=').*(?=')",
                        projectName
                    )
                let isPartOfNugetServerOpt = JsInterop.Regex.IsMatch nugetServerRegex nugetServerInfo

                match isPartOfNugetServerOpt with
                | Some isPartOfNugetServer ->
                    match isPartOfNugetServer with
                    | true ->
                        let existingPackages =
                            JsInterop.Regex.Matches nugetServerRegex nugetServerInfo
                            
                        let newInfo =
                            {
                                Name = projectName
                                Changes = Project_Changes.Project_Has_No_Changes
                                Nuget_Names =
                                    {
                                        CurrName =
                                            JsInterop.Regex.Match nugetPackageVersionRegex res.responseText
                                            |> fun x -> x.Value

                                        New_Nuget_Name = New_Nuget_Name.Has_No_Name
                                    }
                                Existing_Packages = existingPackages.Value
                                Server_Options = No_Server_Actions
                                Loading_To_Server = Not_Loading_Info_To_Server
                            }
                            |> Loganalyzer_Projects_Table_Result.Loading_Was_Successfull
                            
                        return(checkFinishLoading newInfo)
                    | _ ->
                        let asyncMsg2Dispatch =
                            (
                                (projectName,"Project is not part of NuGet server")
                                |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
                            )
                            |> checkFinishLoading 

                        return(asyncMsg2Dispatch)
                    
                | _ ->
                    let asyncMsg2Dispatch =
                        (
                            (projectName,"Project is not part of NuGet server")
                            |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
                        )
                        |> checkFinishLoading 

                    return(asyncMsg2Dispatch)
                
            | _ ->
                let asyncMsg2Dispatch =
                    (
                        (projectName,standardFailMsg)
                        |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
                    )
                    |> checkFinishLoading 

                return(asyncMsg2Dispatch)
        | _ ->
            let asyncMsg2Dispatch =
                (
                    (projectName,standardFailMsg)
                    |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
                )
                |> checkFinishLoading 

            return(asyncMsg2Dispatch)
    | _ ->
        let asyncMsg2Dispatch =
            (
                (projectName,standardFailMsg)
                |> Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull
            )
            |> checkFinishLoading

        return(asyncMsg2Dispatch)
}

let cretateLoadingFinishedPopup msgs dispatch =
    let killPopupMsg =
        Popup.Types.PopupStyle.Popup_Is_Dead |>
        (
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget
        )

    let entireOkButton =
        Popup.View.simpleOkButton
                        killPopupMsg
                        dispatch

    let allMsgsCollected =
        msgs
        |> Array.collect (fun msg ->
            Popup.View.getPopupMsg msg)
    let msgAsPopup =
        (entireOkButton,allMsgsCollected)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok 

    let popupMsg2Dispatch =
        (msgAsPopup,Popup.Types.standardPositions) |>
        (
            Popup.Types.Has_Alternatives >>
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
            dispatch
        )

    popupMsg2Dispatch

let cretateLoadingPopup msgs dispatch  =

    let allMsgsCollected =
        msgs
        |> Array.collect (fun (msg,msgType) ->
            match msgType with
            | Global.Types.Spinner_Popup ->
                Popup.View.getPopupMsgSpinnerBeside msg
            | Global.Types.No_Loading_Popup_Type ->
                Popup.View.getPopupMsg msg
            | Global.Types.Progress_Popup progress ->
                Popup.View.getPopupMsgProgress msg progress
            )

    let popupMsg2Dispatch =
        (allMsgsCollected,Popup.Types.standardPositions) |>
        (
            Popup.Types.Has_No_Alternatives >>
            Global.Types.Popup_Msg_Global >>
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
            dispatch
        )

    popupMsg2Dispatch

let changeProjectStatus model project =
    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info projects ->
            let newInfo =
                projects
                |> Array.map (fun proj ->
                    if proj.Name = project.Name
                    then
                        project
                    else proj)
            Some newInfo
        | _ -> None
    | _ -> None

let isWrittenNewNugetNameValid newName project =
            let validNuGetVersionRegex = "\d{1,}.\d{1,}.\d{1,}($|-\w+)"
            let validNugetNameOpt = JsInterop.Regex.IsMatch validNuGetVersionRegex newName
            match validNugetNameOpt with
            | Some validNugetName ->
                match validNugetName with
                | true ->
                    let alreadyExistsInServer =
                        project.Existing_Packages
                        |> Array.exists (fun pckg ->
                            pckg = newName)
               
                    match alreadyExistsInServer with
                    | false ->
                        newName |>
                        (
                            Nuget_Name_Validity.Nuget_Name_Valid >>
                            Some
                        )
                            
                    | _ ->
                        Not_Valid_Nuget_Reason.Nuget_Already_In_Server |>
                        (
                            Nuget_Name_Not_Valid >>
                            Some
                        )
                | _ ->
                    Not_Valid_Nuget_Reason.Has_Wrong_Pattern |>
                    (
                        Nuget_Name_Not_Valid >>
                        Some
                    )
            | _ ->
                Not_Valid_Nuget_Reason.Has_Wrong_Pattern |>
                (
                    Nuget_Name_Not_Valid >>
                    Some
                )

let newNugetNameEvaluation projects_table info_chosen ( ev : Browser.Types.Event )=
    let newName = ev.target?value : string

    let validationRes = isWrittenNewNugetNameValid newName info_chosen

    let newInfo =
        match validationRes with
        | Some res ->
            let newNewNugetName =
                { info_chosen.Nuget_Names with New_Nuget_Name = res |> New_Nuget_Name.Has_New_Name}
    
            let newProjInfo =
                { info_chosen with Nuget_Names = newNewNugetName}
    
            newProjInfo
        | _ ->
            let newNewNugetName =
                { info_chosen.Nuget_Names with New_Nuget_Name = New_Nuget_Name.Has_No_Name}
    
            let newProjInfo =
                { info_chosen with Nuget_Names = newNewNugetName}
    
            newProjInfo
    
    match projects_table with
    | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info infos ->
            let newInfos =
                infos
                |> Array.map (fun info ->
                    if info.Name = info_chosen.Name
                    then
                        newInfo
                    else
                        info)
    
            let newProjectsStatus =
                newInfos |>
                (
                    Loganalyzer_Projects_Table.Yes_Projects_Table_Info >>
                    Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded
                )
    
            Some newProjectsStatus
        | _ -> None   
    | _ -> None

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Upgrade_NuGet.Types.Popup_Msg_Upgrade_Nuget
    )

let standardPositions = {
    Popup.Types.PosX = 250.0
    Popup.Types.PosY = 250.0
}

let killPopupMsg =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Upgrade_NuGet.Types.Popup_Msg_Upgrade_Nuget
    )

let checkIfDotNetInstalled dispatch = async {

    let popupMsg =
        "Checking if Dotnet is installed"
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000

    let commandStr = "shellCommand=cd server;cd loganalyzer;dotnet --list-sdks"

    let! res = Global.Types.request commandStr

    match res.status with
    | 200.0 ->
        let dotnetRegex = JsInterop.Regex.IsMatch "dotnet" res.responseText

        match dotnetRegex with
        | Some regResult ->
            match regResult with
            | true ->
                let getNugetInfoMsg =
                    dispatch |>
                    (
                        Check_Nuget_Server >>
                        dispatch
                    )

                getNugetInfoMsg
            | _ ->
                let exitMsg =
                    "Dotnet is not installed on the server side. Please install it and retry."
                    |> Popup.View.getPopupMsg

                let button =
                    Popup.View.simpleOkButton
                                    killPopupMsg
                                    dispatch

                let kickedOutMsg =
                    (button,exitMsg) |>
                    (
                        GlobalMsg.Go_To_Failed_Page >>
                        Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                        dispatch
                    )

                kickedOutMsg

        | _ ->
            let exitMsg =
                "Dotnet is not installed on the server side. Please install it and retry."
                |> Popup.View.getPopupMsg

            let button =
                Popup.View.simpleOkButton
                                killPopupMsg
                                dispatch

            let kickedOutMsg =
                (button,exitMsg) |>
                (
                    GlobalMsg.Go_To_Failed_Page >>
                    Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                    dispatch
                )

            kickedOutMsg
    | _ ->
        let exitMsg =
            res.responseText + ". Please refresh to return"
            |> Popup.View.getPopupMsg

        let button =
            Popup.View.simpleOkButton
                            killPopupMsg
                            dispatch

        let kickedOutMsg =
            (button,exitMsg) |>
            (
                GlobalMsg.Go_To_Failed_Page >>
                Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                dispatch
            )

        kickedOutMsg
}

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
            Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
            dispatch
        )

    kickedOutMsg

let getNuGetTableInfo dispatch = async {

    let popupMsg =
        "Locating all relevant AM.LogAnalyzer projects..."
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000
    
    let commandStr = "shellCommand=cd server;cd loganalyzer;ls"

    let! res = request commandStr

    match res.status with
    | 200.0 ->
        let regexStr = "(?<=Ericsson\.AM\.(?!sln))\w+(?!.*\.)"
        
        let matchesOpt = JsInterop.Regex.Matches regexStr res.responseText

        match matchesOpt with
        | Some matches ->
            let projectNotTest =
                matches
                |> Array.choose (fun matchStr ->
                    let isTestProjectOpt =
                        JsInterop.Regex.IsMatch ".Test" matchStr

                    match isTestProjectOpt with
                    | Some isTestProject ->
                        match isTestProject with
                        | true -> None
                        | _ -> matchStr |> Some
                    | _ -> None)
                |> Array.map (fun proj ->
                    {
                        Upgrade_NuGet.Types.Project_Name = proj
                        Upgrade_NuGet.Types.Loading_Msg = "loading info for " + proj + "."
                    }
                    |> Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Mix.Project_Loading)

            let getProjectsmsgs =
                matches
                |> Array.map (fun proj ->
                    proj |>
                    (
                        Upgrade_NuGet.Types.Get_Project_Info >>
                        delayedMessage 2000
                    ))
                    |> Batch_Upgrade_Nuget_Async

            let changeStatusMsgWithPopuWrapped =
                (
                    projectNotTest |>
                    (
                        Upgrade_NuGet.Types.Loganalyzer_Projects_Table_Status.Info_Is_Loading >>
                        Upgrade_NuGet.Types.Change_NuGet_Status
                    ),
                    dispatch
                )
                |> Send_Popup_With_New_State
                
            let msgs =
                [|
                    changeStatusMsgWithPopuWrapped
                    getProjectsmsgs
                |]

            msgs
            |> Array.iter (fun msg -> msg |> dispatch)

        | _ ->
            kickedOutTemplate dispatch res.responseText
    | _ ->
        kickedOutTemplate dispatch res.responseText

}

let changeBranchNugetUpgrade model dispatch ( ev : Browser.Types.Event ) =
    match model.Info with
    | Yes_Git_Info_Nuget repo ->
        let branchName = ev.target?value |> string

        let msgs =
            [|
                (branchName, Global.Types.getPositions ev,dispatch) |>
                (
                     Change_Current_Branch_UpgradeNuget >>
                     Global.Types.AsynSyncMix.Is_Not_Async
                )

                handleBranchNameChange branchName repo
                |> Global.Types.AsynSyncMix.Is_Not_Async

                "Updating table after branch change..." |>
                (
                    Popup.View.getPopupMsgSpinner >>
                    checkingProcessPopupMsg Popup.Types.standardPositions >>
                    Global.Types.AsynSyncMix.Is_Not_Async
                )

                (dispatch,Global.Types.App_Activity.NugetUpgrade) |>
                (
                    Obtain_New_Nuget_Info >>
                    Global.Types.delayedMessage 2000 >>
                    Global.Types.AsynSyncMix.Is_Async
                )
                
                
            |]

        msgs
        |> Array.iter (fun msg ->
            let asyncAction =
                async {
                    match msg with
                    | Global.Types.AsynSyncMix.Is_Async asynMsg ->
                        let! asyncToSync = asynMsg

                        asyncToSync |> dispatch
                    | Global.Types.AsynSyncMix.Is_Not_Async msg ->
                        msg |> dispatch
                }

            asyncAction |> Async.StartImmediate)
            
    | _ -> ()

let updateNugetTable model dispatch =
    match model.Info with
    | Types.Git_Info_Nuget.Yes_Git_Info_Nuget repo ->
        match model.Projects_Table with
        | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
            match res with
            |Loganalyzer_Projects_Table.Yes_Projects_Table_Info _ ->
                Html.div[
                    prop.className "column is-2"
                    prop.children[
                        Html.div[
                            prop.className "button"
                            prop.text "Update table"
                            prop.onClick (fun ev ->
                                let msgs =
                                    [|

                                        changeBranchNugetUpgrade model dispatch ev

                                        (dispatch,Global.Types.App_Activity.NugetUpgrade) |>
                                        (
                                            Obtain_New_Nuget_Info  >>
                                            dispatch
                                        )
                                        
                                    |]

                                msgs
                                |> Array.iter (fun msg -> msg))
                        ]
                    ]
                ]
            | _ -> Html.none
        | _ -> Html.none
    | _ -> Html.none

let saveChanges model dispatch =
    match model.Info with
    | Types.Git_Info_Nuget.Yes_Git_Info_Nuget _ ->
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
                                        Types.Msg.Save_Nuget_Info_To_Server >>
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


let checkoutNewBranch ( newBranch : string ) dispatch positions = async{

    do! Async.Sleep 2000

    let prms =
        String.Format(
            "shellCommand=cd server;cd loganalyzer;git checkout {0}",
            newBranch
        )

    let! res = request prms

    match res.status with
    | 200.0 -> ()
            
    | _ ->
        let popupMsg =
            res.responseText |>
            (
                Popup.View.getPopupMsg >>
                checkingProcessPopupMsg positions
            )

        let exitMsg =
            "You've been thrown out due to some error. Please refresh to return"
            |> Popup.View.getPopupMsg

        let button =
            Popup.View.simpleOkButton
                            killPopupMsg
                            dispatch
                        
        let! kickedOutMsg =
            (button,exitMsg) |>
            (
                GlobalMsg.Go_To_Failed_Page >>
                Types.GlobalMsg_Upgrade_Nuget >>
                delayedMessage 3000
            )

        let msgsCombined =
            [|
                popupMsg
                kickedOutMsg
            |]

        msgsCombined
        |> Array.iter (fun msg -> msg |> dispatch)
}

let getAllAvailablePackageVersions dispatch = async {

    let popupMsg =
        "Getting all NuGet package versions from http://segaeesw04.eipu.ericsson.se/nuget/Packages..."
        |> Popup.View.getPopupMsgSpinner
        |> checkingProcessPopupMsg standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000

    let! res = simpleGetRequest "http://localhost:3001/nugetinfo"

    match res.status with
    | 200.0 ->
        [|
            res.responseText |>
            (
                Nuget_Server_Is_Available >>
                Change_Nuget_Server_Info
            )

            dispatch |>
            (
                Get_All_Projects_Info
            )
        |]
        |> Array.iter (fun msg -> dispatch msg)
        
    | _ ->
        let exitMsg =
            res.responseText + " Couldn't reach NuGet server. Please refresh to return"
            |> Popup.View.getPopupMsg

        let button =
            Popup.View.simpleOkButton
                            killPopupMsg
                            dispatch

        let kickedOutMsg =
            (button,exitMsg) |>
            (
                GlobalMsg.Go_To_Failed_Page >>
                Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                dispatch
            )

        kickedOutMsg
}

let getActionsList project
                   ( arr : Global.Types.TypeString<Server_Options> [])
                   ( obj : Global.Types.TypeString<Server_Options> ) =

    let standardAnswer =
        arr
        |> Array.choose (fun opt ->
            match opt.ObjType with
            | Server_Options.Push_Nuget ->
                None
            | _ ->
                opt
                |> Some)

    match project.Nuget_Names.New_Nuget_Name with
    | New_Nuget_Name.Has_New_Name validity ->
        match validity with
        | Nuget_Name_Validity.Nuget_Name_Valid _ ->
            arr
        | _ ->
            standardAnswer
    | _ ->
        standardAnswer

let getServerActionButton ( option : Global.Types.TypeString<Server_Options>) =
    Html.option[
        prop.text option.ObjString
    ]

let currChoiceWithName =
    [|
        {
            Global.Types.ObjType = Server_Options.No_Server_Actions
            Global.Types.ObjString = "No Actions"
        }
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Deleted
            Global.Types.ObjString = "Delete"
        }
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Updated
            Global.Types.ObjString = "Replace"
        }
        {
            Global.Types.ObjType = Server_Options.Push_Nuget
            Global.Types.ObjString = "Push"
        }
    |]

let nugetServerOptionsViewItems project =
    match project.Server_Options with
    | Server_Options.No_Server_Actions ->
        {
            Global.Types.ObjType = Server_Options.No_Server_Actions
            Global.Types.ObjString = "No Actions"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )
    | Server_Options.Is_To_Be_Deleted ->
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Deleted
            Global.Types.ObjString = "Delete"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )
    | Server_Options.Is_To_Be_Updated ->
        {
            Global.Types.ObjType = Server_Options.Is_To_Be_Updated
            Global.Types.ObjString = "Replace"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )
    | Server_Options.Push_Nuget ->
        {
            Global.Types.ObjType = Server_Options.Push_Nuget
            Global.Types.ObjString = "Push"
        } |>
        (
            getActionsList project currChoiceWithName >>
            Array.map (fun option -> getServerActionButton option )
        )

let serverActionChanged project ( ev : Browser.Types.Event ) dispatch =
    let name = ev.target?value : string

    currChoiceWithName
    |> Array.tryFind (fun o ->
        o.ObjString = name)
    |> function
        | res when res.IsSome ->
            (project,res.Value.ObjType) |>
            (
                Change_Server_Action_Option >>
                dispatch
            )
        | _ ->
            ()

let loadingIntoServerStrMsgs projs =
    projs
    |> Array.choose (fun info ->
        match info.Loading_To_Server with
        | Info_Loaded_Options.Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done alternatives ->
                match alternatives with
                | Loading_To_Nuget_Server_Alternatives.Changing_Nuget_Name progress ->
                    (
                        String.Format(
                            "{0} -> {1}",
                                info.Name,
                                "Changing NuGet version name"
                        ),
                        Progress_Popup(progress)
                    )
                    |> Some
                | Loading_To_Nuget_Server_Alternatives.Executing_Nuget_Server_Command ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            info.Name,
                            "Executing NuGet server command"
                        ),
                        Spinner_Popup
                    )
                    |> Some
                | Loading_To_Nuget_Server_Alternatives.Building ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            info.Name,
                            "Waiting for build"
                        ),
                        Spinner_Popup
                    )
                    |> Some
                        
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Done res ->
                match res with
                | Loading_To_Server_Result.Loading_To_Server_Failed msg ->
                    (
                        String.Format(
                            "{0} -> {1}: {2}",
                            info.Name,
                            "Loading failed",
                            msg
                        ),
                        No_Loading_Popup_Type
                    )
                    |> Some
                | Loading_To_Server_Result.Loading_To_Server_Succeeded ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            info.Name,
                            "Changes to server succeeded!"
                        ),
                        No_Loading_Popup_Type
                    )
                    |> Some
        | _ -> None   )
    |> function
        | res when (res |> Array.length) <> 0 ->
            Some res
        | _ -> None

let anyProjsNugetServerLoading projs =
    projs
    |> Array.exists (fun proj ->
        match proj.Loading_To_Server with
        | Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done _ ->
                true
            | _ -> false
        |  _ -> false)

let allisBuild projs =
    projs
    |> Array.forall (fun info ->
        match info.Loading_To_Server with
        | Info_Loaded_Options.Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done alternatives ->
                match alternatives with
                | Loading_To_Nuget_Server_Alternatives.Changing_Nuget_Name progress ->
                    false
                | Loading_To_Nuget_Server_Alternatives.Executing_Nuget_Server_Command ->
                    false
                | Loading_To_Nuget_Server_Alternatives.Building ->
                    true
                    
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Done res ->
                match res with
                | Loading_To_Server_Result.Loading_To_Server_Failed msg ->
                    true
                | Loading_To_Server_Result.Loading_To_Server_Succeeded ->
                    false
        | _ -> false   )

let getLoadingAlternatives projs =
    projs
    |> Array.choose (fun proj ->
        match proj.Loading_To_Server with
        | Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done alternatives ->
                Some alternatives
            | _ -> None
        |  _ -> None)
    |> function
        | res when (res|>Array.length) <> 0 ->
            true
        | _ -> false

let projectsNotLoading projs =
    projs
    |> Array.choose (fun proj ->
        match proj.Loading_To_Server with
        | Loading_Info_To_Server status ->
            match status with
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done _ ->
                None
            | Loading_Nuget_Status.Loading_Nuget_Info_Is_Done res ->
                match res with
                | Loading_To_Server_Succeeded ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            proj.Name,
                            "Changes to server succeeded!"
                        ),
                        proj
                    )
                    |> Some
                    
                | Loading_To_Server_Failed msg ->
                    (
                        String.Format(
                            "{0} -> {1}",
                            proj.Name,
                            msg
                        ),
                        proj
                    )
                    |> Some
                    
        |  _ -> None)
    |> function
        | res when (res|>Array.length) <> 0 ->
            res |> Some
        | _ -> None

let newStatuses projsNotLoadingAnymore dispatch =
    projsNotLoadingAnymore
    |> Array.iter (fun proj ->
             
        let newLoadingStatusMsg =
            { proj with Loading_To_Server = Info_Loaded_Options.Not_Loading_Info_To_Server} |>
            (
                Types.Change_Project_Info >>
                dispatch
            )
        newLoadingStatusMsg 
        )

let getTableLoadPopup model dispatch =
    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Is_Loading mix ->
        let allProjectsLoaded = haveProjectsBeenLoaded mix

        let allMsgs =
            mix
            |> Array.map (fun proj ->
                match proj with
                | Loganalyzer_Projects_Table_Mix.Project_Not_Loading proj_not_loading ->
                    match proj_not_loading with
                    | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull proj_not_loading ->
                        let msg =
                            (
                                String.Format(
                                    "{0} -> Loading was successfull",
                                    proj_not_loading.Name
                                ),
                                Loading_Popup_Options.Spinner_Popup
                            )
                            
                        msg
                    | Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull (name, msg) ->
                        let msg =
                            (
                                String.Format(
                                    "{0} -> {1}",
                                    name,
                                    msg
                                ),
                                Loading_Popup_Options.Spinner_Popup
                            )
                            
                        msg

                | Loganalyzer_Projects_Table_Mix.Project_Loading proj_loading ->
                    let msg =
                        (
                            String.Format(
                                "{0} -> {1}",
                                proj_loading.Project_Name,
                                proj_loading.Loading_Msg
                            ),
                            Loading_Popup_Options.Spinner_Popup
                        )
                        

                    msg)

        match allProjectsLoaded with
        | true ->
            let allMsgsNoTypeConsideration =
                allMsgs
                |> Array.map (fun (x,_) -> x)
            cretateLoadingFinishedPopup allMsgsNoTypeConsideration dispatch
        | _ ->
            cretateLoadingPopup allMsgs dispatch
    | Info_Has_Been_Loaded table ->
        match table with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info projs ->
            match (allisBuild projs) with
            | true -> ()
            | false ->
                match (anyProjsNugetServerLoading projs) with
                | true ->
                    match (loadingIntoServerStrMsgs projs) with
                    | Some allMsgs ->
                        match (getLoadingAlternatives projs) with
                        | false ->
                            let allMsgsNoTypeConsideration =
                                allMsgs
                                |> Array.map (fun (x,_) -> x)
                            cretateLoadingFinishedPopup allMsgsNoTypeConsideration dispatch
                        | _ ->
                            cretateLoadingPopup allMsgs dispatch
                    | _ -> ()
                    
                | _ ->
                    match (projectsNotLoading projs) with
                    | Some projsNotLoadingAnymore ->
                        let msgsOnly =
                            projsNotLoadingAnymore
                            |> Array.map (fun (x,_) -> x)

                        cretateLoadingFinishedPopup msgsOnly dispatch

                        let statusOnly =
                            projsNotLoadingAnymore
                            |> Array.map (fun (_,y) -> y)

                        newStatuses statusOnly dispatch
                    | _ -> ()
        | _ -> ()
            
    | _ -> ()

let changeNameAsync project version dispatch = async {
    let reqBody =
        String.Format(
            "project=Ericsson.AM.{0}&version={1}",
            project.Name,
            version
        )

    let url = "http://localhost:3001/ChangeName"

    let! request =
        Async.FromContinuations <| fun (resolve,_,_) ->

            let xhr = Browser.XMLHttpRequest.Create()
            xhr.``open``(method = "POST", url = url)
            xhr.setRequestHeader("Content-Type","application/x-www-form-urlencoded")
            xhr.timeout <- 10000.0

            let socketResponse = ProgressSocket.connect("http://localhost:3001")
    
            match socketResponse.ErrorMessage with
            | None  ->
                socketResponse.Socket.Value
                |> ProgressSocket.addEventListener_message(fun scktMsg ->
                    let eventResult = (scktMsg :?> Global.Types.MessageType)

                    let newStatus =
                        eventResult.Progress |>
                        (
                            Loading_To_Nuget_Server_Alternatives.Changing_Nuget_Name >>
                            Loading_Nuget_Info_Is_Not_Done >>
                            Loading_Info_To_Server
                        )
                         

                    let newLoadingStatusMsg =
                        { project with Loading_To_Server = newStatus} |>
                        (
                            Types.Change_Project_Info >>
                            dispatch
                        )

                    newLoadingStatusMsg
                            
                    ) "message"
                |> ProgressSocket.addEventListener_message(fun scktMsg ->
                    let response = (scktMsg :?> FinishedType)
            
                    socketResponse.Socket.Value 
                    |> ProgressSocket.disconnect
                    |> ignore

                    resolve response
                    
                    ) "finished"
                |> ignore

            | Some error ->

                resolve
                    {
                        Status = 404
                        Msg = error
                    }

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

let changeNameRequestToMsgArray projectsWithNewNames dispatch =
    projectsWithNewNames
    |> Array.map (fun (proj,version) ->
        async {

            let! res = changeNameAsync proj version dispatch

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
                            Types.Change_Project_Info
                        )

                        Types.Build_Solution_If_Ready_Msg
                    |]
                    |> Types.Batch
                    
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
                        Types.Change_Project_Info
                    )
                return(newLoadingStatusMsg)
        })

let allProjsLoadingDecisionQuestionPopup projects =
    projects
    |> Array.choose (fun proj ->
        match proj.Server_Options with
        | Server_Options.No_Server_Actions ->
            None
        | Server_Options.Push_Nuget ->
            match proj.Nuget_Names.New_Nuget_Name with
            | New_Nuget_Name.Has_New_Name validity ->
                match validity with
                | Nuget_Name_Validity.Nuget_Name_Valid version ->
                    String.Format(
                        "{0} -> Push NuGet with version {1}",
                        proj.Name,
                        version
                    )
                    |> Some
                | _ -> None
            | _ ->
                None
        | Server_Options.Is_To_Be_Deleted ->
            String.Format(
                "{0} -> version {1} is about to be deleted from NuGet server",
                proj.Name,
                proj.Nuget_Names.CurrName
            )
            |> Some
        | Server_Options.Is_To_Be_Updated ->
            String.Format(
                "{0} -> version {1} is about to be replaced from NuGet server",
                proj.Name,
                proj.Nuget_Names.CurrName
            )
            |> Some)
    |> Array.collect (fun projMsg ->
        Popup.View.getPopupMsg projMsg)

let changeToBuildingMsgs projs =
    projs
    |> Array.map (fun proj ->
        let newStatus =
            Loading_To_Nuget_Server_Alternatives.Building |>
            (
                Loading_Nuget_Info_Is_Not_Done >>
                Loading_Info_To_Server
            )
         
        let newLoadingStatusMsg =
            { proj with Loading_To_Server = newStatus} |>
            (
                Types.Change_Project_Info 
            )

        newLoadingStatusMsg
        )
    |> Upgrade_NuGet.Types.Batch

let ChangeNugetNameAndBuildSolution projects dispatch = 
    let killPopup =
        Popup.Types.Popup_Is_Dead |>
        (
            Global.Types.Popup_Msg_Global >>
            Types.GlobalMsg_Upgrade_Nuget
        )
        
    let yesNoPopupMsg yesMsg =
        let yesNoPopupButtons =
            Popup.View.yesNoButtons yesMsg killPopup dispatch

        let popupType =
            (yesNoPopupButtons |> List.toArray,allProjsLoadingDecisionQuestionPopup projects)
            |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

        let yesNoButton =
            (
                (popupType,Popup.Types.standardPositions)
                |> Popup.Types.Has_Alternatives
            )

        yesNoButton |>
        (
            Global.Types.Popup_Msg_Global >>
            Types.GlobalMsg_Upgrade_Nuget
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

    let buildMsgs = changeToBuildingMsgs projects

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
                    changeToBuildingMsgs buildProjs
                    msgWithRequests
                |]
                |> Upgrade_NuGet.Types.Batch

            yesNoPopupMsg buildAndChangeNameMsgs
                    
        | _ -> 
            yesNoPopupMsg msgWithRequests
    | _ ->
        yesNoPopupMsg buildMsgs

let buildSolution projectsLoading = async {

    do! Async.Sleep 2000

    let reqStr = "shellCommand=cd server;cd loganalyzer;dotnet build Ericsson.AM.sln"

    let! res = request reqStr

    let buildFailedMsgs msg =
        projectsLoading
        |> Array.map (fun proj ->
            let newStatus =
                msg |>
                (
                    Loading_To_Server_Failed >>
                    Loading_Nuget_Info_Is_Done >>
                    Loading_Info_To_Server
                )
     
            let newLoadingStatusMsg =
                { proj with Loading_To_Server = newStatus} |>
                (
                    Types.Change_Project_Info 
                )
            newLoadingStatusMsg
            )

    match res.status with
    | 200.0 ->
        match (res.responseText.Contains("Build FAILED.")) with
        | true ->

            let res =
                "Build failed. Make sure you solution compiles clean!" |>
                (
                    buildFailedMsgs >>
                    Types.Batch 
                )
            return(res)

        | _ ->
            let buildSuccededMsgs =
                projectsLoading
                |> Array.map (fun proj ->
                    let newStatus =
                        Loading_To_Nuget_Server_Alternatives.Executing_Nuget_Server_Command |>
                        (
                            Loading_Nuget_Status.Loading_Nuget_Info_Is_Not_Done >>
                            Loading_Info_To_Server
                        )
             
                    let newLoadingStatusMsg =
                        { proj with Loading_To_Server = newStatus} |>
                        (
                            Types.Change_Project_Info 
                        )
                    newLoadingStatusMsg
                    )
            let res = buildSuccededMsgs |> Types.Batch

            return(res)
    | _ ->
        let res =
            res.responseText |>
            (
                buildFailedMsgs >>
                Types.Batch 
            )
        return(res)
}

let performNugetActionsToServer projs =

    let getProjNameAndVersion =
        projs
        |> Array.map (fun proj ->
            match proj.Server_Options with
            | Server_Options.Push_Nuget ->
                match proj.Nuget_Names.New_Nuget_Name with
                | New_Nuget_Name.Has_New_Name validity ->
                    match validity with
                    | Nuget_Name_Validity.Nuget_Name_Valid newName ->
                        (proj,newName)
                        |> Perform_Nuget_Action_To_Server
                    | _ ->
                        MsgNone
                        |> Types.GlobalMsg_Upgrade_Nuget
                | _ ->
                    MsgNone
                    |> Types.GlobalMsg_Upgrade_Nuget
            | Server_Options.Is_To_Be_Deleted ->
                (proj,proj.Nuget_Names.CurrName)
                |> Perform_Nuget_Action_To_Server
            | Server_Options.Is_To_Be_Updated ->
                (proj,proj.Nuget_Names.CurrName)
                |> Perform_Nuget_Action_To_Server
            | _ ->
                MsgNone
                |> Types.GlobalMsg_Upgrade_Nuget)

    getProjNameAndVersion
    |> Array.map (fun msg ->
        msg
        |> delayedMessage 2000)
    |> Batch_Upgrade_Nuget_Async
    

let performNugetActionToServerAsync proj version = async {
    do! Async.Sleep 2000

    let reqStrBase = "shellCommand=cd server;cd loganalyzer;"

    let nugetPushTemplate projName version =
        String.Format(
            "dotnet nuget push \"Ericsson.AM.{0}/bin/Debug/Ericsson.AM.{0}.{1}.nupkg\" -k 875e5930-56d2-4f06-9fe9-f3f5a8c09aa2 -s http://segaeesw04.eipu.ericsson.se/nuget",
            projName,
            version
        )

    let nugetDeleteTemplate projName version =
        String.Format(
            "yes|dotnet nuget delete Ericsson.AM.{0}.{1} -k 875e5930-56d2-4f06-9fe9-f3f5a8c09aa2 -s http://segaeesw04.eipu.ericsson.se/nuget;y",
            projName,
            version
        )

    let failedMsg msg =
        let newStatus =
            msg |>
            (
                Loading_To_Server_Failed >>
                Loading_Nuget_Status.Loading_Nuget_Info_Is_Done >>
                Loading_Info_To_Server
            )
             
        let newLoadingStatusMsg =
            { proj with Loading_To_Server = newStatus} |>
            (
                Types.Change_Project_Info 
            )

        newLoadingStatusMsg

    let getErrorMsg txt altMsg =
        JsInterop.Regex.Match "(?<=error:)(.|\n)*?(?=\n\s)" txt
        |> fun x ->
            match x with
            | Some m -> m
            | _ -> altMsg

    let pusProcedure pushReqString = async {
        let! pushRes = request pushReqString
        
        match pushRes.status with
        | 200.0 ->
            match (pushRes.responseText.Contains("Your package was pushed.")) with
            | false ->
                
                let errorMsg =
                    getErrorMsg pushRes.responseText "couldn't push NuGet package"

                return(failedMsg errorMsg)
        
            | _ ->
                let newStatus =
                    Loading_To_Server_Result.Loading_To_Server_Succeeded |>
                    (
                        Loading_Nuget_Status.Loading_Nuget_Info_Is_Done >>
                        Loading_Info_To_Server
                    )
                     
                let newLoadingStatusMsg =
                    { proj with Loading_To_Server = newStatus} |>
                    (
                        Types.Change_Project_Info 
                    )

                return(newLoadingStatusMsg)
        | _ ->

            return(failedMsg pushRes.responseText)
    }
        
    let deleteReqString =
        reqStrBase + (nugetDeleteTemplate proj.Name version)

    let pushReqString =
        reqStrBase + (nugetPushTemplate proj.Name version)

    match proj.Server_Options with
    | Server_Options.Is_To_Be_Updated ->

        let! deleteResp = request deleteReqString
        
        match deleteResp.status with
        | 200.0 ->
            match (deleteResp.responseText.Contains("was deleted successfully.")) with
            | false ->
                
                let errorMsg =
                    getErrorMsg deleteResp.responseText "couldn't delete NuGet before pushing"

                return(failedMsg errorMsg)
        
            | _ ->
                let! res = pusProcedure pushReqString

                return(res)
        | _ ->
            return(failedMsg deleteResp.responseText)
            
    | Server_Options.Push_Nuget ->
        let! res = pusProcedure pushReqString
        
        return(res)
    | Server_Options.Is_To_Be_Deleted ->
        let! deleteResp = request deleteReqString
        
        match deleteResp.status with
        | 200.0 ->
            match (deleteResp.responseText.Contains("was deleted successfully.")) with
            | false ->
                
                let errorMsg =
                    getErrorMsg deleteResp.responseText "couldn't delete NuGet before pushing"

                return(failedMsg errorMsg)
        
            | _ ->
                let newStatus =
                    Loading_To_Server_Result.Loading_To_Server_Succeeded |>
                    (
                        Loading_Nuget_Status.Loading_Nuget_Info_Is_Done >>
                        Loading_Info_To_Server
                    )
                     
                let newLoadingStatusMsg =
                    { proj with Loading_To_Server = newStatus} |>
                    (
                        Types.Change_Project_Info 
                    )

                return(newLoadingStatusMsg)
        | _ ->
            return(failedMsg deleteResp.responseText)
    | _ ->
        return(MsgNone |> Types.GlobalMsg_Upgrade_Nuget)
        
}

let decideifBuild model =
    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded res ->
        match res with
        | Loganalyzer_Projects_Table.Yes_Projects_Table_Info projs ->
            let projectsLoading =
                projs
                |> Array.choose (fun proj ->
                    match proj.Loading_To_Server with
                    | Loading_Info_To_Server _ ->
                        Some proj
                    |  _ -> None)

            match (projectsLoading |> Array.length) with
            | 0 ->
                model, Global.Types.MsgNone, []
            | _ ->
                match (allisBuild projectsLoading) with
                | true ->
                    let buildingInformation =
                        "Building Ericsson.AM solution..." |>
                        (
                            Popup.View.getPopupMsgSpinner >>
                            checkingProcessPopupMsg standardPositions >>
                            delayedMessage 2000
                        )

                    let msg =
                        [|
                            buildingInformation

                            projectsLoading |>
                            (
                                buildSolution 
                            )
                        |] |>
                        (
                            Batch_Upgrade_Nuget_Async >>
                            Cmd.ofMsg
                        )
                        
                    model, Global.Types.MsgNone, msg
                | _ ->
                    model, Global.Types.MsgNone, []
        | _ ->
            model, Global.Types.MsgNone, []
    | _ ->
        model, Global.Types.MsgNone, []

    
    


        






