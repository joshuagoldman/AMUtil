module Main.Types

open Global.Types

type GitLog = {
    Message : string
    Commit : string
    Date : string
}

type GitBranch = {
    Name : string
    Log : GitLog []
}

type Msg =
    | Batch_Main of Msg []
    | MsgNone_Main
    | GlobalMsg_Main of GlobalMsg
    | Rco_Update_Msg of Rco_Update.Types.Msg
    | Upgrade_NuGet_Msg of Upgrade_NuGet.Types.Msg
    | Check_If_Git_Installed_Msg of CheckProcess<(Msg -> unit), Git_Installed_Result>
    | Check_Origin_Accessibility_Msg of CheckProcess<(Msg -> unit),Origin_Access_Result>
    | Check_If_Repo_Cloned_Msg of CheckProcess<(Msg -> unit),Git_Repo_Cloned_Result>
    | Clone_Repo_Msg of CheckProcess<(Msg -> unit),Git_Repo_Cloned_Result>
    | Check_Repo_Parsing_Msg of CheckProcess<(Msg -> unit),Git_Repo_Parsing_Result>
    | Popup_Msg of Popup.Types.PopupStyle
    | Change_Activity of App_Activity
    | Spread_New_Branch_Name_Main of Git_Repository
    | Spread_New_Git_Repo_Main of Git_Repository
    | Obtain_New_Nuget_Info of (Msg -> unit) * Global.Types.App_Activity
    | Get_All_Projects_Info of (Msg -> unit)


type Model = {
    CurrentRcoFile : string
    CurrentOption : PageOption
    Activity : App_Activity
    Rco_Update : Rco_Update.Types.Model
    Upgrade_NuGet : Upgrade_NuGet.Types.Model
}

