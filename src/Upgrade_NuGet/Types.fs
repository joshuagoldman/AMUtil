module Upgrade_NuGet.Types

open Global.Types
open Browser
open Thoth.Json

type Git_Info_Nuget =
    | Yes_Git_Info_Nuget of Global.Types.Git_Repository
    | No_Git_Info_Nuget

type Git_Change_Rows = {
    Row_Number : string
    Row_Content : string
}

type Project_Changes_File = {
    File_Name : string
    Plus_Rows : Git_Change_Rows[]
    Minus_Rows : Git_Change_Rows[]
}

type Project_Changes =
    | Project_Has_Changes of Project_Changes_File[]
    | Project_Has_No_Changes

type Project_Chosen_option =
    | Project_Chosen
    | Project_Not_Chosen

type Nuget_Name_Validity =
    | Nuget_Name_Valid of string
    | Nuget_Name_Not_Valid

type New_Nuget_Name =
    | Has_No_Name
    | Has_New_Name of Nuget_Name_Validity

type Nuget_Names = {
    CurrName : string
    New_Nuget_Name : New_Nuget_Name
}

type Project_Info = {
    Name : string
    Is_Chosen : Project_Chosen_option
    Changes : Project_Changes
    Nuget_Names : Nuget_Names
}

type Loganalyzer_Projects_Table =
    | Yes_Projects_Table_Info of Project_Info[]
    | No_Projects_Table_Info

type Loganalyzer_Projects_Table_Loading = {
    Project_Name : string
    Loading_Msg : string
}

type Loganalyzer_Projects_Table_Result =
    | Loading_Was_Successfull of Project_Info
    | Loading_Was_Not_Successfull of Proj_Name : string

type Loganalyzer_Projects_Table_Mix =
    | Project_Not_Loading of Loganalyzer_Projects_Table_Result
    | Project_Loading of Loganalyzer_Projects_Table_Loading

type Loganalyzer_Projects_Table_Status =
    | Info_Not_Loaded
    | Info_Is_Loading of Loganalyzer_Projects_Table_Mix []
    | Info_Has_Been_Loaded of Loganalyzer_Projects_Table

type Msg =
    | Batch of Msg[]
    | Batch_Upgrade_Nuget_Async of Async<Msg>[]
    | Popup_Msg_Upgrade_Nuget of Popup.Types.PopupStyle
    | GlobalMsg_Upgrade_Nuget of GlobalMsg
    | Change_Project_Info of Project_Info
    | New_Nuget_Name_Change of Project_Info * Types.Event
    | Change_NuGet_Status of Loganalyzer_Projects_Table_Status
    | Change_Project_Status of Project_Info
    | Get_Project_Info of string
    | Obtain_New_Nuget_Info of (Msg -> unit) * App_Activity
    | Get_All_Projects_Info of (Msg -> unit)
type Model = {
    Info : Git_Info_Nuget
    Projects_Table : Loganalyzer_Projects_Table_Status
}

