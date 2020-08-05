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

type Msg =
    | Batch of Msg[]
    | GlobalMsg_Upgrade_Nuget of GlobalMsg
    | Change_NuGet_Status of Project_Info

type Model = {
    Info : Git_Info_Nuget
    Projects_Table : Loganalyzer_Projects_Table
}

