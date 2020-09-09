module Criteria_Changes.Types

open Global.Types
open Browser
open Thoth.Json

type Git_Info_Criteria_Changes =
    | Yes_Git_Info_Criteria_Changes of Global.Types.Git_Repository
    | No_Git_Info_Criteria_Changes

type Evaluation_Status<'a> =
    | Has_Been_Evaluated of 'a
    | Has_Not_Been_Evaluated

type Log_Search_Criteria_Info = {
        Document_Number : string
        Revision : string
        Released : string
        InfoText : Evaluation_Status<string>
}

type Rel_Plan_Log_Analysis =
    | Yes_Rel_Plan_Log_Analysis of Log_Search_Criteria_Info[] * CurrInfo: Log_Search_Criteria_Info
    | No_Rel_Plan_Log_Analysis

type Curr_Rel_Plan_Log_Analysis_File =
    | No_Log_Analysis_File
    | Yes_Log_Analysis_File of Types.File

type Msg =
    | Batch_Criteria_Changes of Msg []
    | Global_Msg_Criteria_Changes of GlobalMsg
    | Update_Current_Branch_Name of string
    | Change_File_Msg of Curr_Rel_Plan_Log_Analysis_File
    | Change_Curr_Release of Rel_Plan_Log_Analysis

type Model = {
    Info : Git_Info_Criteria_Changes
    ExcelInfo : Rel_Plan_Log_Analysis
    CurrFIle : Curr_Rel_Plan_Log_Analysis_File
}

