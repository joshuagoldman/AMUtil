module Criteria_Changes.Types

open Global.Types
open Browser
open Thoth.Json

type Evaluation_Status<'a> =
    | Has_Been_Evaluated of 'a
    | Has_Not_Been_Evaluated

type Log_Search_Criteria_Excel_Info = {
    Document_Number : string
    Revision : string
    Released : string
}

type Log_Search_Criteria_Info = {
    Excel_Info : Log_Search_Criteria_Excel_Info
    Info_Text : string
}

let RcoObjectDecoder : Decoder<Log_Search_Criteria_Excel_Info> = 
    Decode.object (fun fields -> {
        Document_Number = fields.Required.At ["ReleaseDate"] Decode.string
        Revision = fields.Required.At ["RcoDocument"] Decode.string
        Released = fields.Required.At ["RcoRevision"] Decode.string
    })

let RcoObjecArrayDecoder : Decoder<Log_Search_Criteria_Excel_Info[]> =
    Decode.array RcoObjectDecoder

let parseExcelInfo (json : string ) =
    Decode.fromString RcoObjecArrayDecoder json

type Git_Info_Criteria_Changes =
    | Yes_Git_Info_Criteria_Changes of Global.Types.Git_Repository
    | No_Git_Info_Criteria_Changes

type Rel_Plan_Log_Analysis =
    | Yes_Rel_Plan_Log_Analysis of Log_Search_Criteria_Info[] * CurrInfos: Log_Search_Criteria_Info []
    | No_Rel_Plan_Log_Analysis

type Curr_Rel_Plan_Log_Analysis_File =
    | No_Log_Analysis_File
    | Yes_Log_Analysis_File of Types.File

type Msg =
    | Batch_Criteria_Changes of Msg []
    | Async_Msg_Criteria_Changes of Async<Msg>
    | Global_Msg_Criteria_Changes of GlobalMsg
    | Update_Current_Branch_Name of string
    | Change_File_Msg of Curr_Rel_Plan_Log_Analysis_File
    | Change_Curr_Release of Rel_Plan_Log_Analysis
    | Extract_Info_Text_Criteria_File of Log_Search_Criteria_Excel_Info [] * (Msg -> unit)

type Model = {
    Info : Git_Info_Criteria_Changes
    ExcelInfo : Rel_Plan_Log_Analysis
    CurrFile : Curr_Rel_Plan_Log_Analysis_File
}

