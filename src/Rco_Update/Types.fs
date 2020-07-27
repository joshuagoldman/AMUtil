module Rco_Update.Types

open Global.Types
open Browser
open Thoth.Json

type RcoObject = {
    ReleaseDate : string
    RcoDocument : string
    RcoRevision : string
    BarcodeText : string
    Slogan : string
    ProductNumber : string
    ProductGroup : string
    RStateIn : string
    RStateOut : string
    RcLatEvaluate : string
    RcLatTextOut : string
    ScPrttEvaluate : string
    ScPrttTextOut : string
    CloudLatEvaluate : string
    CloudLatTextOut : string
    ExecutionOrder : string
    MfgDateFrom : string
    MfgDateTo : string
    ProductFamily : string
    Closed : string
    Cost : string
    Comments : string
};

let RcoObjectDecoder : Decoder<RcoObject> = 
    Decode.object (fun fields -> {
        ReleaseDate = fields.Required.At ["ReleaseDate"] Decode.string
        RcoDocument = fields.Required.At ["RcoDocument"] Decode.string
        RcoRevision = fields.Required.At ["RcoRevision"] Decode.string
        BarcodeText = fields.Required.At ["BarcodeText"] Decode.string
        Slogan = fields.Required.At ["Slogan"] Decode.string
        ProductNumber = fields.Required.At ["ProductNumber"] Decode.string
        ProductGroup = fields.Required.At ["ProductGroup"] Decode.string
        RStateIn = fields.Required.At ["RStateIn"] Decode.string
        RStateOut = fields.Required.At ["RStateOut"] Decode.string
        RcLatEvaluate = fields.Required.At ["RcLatEvaluate"] Decode.string
        RcLatTextOut = fields.Required.At ["RcLatTextOut"] Decode.string
        ScPrttEvaluate = fields.Required.At ["ScPrttEvaluate"] Decode.string
        ScPrttTextOut = fields.Required.At ["ScPrttTextOut"] Decode.string
        CloudLatEvaluate = fields.Required.At ["CloudLatEvaluate"] Decode.string
        CloudLatTextOut = fields.Required.At ["CloudLatTextOut"] Decode.string
        ExecutionOrder = fields.Required.At ["ExecutionOrder"] Decode.string
        MfgDateFrom = fields.Required.At ["MfgDateFrom"] Decode.string
        MfgDateTo = fields.Required.At ["MfgDateTo"] Decode.string
        ProductFamily = fields.Required.At ["ProductFamily"] Decode.string
        Closed = fields.Required.At ["Closed"] Decode.string
        Cost = fields.Required.At ["Cost"] Decode.string
        Comments = fields.Required.At ["Comments"] Decode.string
    })

let RcoObjecArrayDecoder : Decoder<RcoObject[]> =
    Decode.array RcoObjectDecoder

let parseRcoData (json : string ) =
    Decode.fromString RcoObjecArrayDecoder json

type Git_Info =
    | No_Git_Info
    | Yes_Git_info of Git_Repository

type Curr_Rco_File =
    | No_Rco_File
    | Yes_Rco_File of Types.File

type CorrectionValue<'a> =
    | Correction of 'a
    | No_Correction

type RcoFaultInfo = {
    Line : int
    LineInfo : RcoObject
    Correction : CorrectionValue<string>
}

type NeedsCorrection =
    | No_Correction_Needed of RcoObject[]
    | Correction_Needed of RcoObject[] * RcoFaultInfo[]

type Curr_Rco_Info =
    | No_Rco_Info
    | Yes_Rco_Info of NeedsCorrection

type Msg =
    | Batch of Msg[]
    | Get_New_Info_Msg of Git_Info
    | Change_File_Msg of Curr_Rco_File
    | Change_Current_Branch_Msg of Branch_Name : string * Popup.Types.PopupPosition * (Msg -> unit) 
    | Change_Current_Rco_Info of Curr_Rco_Info
    | Global_Msg of GlobalMsg
    | Get_Rco_Data_Msg of (Msg -> unit) * Types.Event 
    | Investigate_Issues_Rco_Files_Msg of Popup.Types.PopupPosition * RcoObject[] * (Msg -> unit)
    | Update_Rco_Changes of RcoObject[] * RcoFaultInfo[] * Popup.Types.PopupPosition * (Msg -> unit)
    | Save_New_Rco_Info of RcoObject[] * Popup.Types.PopupPosition * (Msg -> unit)
    | Change_RCO_Fault_Arr of RstateIn: string *
                              RcoFaultInfo *
                              RcoObject[] *
                              Popup.Types.PopupPosition *
                              (Msg -> unit)

type Model = {
    Info : Git_Info
    CurrFile : Curr_Rco_File
    CurrRcoInfo : Curr_Rco_Info
}

