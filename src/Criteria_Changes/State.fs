module Criteria_Changes.State

open Elmish
open JsInterop
open Fable.Core.JsInterop
open Criteria_Changes.Types

let init result =
    {
        Types.Info = Types.No_Git_Info_Criteria_Changes
        ExcelInfo = Rel_Plan_Log_Analysis.No_Rel_Plan_Log_Analysis
        CurrFIle = Curr_Rel_Plan_Log_Analysis_File.No_Log_Analysis_File
    }


let update msg (model:Model) : Types.Model * Global.Types.GlobalMsg * Cmd<Msg> =
    match msg with
    | Batch_Criteria_Changes msgs ->
        let msg =
            msgs
            |> Array.map (fun x -> x |> Cmd.ofMsg)
            |> Cmd.batch

        model, Global.Types.MsgNone, msg
    | Global_Msg_Criteria_Changes global_msg ->
        model, global_msg, []

    | Update_Current_Branch_Name new_name ->
        match model.Info with
        | Yes_Git_Info_Criteria_Changes repo ->
            let newState =
                { repo with CurrBranch = new_name }
                |> Yes_Git_Info_Criteria_Changes
            { model with Info = newState } , Global.Types.MsgNone, []
        | _ ->
            model, Global.Types.MsgNone, []

    | Change_File_Msg file ->
        { model with CurrFIle = file }, Global.Types.MsgNone, []

    | Change_Curr_Release info ->
        { model with ExcelInfo = info }, Global.Types.MsgNone, []
