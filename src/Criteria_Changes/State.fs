module Criteria_Changes.State

open Elmish
open JsInterop
open Fable.Core.JsInterop
open Criteria_Changes.Types

let init result =
    {
        Types.Info = Types.No_Git_Info_Criteria_Changes
    }


let update msg (model:Model) : Types.Model * Global.Types.GlobalMsg * Cmd<Msg> =
    match msg with
    | Batch_Criteria_Changes msgs ->
        let msg =
            msgs
            |> Array.map (fun x -> x |> Cmd.ofMsg)
            |> Cmd.batch

        model, Global.Types.MsgNone, msg
    | Change_Current_Branch_Criteria_Changes(branch_name,positions,dispatch) ->
        match model.Info with
        | No_Git_Info_Criteria_Changes ->
            model, Global.Types.MsgNone,[]
        | Yes_Git_Info_Criteria_Changes info ->
            let newInfo =
                { info with CurrBranch = branch_name }

            let spreadMsg =
                newInfo
                |> Global.Types.GlobalMsg.Spread_New_Branch_Name

            Logic.checkoutNewBranch branch_name dispatch positions  
            |> Async.StartImmediate

            model, spreadMsg,[]
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
