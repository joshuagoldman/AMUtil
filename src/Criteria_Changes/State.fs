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
