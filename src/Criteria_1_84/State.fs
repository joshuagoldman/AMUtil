module Criteria_1_84.State

open Elmish
open JsInterop
open Fable.Core.JsInterop
open Criteria_1_84.Types

let init result =
    {
        Types.Something = ""
    }


let update msg (model:Model) : Types.Model * Global.Types.GlobalMsg * Cmd<Msg> =
    match msg with
    | Batch_Criteria_1_84 msgs ->
        let msg =
            msgs
            |> Array.map (fun x -> x |> Cmd.ofMsg)
            |> Cmd.batch

        model, Global.Types.MsgNone, msg
