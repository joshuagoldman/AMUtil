module Upgrade_NuGet.State

open Elmish
open Types
open JsInterop
open Fable.Core.JsInterop

let init result =
    {
        Info = Git_Info_Nuget.No_Git_Info_Nuget
        Projects_Table = No_Projects_Table_Info
    }


let update msg (model:Model) : Types.Model * Global.Types.GlobalMsg * Cmd<Msg> =
    match msg with
    | Batch msgs ->
        let msgsIntoOneCmd =
            msgs
            |> Array.map (fun msg ->
                msg |> Cmd.ofMsg)
            |> Cmd.batch

        model, Global.Types.MsgNone, msgsIntoOneCmd
        
    | GlobalMsg_Upgrade_Nuget msg ->
        model, msg, []
