module WebSocketServer

open Elmish
open Elmish.Bridge
open SharedTypes.Shared

let init (clientDispatch:Dispatch<SharedTypes.Shared.BridgeMsg>) () =
    {
        CurrAction = SharedTypes.Shared.None
    }, Cmd.none

let update (clientDispatch:Dispatch<SharedTypes.Shared.BridgeMsg>) msg (model : BridgeModel) =
    match msg with 
    | ChangeAction action ->
        action
        |> ChangeAction
        |> clientDispatch 

        let newModel = { model with CurrAction = action }

        newModel, Cmd.none



let server =
  Bridge.mkServer SharedTypes.Shared.endpoint init update
  |> Bridge.run Giraffe.server