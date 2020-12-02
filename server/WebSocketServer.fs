module WebSocketServer

open Elmish
open Elmish.Bridge
open SharedTypes.Shared
open Saturn
open System.IO



let init (clientDispatch:Dispatch<SharedTypes.Shared.ClientMsg>) ()=
    {
        CurrAction = SharedTypes.Shared.None
    }, Cmd.none

let update (clientDispatch : Dispatch<SharedTypes.Shared.ClientMsg>) msg (model : BridgeModel) =
    match msg with
    | ChangeNuGet nugetModel ->

        ChangeNuGetName.update nugetModel ChangeNuGetName.Initialize clientDispatch

        model, Cmd.none