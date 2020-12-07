module WebSocketServer

open Elmish
open Elmish.Bridge
open SharedTypes.Shared
open Giraffe.Core

let init _ ()=
    {
        CurrAction = SharedTypes.Shared.None
    }, Cmd.none

let update (clientDispatch : Dispatch<SharedTypes.Shared.ClientMsg>) msg (model : BridgeModel) =
    match msg with
    | ChangeNuGet nugetModelArr ->
        let actionsAsync =
            nugetModelArr
            |> Array.map (fun nugetModel ->
                    async {
                        ChangeNuGetName.update nugetModel ChangeNuGetName.Initialize clientDispatch
                    }
                )
        
        actionsAsync
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        

        model, Cmd.none
    | ServerMsgNone -> 
        model,Cmd.none
    | TestMsg ->
        System.Console.Write "Test message to socket server from client!"
        model,Cmd.none

let server : HttpHandler =
  Bridge.mkServer "" init update
  |> Bridge.whenDown ServerMsgNone
  |> Bridge.run Giraffe.server