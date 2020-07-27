namespace JsInterop

open Fable.Core
open Elmish

module Regex =

    /// <summary>Returns the first regex match if it exists, otherwise a None value is returned</summary>
    let Match (pattern:string) (input:string) : string Option = Fable.Core.JsInterop.import "Match" "./JsInterop/Regex.js"
   
    /// <summary>Returns a string array of all matches, if none exist, a None value is returned</summary>
    let Matches (pattern:string) (input : string) : string[] Option = Fable.Core.JsInterop.import "Matches" "./JsInterop/Regex.js"
   
    /// <summary>Returns true if any matches exists, false if none exist, and None is an error occurs</summary>
    let IsMatch (pattern:string) (input : string) : bool Option = Fable.Core.JsInterop.import "IsMatch" "./JsInterop/Regex.js"

type IProgress =
    abstract getProg : string -> IProgress option
    abstract percentage : float
    abstract remaining : string

type [<AllowNullLiteral>] IWebsocket =
    abstract addEventListener_message: listener: (IProgress -> unit) -> socketObj: IWebsocket -> IWebsocket
    abstract emit: eventName: string -> message: obj -> socketObj: IWebsocket -> IWebsocket

type SocketResponse = {
    Socket : IWebsocket option
    ErrorMessage : string option
}

[<Erase>]
module ProgressSocket =

    [<Import("createSocket", "./JsInterop/Socket.js")>] 
    let connect (url : string) : SocketResponse = jsNative

    [<Import("addEventListener", "./JsInterop/Socket.js")>] 
    let addEventListener_message (handler: obj -> unit) (eventName: string) (socket: IWebsocket) : IWebsocket = jsNative

    [<Import("disconnect", "./JsInterop/Socket.js")>] 
    let disconnect (socketObj: IWebsocket) : unit = jsNative

    [<Import("emit", "./JsInterop/Socket.js")>] 
    let emit (eventName: string) (message: obj) (socketObj: IWebsocket) : IWebsocket = jsNative

module Cmd =
    let fromAsyncSeveral ( operation : Async<'msg[]> ) : Cmd<'msg> =
        let delayedCmd ( dispatch: 'msg -> unit ) : unit =
            let delayedDispatch = async {
                    let! msgs = operation
                    msgs
                    |> Array.iter (fun x -> x |> dispatch)
             }
    
            Async.StartImmediate delayedDispatch
    
        Cmd.ofSub delayedCmd
    
    let fromAsync ( operation : Async<'msg> ) : Cmd<'msg> =
        let delayedCmd ( dispatch: 'msg -> unit ) : unit =
            let delayedDispatch = async {
                    let! msg = operation
                    dispatch msg
             }
    
            Async.StartImmediate delayedDispatch
    
        Cmd.ofSub delayedCmd
