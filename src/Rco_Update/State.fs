module Rco_Update.State

open Elmish
open Types
open JsInterop
open Fable.Core.JsInterop

let init result =
    {
        Info = No_Git_Info
        CurrFile = No_Rco_File
        CurrRcoInfo = No_Rco_Info
    }


let update msg (model:Model) : Types.Model * Global.Types.GlobalMsg * Cmd<Msg> =
    match msg with
    | Batch msgs ->
        let msg =
            msgs
            |> Array.map (fun x -> x |> Cmd.ofMsg)
            |> Cmd.batch

        model, Global.Types.MsgNone, msg
            
    | Global_Msg glob_msg ->
        model, glob_msg,[]
    | Get_New_Info_Msg info ->
        { model with Info = info }, Global.Types.MsgNone,[]

    | Change_File_Msg fileOpt ->
        { model with CurrFile = fileOpt }, Global.Types.MsgNone,[]

    | Change_Current_Branch_Msg branch_name ->
        match model.Info with
        | No_Git_Info ->
            model, Global.Types.MsgNone,[]
        | Yes_Git_info info ->
            let newInfo =
                { info with CurrBranch = branch_name }
                |> Yes_Git_info

            { model with Info = newInfo }, Global.Types.MsgNone,[]
    | Change_Current_Rco_Info info ->
        { model with CurrRcoInfo = info }, Global.Types.MsgNone,[]
    | Get_Rco_Data_Msg(dispatch,ev) ->
        let positions =
            {
                Popup.Types.PosX = ( ev?pageX : float )
                Popup.Types.PosY = ( ev?pageY : float )
            }

        Logic.getFileListData model dispatch positions
        |> Async.StartImmediate

        model, Global.Types.MsgNone,[]
    | Investigate_Issues_Rco_Files_Msg(popupPosition,rcoObjArr,dispatch)->
        let msgs =
            Logic.findFaultsInRcoFile rcoObjArr popupPosition dispatch
            |> Cmd.fromAsyncSeveral

        model, Global.Types.MsgNone, msgs
    | Save_New_Rco_Info rcoObj ->
        model, Global.Types.MsgNone,[]

        
        
