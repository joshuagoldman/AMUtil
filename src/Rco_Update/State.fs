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

    | Change_Current_Branch_Rco(branch_name,positions,dispatch) ->
        match model.Info with
        | No_Git_Info ->
            model, Global.Types.MsgNone,[]
        | Yes_Git_info info ->
            let newInfo =
                { info with CurrBranch = branch_name }

            let spreadMsg =
                newInfo
                |> Global.Types.GlobalMsg.Spread_New_Branch_Name

            Logic.checkoutNewBranch branch_name dispatch positions  
            |> Async.StartImmediate

            model, spreadMsg,[]
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
    | Save_New_Rco_Info(rcoObjArr,postions,dispatch) ->
        match model.CurrFile with
        | Yes_Rco_File(_,rco_type) ->
            rcoObjArr
            |> Logic.updateFile rco_type dispatch postions
            |> Async.StartImmediate
        | _ ->
            ()
        model, Global.Types.MsgNone,[]

    | Update_Rco_Changes(info,faults,positions,dispatch) ->
        let newRcoObjArr =
            Logic.modifyRcoLines info faults

        let resultMsg =
            Investigate_Issues_Rco_Files_Msg(positions,newRcoObjArr,dispatch)
            |> Cmd.ofMsg

        model,Global.Types.MsgNone,resultMsg
    | Change_RCO_Fault_Arr newFault ->
        match model.CurrRcoInfo with
        | Yes_Rco_Info correction ->
            match correction with
            | NeedsCorrection.Correction_Needed(rcoObjArr,faultsArr) ->
                let newfaultsArr =
                    faultsArr
                    |> Array.map (fun fault ->
                        if fault.Line = newFault.Line
                        then
                            newFault
                        else
                            fault)

                let newRcoInfo =
                    (rcoObjArr,newfaultsArr) |>
                    (
                        NeedsCorrection.Correction_Needed >>
                        Yes_Rco_Info
                    )

                { model with CurrRcoInfo = newRcoInfo },Global.Types.MsgNone,[]
                    
            | _ ->
                model,Global.Types.MsgNone,[]
        | _ ->
            model,Global.Types.MsgNone,[]

    | Change_RCO_File_Type ev ->
        let newRcoType = ev.target?value : string

        let rcoTypeConverter ( strVal  : string ) =
            match (strVal.Replace(" ","")) with
            |  "RBS6000" -> RBS_6000
            | _ -> ERS
        match model.CurrFile with
        | Yes_Rco_File(file,_) ->
            let newFile =
                (file,newRcoType |> rcoTypeConverter)
                |> Yes_Rco_File

            {model with CurrFile = newFile},Global.Types.MsgNone,[]
        | _ ->
            model,Global.Types.MsgNone,[]

        
        
