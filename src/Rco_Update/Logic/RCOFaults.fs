module Rco_Update.Logic.RCOFaults

open JsInterop
open Elmish
open Fable.Import
open System
open Global.Types
open Feliz
open Fable.Remoting.Client
open SharedTypes

let findFaultsInRcoFile rcoObjectArr ( popupPosition : Popup.Types.PopupPosition ) dispatch = async {

    let chcekingFaultsInformationMsg =
        "Checking faults in RCO file..." |>
        (
            Popup.View.getPopupMsgSpinner >>
            Rco_Update.Logic.Miscellaneous.checkingProcessPopupMsg popupPosition >>
            dispatch
        )

    chcekingFaultsInformationMsg

    let forbiddenRstateRegex = "(\+|-|^)R[0-9]{1,2}([A-Z]|$)|All$"
    
    let faultyLines =
        rcoObjectArr
        |> Array.indexed
        |> Array.choose (fun (pos,line) ->
            let rstateAllowedOpt =
                JsInterop.Regex.IsMatch forbiddenRstateRegex line.RStateIn
            match rstateAllowedOpt with
            | Some rstateAllowed ->
                if rstateAllowed |> not
                then
                    {
                        Rco_Update.Types.Line = pos
                        Rco_Update.Types.LineInfo = line
                        Rco_Update.Types.Correction = Rco_Update.Types.No_Correction
                    }
                    |> Some
                else
                    None    
            | _ ->
                None
        )

    match faultyLines.Length with
    | 0 ->
        let saveNewRcoFileMsg =
            (rcoObjectArr,popupPosition,dispatch) 
            |> Rco_Update.Types.Save_New_Rco_Info
            |> fun x -> [|x|]
    
        return(saveNewRcoFileMsg)
        
    | _ ->
        let errorMsg =
            "Some faulty RCO lines were found. Would you like to fix the faults?"
    
        let correctionMsg =
            [|
                Rco_Update.Logic.Miscellaneous.killPopupMsg

                (rcoObjectArr,faultyLines) |>
                (
                    Rco_Update.Types.NeedsCorrection.Correction_Needed >>
                    Rco_Update.Types.Curr_Rco_Info.Yes_Rco_Info >>
                    Rco_Update.Types.Change_Current_Rco_Info 
                )
            |]
            |> Rco_Update.Types.Batch

        let saveNewRcoInfoMsg =
            [|
                Rco_Update.Logic.Miscellaneous.killPopupMsg

                (rcoObjectArr,popupPosition,dispatch) 
                |> Rco_Update.Types.Save_New_Rco_Info
            |]
            |> Rco_Update.Types.Batch
            

        let buttonInfoArr =
            [|
                {
                    Popup.Types.Name = "Yes, I'll fix them"
                    Popup.Types.Msg = correctionMsg
                }
                {
                    Popup.Types.Name = "No, just save the data"
                    Popup.Types.Msg = saveNewRcoInfoMsg
                }
            |]

        let! faultsFoundMsg =
            Rco_Update.Logic.Miscellaneous.alternaitvesPopup
                errorMsg
                buttonInfoArr
                dispatch
                popupPosition
    
        return([|faultsFoundMsg|])
}

let modifyRcoLines rcoObjArr ( faults : Rco_Update.Types.RcoFaultInfo[] ) =
    let newRcoObjectArr =
        rcoObjArr
        |> Array.indexed
        |> Array.map (fun (pos,line) ->
            faults
            |> Array.tryPick (fun faultLine ->
                if faultLine.Line = pos
                then
                    Some faultLine
                else
                    None)
            |> function
                | res when res.IsSome ->
                    match res.Value.Correction with
                    | Rco_Update.Types.Correction rstateInNew ->
                        { line with RStateIn = rstateInNew}
                    | _ -> line
                
                | _ -> line
            )
    newRcoObjectArr