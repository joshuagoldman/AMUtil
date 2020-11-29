module Rco_Update.Logic.RCOObject

open JsInterop
open Elmish
open Fable.Import
open System
open Feliz
open Fable.Remoting.Client
open SharedTypes

let getFileListData ( model : Rco_Update.Types.Model ) dispatch popupPosition = async {

    let popupInfoStr =
        "retrieving RCO info from file" |>
        (
            Popup.View.getPopupMsgSpinner >>
            Rco_Update.Logic.Miscellaneous.checkingProcessPopupMsg popupPosition >>
            dispatch
        )
    popupInfoStr

    match model.CurrFile with
    | Rco_Update.Types.Yes_Rco_File(file,file_type) ->
            
        let! res = Global.Types.apis.GetRcoObject ( file :?> obj )
            
        match res with
        | Ok resSuccess ->
                let parsedInfo = Rco_Update.Logic.Miscellaneous.getWriteRCOInfo resSuccess file_type

                let question4Msg =
                    "The RCO file list was successfully parsed. Would you like to check for any abnormalities within the file?"

                let investigateIssuesMsg =
                    (popupPosition,parsedInfo,dispatch) |>
                    (
                        Rco_Update.Types.Investigate_Issues_Rco_Files_Msg
                    )

                let saveNewRcoDataMsg =
                    (parsedInfo,popupPosition,dispatch) |>
                    (
                        Rco_Update.Types.Save_New_Rco_Info
                    )

                let turnIntoMsgWithKillPopup msgChosen =
                    [|
                        Rco_Update.Logic.Miscellaneous.killPopupMsg
                        msgChosen
                    |]
                    |> Rco_Update.Types.Msg.Batch
                

                let! questionPopupMsg =
                    Rco_Update.Logic.Miscellaneous.QuestionPopup
                                            question4Msg
                                            (investigateIssuesMsg |> turnIntoMsgWithKillPopup)
                                            (saveNewRcoDataMsg |> turnIntoMsgWithKillPopup)
                                            dispatch
                                            popupPosition

                let newRcoInfoMsg =
                    parsedInfo |>
                    (
                        Rco_Update.Types.NeedsCorrection.No_Correction_Needed >>
                        Rco_Update.Types.Yes_Rco_Info >>
                        Rco_Update.Types.Change_Current_Rco_Info
                    )

                let msgCombined =
                    [|
                        newRcoInfoMsg
                        questionPopupMsg
                    |]

                msgCombined
                |> Array.iter (fun msg -> msg |> dispatch)
                    
        | Error err ->

            let! errorMsg =
                popupPosition
                |> Rco_Update.Logic.Miscellaneous.errorPopupMsg err Rco_Update.Logic.Miscellaneous.killPopupMsg dispatch 

            errorMsg |> dispatch
    | _ -> 
        let errorMsgStr =
            "No Rco list file to update!"

        let! errorMsg =
            popupPosition
            |> Rco_Update.Logic.Miscellaneous.errorPopupMsg errorMsgStr Rco_Update.Logic.Miscellaneous.killPopupMsg dispatch 

        errorMsg |> dispatch
}