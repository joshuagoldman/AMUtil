module Rco_Update.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Types
open Global.Types
open Feliz

let alternaitvesPopup msg buttonSettings dispatch positions =

    let buttons = Popup.View.createButtons dispatch buttonSettings

    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let popupStyle =
        (buttons,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

    (popupStyle,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg >>
        delayedMessage 3000
    )

let QuestionPopup msg yesMsg noMsg dispatch positions =

    let questionToAsk =
        msg
        |> Popup.View.getPopupMsg

    let yesNoButtons =
        Popup.View.yesNoButtons yesMsg noMsg dispatch
        |> List.toArray

    let popupStyle =
        (yesNoButtons,questionToAsk)
        |> Popup.Types.Alternative_Popup_Otpions.Several_Alternatives

    (popupStyle,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg >>
        delayedMessage 3000
    )

let errorPopupMsg msg msgToDispatch dispatch positions =
    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let simpleOkButton =
        Popup.View.simpleOkButton msgToDispatch dispatch

    let allComponentsSimleOk =
        (simpleOkButton,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok

    (allComponentsSimleOk,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg >>
        delayedMessage 3000
    )

let errorPopupMsgNotAsync msg msgToDispatch dispatch positions =
    let reactElementMsgs =
        msg
        |> Popup.View.getPopupMsg

    let simpleOkButton =
        Popup.View.simpleOkButton msgToDispatch dispatch

    let allComponentsSimleOk =
        (simpleOkButton,reactElementMsgs)
        |> Popup.Types.Alternative_Popup_Otpions.Simple_Ok

    (allComponentsSimleOk,positions) |>
    (
        Popup.Types.PopupStyle.Has_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg
    )

let checkingProcessPopupMsg positions msg =

    (msg,positions) |>
    (
        Popup.Types.PopupStyle.Has_No_Alternatives >>
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg
    )

let killPopupMsg =
    Popup.Types.PopupStyle.Popup_Is_Dead |>
    (
        Global.Types.Popup_Msg_Global >>
        Types.Global_Msg
    )

let requestFormDataStyle ( fData : Browser.FormData ) = 
    Async.FromContinuations <| fun (resolve,_,_) ->
        let xhr = Browser.XMLHttpRequest.Create()
        xhr.``open``(method = "POST", url = "http://localhost:3001/shellcommand")
        xhr.setRequestHeader("Content-Type","application/x-www-form-urlencoded")
        

        xhr.onreadystatechange <- fun _ ->
            if xhr.readyState = (4 |> float)
            then
                resolve(xhr)

        xhr.send(fData)

let getFileListData model dispatch popupPosition = async {

    let popupInfoStr =
        "retrieving RCO info from file" |>
        (
            Popup.View.getPopupMsgSpinner >>
            checkingProcessPopupMsg popupPosition >>
            dispatch
        )
    popupInfoStr


    match model.CurrFile with
    | Yes_Rco_File file ->

        let fData = Browser.FormData.Create()
            
        fData.append("file",file)
            
        let! res = requestFormDataStyle fData
            
        match res.status with
        | 200.0 ->
            let parsedInfo = parseRcoData res.responseText

            match parsedInfo with
            | Ok info ->
                let question4Msg =
                    "The RCO file list was successfully parsed. Would you like to check for any abnormalities within the file?"

                let investigateIssuesMsg =
                    (popupPosition,info,dispatch) |>
                    (
                        Investigate_Issues_Rco_Files_Msg
                    )

                let saveNewRcoDataMsg =
                    info |>
                    (
                        Save_New_Rco_Info
                    )

                let turnIntoMsgWithKillPopup msgChosen =
                    [|
                        killPopupMsg
                        msgChosen
                    |]
                    |> Types.Msg.Batch
                    

                let! questionPopupMsg =
                    QuestionPopup
                        question4Msg
                        (investigateIssuesMsg |> turnIntoMsgWithKillPopup)
                        (saveNewRcoDataMsg |> turnIntoMsgWithKillPopup)
                        dispatch
                        popupPosition

                let newRcoInfoMsg =
                    info |>
                    (
                        NeedsCorrection.No_Correction_Needed >>
                        Yes_Rco_Info >>
                        Change_Current_Rco_Info
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
                    |> errorPopupMsg err killPopupMsg dispatch 

                errorMsg |> dispatch
        | _ ->

            let! errorMsg =
                popupPosition
                |> errorPopupMsg res.responseText killPopupMsg dispatch 

            errorMsg |> dispatch
        
    | _ -> 
        let errorMsgStr =
            "No Rco list file to update!"

        let! errorMsg =
            popupPosition
            |> errorPopupMsg errorMsgStr killPopupMsg dispatch 

        errorMsg |> dispatch
}

let findFaultsInRcoFile rcoObjectArr ( popupPosition : Popup.Types.PopupPosition ) dispatch = async {

    let chcekingFaultsInformationMsg =
        "Checking faults in RCO file..." |>
        (
            Popup.View.getPopupMsgSpinner >>
            checkingProcessPopupMsg popupPosition >>
            dispatch
        )

    chcekingFaultsInformationMsg

    let forbiddenRstateRegex = "^(?!(All\s[0-9])|((-|\+))?R([0-9]|10)([A-Z]|\s)(?:\/[A-Z])?).*"
    
    let faultyLines =
        rcoObjectArr
        |> Array.indexed
        |> Array.choose (fun (pos,line) ->
            let rstateFaultyOpt = JsInterop.Regex.IsMatch forbiddenRstateRegex line.RStateIn
            match rstateFaultyOpt with
            | Some rstateFaulty ->
                if rstateFaulty
                then
                    {
                        Line = pos
                        LineInfo = line
                        Correction = No_Correction
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
            rcoObjectArr 
            |> Save_New_Rco_Info
            |> fun x -> [|x|]
    
        return(saveNewRcoFileMsg)
        
    | _ ->
        let errorMsg =
            "Some faulty RCO lines were found. Would you like to fix the faults?"
    
        let correctionMsg =
            [|
                killPopupMsg

                (rcoObjectArr,faultyLines) |>
                (
                    NeedsCorrection.Correction_Needed >>
                    Curr_Rco_Info.Yes_Rco_Info >>
                    Change_Current_Rco_Info 
                )
            |]
            |> Types.Batch

        let saveNewRcoInfoMsg =
            [|
                killPopupMsg

                rcoObjectArr 
                |> Save_New_Rco_Info
            |]
            |> Types.Batch
            

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
            alternaitvesPopup
                errorMsg
                buttonInfoArr
                dispatch
                popupPosition
    
        return([|faultsFoundMsg|])
}

type MessageType = {
    Progress : float
    Remaining : int
}

type FinishedType = {
    Status : int
    Msg : string
}

let createRcoFileInfo rcoObjArr =
    let header = {
        ReleaseDate = "ReleaseDate"
        RcoDocument = "RCOdoc"
        RcoRevision = "RCOrev"
        BarcodeText = "MatchthestringinRCO-doc(Barcodetext)"
        Slogan = "Slogan"
        ProductNumber = "Productnumber"
        ProductGroup = "ProductGroup"
        RStateIn = "R-stateIN"
        RStateOut = "R-stateOUT"
        RcLatEvaluate = "RCLAT-Evaluate"
        RcLatTextOut = {
            Formula =  "\"Perform RCO \"&D2"
            Result = "RCLAT-Textout"
            Ref = "O2:O10"
            ShareType = "shared"
        }
        ScPrttEvaluate = "SCPRTT-Evaluate"
        ScPrttTextOut = "SCPRTT�Textout"
        CloudLatEvaluate = "CloudLAT-Evaluate"
        CloudLatTextOut = {
            Formula =  "\"Perform RCO \"&D2"
            Result = "CloudLAT-Textout"
            Ref = "O2:O10"
            ShareType = "shared"
        }
        ExecutionOrder = "Executionorder"
        MfgDateFrom = "Manucfacturingdate(From)"
        MfgDateTo = "Manucfacturingdate(To)"
        ProductFamily = "Prod.Family"
        Closed = "Closed"
        Cost = "Cost"
        Comments = "Comments"
    }
    rcoObjArr
    |> Array.append [|header|]
    |> Array.map ( fun row ->
        [|
            row.ReleaseDate
            row.RcoDocument
            row.RcoRevision
            row.BarcodeText 
            row.Slogan 
            row.ProductNumber 
            row.ProductGroup 
            row.RStateIn 
            row.RStateOut
            row.RcLatEvaluate 
            row.RcLatTextOut.Result
            row.ScPrttEvaluate
            row.ScPrttTextOut
            row.CloudLatEvaluate
            row.CloudLatTextOut.Result
            row.ExecutionOrder
            row.MfgDateFrom
            row.MfgDateTo
            row.ProductFamily
            row.Closed
            row.Cost
            row.Comments
        |]
        |> String.concat "&@?"
    )
    |> String.concat "
"

let updateFile dispatch popupPosition rcoObjArr = async {

    let fileContent = createRcoFileInfo rcoObjArr

    let requestData = "content=" + fileContent

    let request = async{
        let xhr = Browser.XMLHttpRequest.Create()
        xhr.``open``(method = "POST", url = "http://localhost:3001/save")
        
        xhr.send(requestData) |> fun  _ -> ()
    }

    let mutable time = 0
    let mutable exitLoop = false

    let socketResponse = ProgressSocket.connect("http://localhost:3001")
    
    match socketResponse.ErrorMessage with
    | None  ->
        socketResponse.Socket.Value
        |> ProgressSocket.addEventListener_message(fun scktMsg ->
            let eventResult = (scktMsg :?> MessageType)

            let popupInfoStr =
                "loading file (" + (eventResult.Progress |> string) + " loaded)" |>
                (
                    Popup.View.getPopupMsgSpinner >>
                    checkingProcessPopupMsg popupPosition >>
                    dispatch
                )

            popupInfoStr
                            
        ) "message"
        |> ProgressSocket.addEventListener_message(fun scktMsg ->
            let response = (scktMsg :?> FinishedType)
            
            socketResponse.Socket.Value 
            |> ProgressSocket.disconnect
            |> ignore

            match response.Status with
            | 200 ->
                let errorMsg =
                    popupPosition
                    |> errorPopupMsgNotAsync response.Msg killPopupMsg dispatch

                errorMsg |> dispatch
            | _ ->
                let popupInfoStr =
                    "Error code 404: " + response.Msg 

                let errorMsg =
                    popupPosition
                    |> errorPopupMsgNotAsync popupInfoStr killPopupMsg dispatch

                errorMsg |> dispatch

            exitLoop <- true
            ) "finished"
        |> ignore

    | Some error ->
        exitLoop <- true

        let! errorMsg =
            popupPosition
            |> errorPopupMsg error killPopupMsg dispatch

        errorMsg |> dispatch

    do! request

    while time < 15000 && exitLoop = false  do
        do! Async.Sleep 10
        time <- time + 10
        if time > 15000
        then
            let error =
                "Connection timed out."
            let! errorMsg =
                popupPosition
                |> errorPopupMsg error killPopupMsg dispatch

            errorMsg |> dispatch

}


   

