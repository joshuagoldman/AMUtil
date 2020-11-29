module Rco_Update.Logic.SaveRCOContent

open JsInterop
open Elmish
open Fable.Import
open System
open Global.Types
open Feliz
open Fable.Remoting.Client
open SharedTypes

let changeNameRequest ( socket : Browser.WebSocket ) writeFileModel dispatch =
    Async.FromContinuations <| fun (resolve,_,_) ->
        socket.onmessage <- fun socketMsg ->
            let msg = (socketMsg.data :?> string)

            let isMsg = JsInterop.Regex.IsMatch "@message:" msg 
            match isMsg.Value with
            | true ->

                let msgMatch = (JsInterop.Regex.Match "@message:" msg).Value
                let msg = "Changing NuGet name (" + (msgMatch |> int |> string) + " written)"

                let popupInfoStr =
                    (msgMatch.Replace("@message:","")) |>
                    (
                        float >>
                        Popup.View.getPopupMsgProgress msg >>
                        Rco_Update.Logic.Miscellaneous.checkingProcessPopupMsg Popup.Types.standardPositions >>
                        dispatch
                    )

                popupInfoStr
            | _ ->
                let response = (socketMsg.data :?> string)
                socket.close()

                resolve 
                    {
                        Status = 500
                        Msg = response.ToLower().Replace("@finished","")
                    }

        Global.Types.apis.WriteFile writeFileModel
        |> Async.StartImmediate

let createRcoFileInfo ( rcoObjArr : SharedTypes.RcoObject array ) =
    let header = {
        ReleaseDate = "Release Date"
        RcoDocument = "RCOdoc"
        RcoRevision = "RCO rev"
        BarcodeText = "Match the string in RCO-doc(Barcodetext)"
        Slogan = "Slogan"
        ProductNumber = "Productnumber"
        ProductGroup = "Product Group"
        RStateIn = "R-stateIN"
        RStateOut = "R-stateOUT"
        RcLatEvaluate = "RC LAT - Evaluate"
        RcLatTextOut = "RC LAT - Textout"
        ScPrttEvaluate = "SC PRTT - Evaluate"
        ScPrttTextOut = "SC PRTT – Textout"
        CloudLatEvaluate = "Cloud LAT - Evaluate"
        CloudLatTextOut = "Cloud LAT - Textout"
        ExecutionOrder = "Execution order"
        MfgDateFrom = "Manucfacturing date (From)"
        MfgDateTo = "Manucfacturing date (To)"
        ProductFamily = "Prod. Family"
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
            row.RcLatTextOut
            row.ScPrttEvaluate
            row.ScPrttTextOut
            row.CloudLatEvaluate
            row.CloudLatTextOut
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

let updateFile file_type dispatch popupPosition rcoObjArr = async {
    let rco_file_type =
        match file_type with
        | Rco_Update.Types.RBS_6000 -> "RBS RCO List.csv"
        | _ -> "ERS RCO List.csv"

    let startSaveMsg = "Starting save process..."

    let fileContent = createRcoFileInfo rcoObjArr

    let popupInfoStr =
        0.0 |>
        (
            Popup.View.getPopupMsgProgress startSaveMsg >>
            Rco_Update.Logic.Miscellaneous.checkingProcessPopupMsg popupPosition
        )
    popupInfoStr |> dispatch

    let writeFileModel = {
        Insert_Text = fileContent
        Dest_Path = rco_file_type
        Rate = 1
        SocketPort = 3001
    }

    let socket = Browser.WebSocket.Create("localhost:3001")
    let! res = changeNameRequest socket writeFileModel dispatch

    popupPosition |>
    (
        Rco_Update.Logic.Miscellaneous.errorPopupMsgNotAsync res.Msg Rco_Update.Logic.Miscellaneous.killPopupMsg dispatch >>
        dispatch
    )
}

