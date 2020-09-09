module Criteria_Changes.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Global.Types
open Feliz
open Fable.Core.JsInterop
open Popup.Types
open Types
open Fable.React

(*
type Popup_Button<'msg> = {
    Name : string
    Msg : 'msg
}

type Popup_Utils<'a,'b> = {
Str_Msg : string
Buttons_With_Msg : Popup_Button<'b> []
Dispatch : 'b -> unit
positions : Browser.Types.Event
global_msg : 'a -> 'b
}
*)

let simpleOkButton =
    [|
        {
            Popup.Types.Name = "Ok"
            Popup.Types.Msg =
                Popup.Types.PopupStyle.Popup_Is_Dead |>
                (
                    Global.Types.Popup_Msg_Global >>
                    Types.Global_Msg_Criteria_Changes
                )
        }
    |]

let simpleOkUtils dispatch positions strMsg = {
    Str_Msg = strMsg
    Buttons_With_Msg = simpleOkButton
    Dispatch = dispatch
    positions = positions
    global_msg = Types.Global_Msg_Criteria_Changes
}

let changeReleaseInfoToShow ( ev : Browser.Types.Event ) infos dispatch =
    let newRelease = ev.target?value : string

    infos
    |> Array.choose (fun info ->
        if info.Excel_Info.Released = newRelease
        then
            Some info
        else None)
    |> function
        | res when (res |> Array.length) <> 0 ->
            (infos,res) |>
            (
                Yes_Rel_Plan_Log_Analysis >>
                Change_Curr_Release >>
                dispatch
            )
        | _ -> ()

let changeFileHandle dispatch ( ev : Browser.Types.Event ) =
    let files = (ev.target?files : Browser.Types.FileList)

    let errorPopupNoDelay msg =
        msg
        |> simpleOkUtils dispatch standardPositions
        |> Popup.View.generalPopupCreation
        
    match files.length with
    | 0 ->
        Types.Curr_Rel_Plan_Log_Analysis_File.No_Log_Analysis_File |>
        (
            Types.Msg.Change_File_Msg >>
            fun x -> [|x|]
        )
    | _ ->
        let file = files.item 0
        let fileEnding =
            JsInterop.Regex.Match "(?<=\.).*" file.name
            |> fun x -> x.Value

        match fileEnding with
        | "xlsx" ->
            files.item 0 |>
            (
                Types.Yes_Log_Analysis_File >>
                Types.Msg.Change_File_Msg
            )
            |> fun x -> [|x|]
        | _ ->
            let errorMsg =
                System.String.Format(
                    "The file chosen {0}. Please choose an excel file.",
                    if
                        file.``type`` = ""
                    then
                        "has file ending " + fileEnding
                    else
                        "is of format " + file.``type``
                )
            [|
                Types.Curr_Rel_Plan_Log_Analysis_File.No_Log_Analysis_File
                |> Types.Msg.Change_File_Msg

                errorPopupNoDelay errorMsg
            |]

let parseCriteriaChangesFile ( file : Browser.Types.File ) dispatch = async {

    let fData = Browser.FormData.Create()
    fData.append("file",file)

    do! Async.Sleep 2000

    ("Parsing file " + file.name + " with size in bytes: " + (file.size |> string) ) |>
    (
        Popup.View.getPopupMsgSpinner >>
        Popup.View.checkingProcessPopupMsg standardPositions Types.Global_Msg_Criteria_Changes >>
        dispatch
    )

    let! isTimedOut = Global.Types.requestFormDataStyle fData ""

    match isTimedOut with
    | HttpResponse.Success res ->
        match res.status with
        | 200.0 ->
            let jsonParsingResult =
                Types.parseExcelInfo res.responseText

            match jsonParsingResult with
            | Ok info ->
                let msg =
                    [|
                        "Getting criteria infotexts from HWLogCriteria.xml file..." |>
                        (
                            Popup.View.getPopupMsgSpinner >>
                            Popup.View.checkingProcessPopupMsg standardPositions Types.Global_Msg_Criteria_Changes
                        )

                        (info,dispatch)
                        |> Extract_Info_Text_Criteria_File
                    |]
                    |> Batch_Criteria_Changes

                return(msg)
            | Error errorMsg ->
                return(
                    errorMsg |>
                    (
                        simpleOkUtils dispatch standardPositions >>
                        Popup.View.generalPopupCreation
                    )
                )
        | _ ->
            return(
                res.responseText |>
                (
                    simpleOkUtils dispatch standardPositions >>
                    Popup.View.generalPopupCreation
                )
            )
    | TimedOut str ->
        return(
            str |>
            (
                simpleOkUtils dispatch standardPositions >>
                Popup.View.generalPopupCreation
            )
        )
}

let getCriteriaInfoText dispatch ( currInfos : Log_Search_Criteria_Excel_Info [] ) = async {

    do! Async.Sleep 2000

    let! res = Global.Types.simpleGetRequest ""

    match res.status with
    | 200.0 ->
        let hwLogCriteriaFileStr = res.responseText

        let searchKeyChunkRegexExpr = "<SearchKey(.|\n)*?(?=<\/SearchKey>)"

        let searchKeys =
            (Regex.Matches searchKeyChunkRegexExpr hwLogCriteriaFileStr).Value

        let ifNullThenUndef strOpt =
            match strOpt with
            | Some str -> str
            | _ -> "Undef"

        let docNoRegexExpr = "(?<=CriteriaReferenceWithRevision Value=\").*(?=(,\s+[rR]ev|;))"
        let infoTextRegex = "(?<=InfotextScreening Value=\").*(?=\")"
        let searchKeyNameRegexExpr = "(?<=<SearchKey Name=\").*(?=\")"
        let revisionInHwLogCriteriaRegexExpr = "(?<=CriteriaReferenceWithRevision Value=\".*(,\s+[rR]ev|;)(\s+|))[aA-zZ]"

        let searchKeysWithSameDocNo =
            currInfos
            |> Array.choose ( fun info ->
                let foundKeyOpt =
                    searchKeys
                    |> Array.tryFind (fun key ->
                        let revisionNoOpt =
                            Regex.Match docNoRegexExpr key

                        match revisionNoOpt with
                        | Some revisionNo ->
                            revisionNo.Replace(" ","") = info.Revision.Replace(" ","")
                        | _ -> false
                        )
                match foundKeyOpt with
                | Some foundKey ->
                    let criteriaFileInfo =
                        {
                            Search_Key_Name =
                                Regex.Match searchKeyNameRegexExpr foundKey
                                |> ifNullThenUndef
                            Info_Text =
                                Regex.Match infoTextRegex foundKey
                                |> ifNullThenUndef
                            Revision =
                                Regex.Match revisionInHwLogCriteriaRegexExpr foundKey
                                |> ifNullThenUndef
                        }

                    let result =
                        {
                            Excel_Info = info
                            HwLog_Crit_File_Info = criteriaFileInfo
                        }

                    result
                    |> Some

                | _ -> None
            )

        match ( searchKeysWithSameDocNo |> Array.length ) with
        | 0 ->
            "No search key matches found in HwLogCriteria.xml file!" |>
            (
                simpleOkUtils dispatch standardPositions >>
                Popup.View.generalPopupCreation >>
                dispatch
            )
        | _ ->
            let firstItem =
                searchKeysWithSameDocNo
                |> Array.head
            let currRevisionInfo =
                searchKeysWithSameDocNo
                |> Array.filter (fun info ->
                    info.Excel_Info.Released = firstItem.Excel_Info.Released)

            let msg =
                (searchKeysWithSameDocNo,currRevisionInfo) |>
                (
                    Yes_Rel_Plan_Log_Analysis >>
                    Types.Change_Table_Info
                )

            msg |> dispatch
                    
    | _ ->
        res.responseText |>
        (
            simpleOkUtils dispatch standardPositions >>
            Popup.View.generalPopupCreation >>
            dispatch
        )
}

