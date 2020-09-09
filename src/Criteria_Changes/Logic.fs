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

let changeReleaseInfoToShow ( ev : Browser.Types.Event ) infos dispatch =
    let newRelease = ev.target?value : string

    infos
    |> Array.tryFind (fun info ->
        info.Released = newRelease)
    |> function
        | res when res.IsSome ->
            (infos, res.Value) |>
            (
                Yes_Rel_Plan_Log_Analysis >>
                Change_Curr_Release >>
                dispatch
            )
        | _ -> ()

let changeFileHandle dispatch ( ev : Browser.Types.Event ) =
    let files = (ev.target?files : Browser.Types.FileList)

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

    let simpleOkUtils strMsg = {
        Str_Msg = strMsg
        Buttons_With_Msg = simpleOkButton
        Dispatch = dispatch
        positions = ev
        global_msg = Types.Global_Msg_Criteria_Changes
    }

    let errorPopupNoDelay msg =
        simpleOkUtils msg
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

