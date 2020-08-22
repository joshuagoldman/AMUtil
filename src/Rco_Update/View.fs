module Rco_Update.View

open Elmish
open Elmish.Navigation
open Elmish.UrlParser
open Fable.Core.JsInterop
open Types
open Feliz
open Feliz.style

let errorPopupMsgNoDelay msg msgToDispatch dispatch positions =
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

let changeFileHandle dispatch ( ev : Browser.Types.Event ) =
    let files = (ev.target?files : Browser.Types.FileList)

    let errorPopupMsg msg =
        let positions =
            {
                Popup.Types.PosX = ( ev?pageX : float )
                Popup.Types.PosY = ( ev?pageY : float )
            }

        let killPopupMsgWDispatch =
            Rco_Update.Logic.killPopupMsg |> dispatch


        errorPopupMsgNoDelay
                    msg
                    Rco_Update.Logic.killPopupMsg
                    dispatch
                    positions
        
    ()
    |> function
        | _ when files.length <> 0 ->
            let file = files.item 0
            let fileEnding =
                JsInterop.Regex.Match "(?<=\.).*" file.name
                |> fun x -> x.Value

            match fileEnding with
            | "xlsx" ->
                files.item 0 |>
                (
                    Curr_Rco_File.Yes_Rco_File >>
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
                    Curr_Rco_File.No_Rco_File
                    |> Types.Msg.Change_File_Msg

                    errorMsg |> errorPopupMsg
                |]
                
        | _ ->
            Curr_Rco_File.No_Rco_File |>
            (
                Types.Msg.Change_File_Msg >>
                fun x -> [|x|]
            )
       
let fileUpload model dispatch =
    Html.form[
        prop.children[
            Html.div[
                prop.className "form-group"
                prop.style[
                    style.margin 40
                ]
                prop.children[
                    Html.label[
                        prop.for' "exampleFormControlFile1"
                        prop.style[
                            Feliz.style.color "black"
                            style.fontWeight.bold
                            Feliz.style.fontSize 18
                        ]
                        prop.text "Choose RCO List file"

                    ]
                    Html.input[
                        prop.type'.file
                        prop.style[
                            Feliz.style.color "black"
                            style.fontWeight.bold
                            Feliz.style.fontSize 16
                        ]
                        prop.onChange ( fun ev ->
                            ev |>
                            (
                                changeFileHandle dispatch >>
                                Array.iter (fun msg -> msg |> dispatch)
                            ))
                        prop.className "form-control-file"
                    ]
                ]
            ]
        ]
    ]

let branchAlt ( name : string ) =
    Html.option[
        prop.text name
        prop.style[
            Feliz.style.backgroundColor "violet"
            Feliz.style.color "black"
            fontWeight.bold
        ]
    ]

let getBranches model dispatch =
    match model.Info with
    | No_Git_Info -> [|Html.none|]
    | Yes_Git_info repo ->
        repo.Branches
        |> Array.map (fun branch ->
            branch |> branchAlt)

let standardBranchAlt =
    [|
        Html.option[
            prop.selected true
            prop.text "Choose Branch..."
            prop.style[
                Feliz.style.backgroundColor "violet"
                Feliz.style.color "black"
                fontWeight.bold
            ]
        ]
    |]

let handleBranchNameChange ( ev : Browser.Types.Event ) dispatch =
    let branchName = ev.target?value |> string

    match branchName with
    | "Choose Branch..." ->
        Curr_Rco_File.No_Rco_File
        |> Types.Msg.Change_File_Msg
        |> fun x ->
            [|
                x

                Global.Types.MsgNone
                |> Types.Global_Msg
            |]
    | _ ->
        let positions =
            {
                Popup.Types.PosX = ( ev?pageX : float )
                Popup.Types.PosY = ( ev?pageY : float )
            }
        [|(branchName,positions,dispatch) |> Types.Msg.Change_Current_Branch_Rco|]
        

let branchDropDown model dispatch =
    match model.CurrFile with
    | Yes_Rco_File _ ->
        Html.form[
            prop.children[
                Html.div[
                    prop.className "form-row align-items-center"
                    prop.children[
                        Html.div[
                            prop.className "col-auto my-1"
                            prop.children[
                                Html.label[
                                    prop.className "mr-sm-2 sr-only"
                                    prop.for' "inlineFormCustomSelect"
                                    prop.text "Preference"
                                ]
                                Html.select[
                                    prop.className "custom-select mr-sm-2"
                                    prop.style[
                                        Feliz.style.backgroundColor "violet"
                                        Feliz.style.color "black"
                                        fontWeight.bold
                                    ]
                                    prop.onChange ( fun ev ->
                                        dispatch |>
                                        (
                                            handleBranchNameChange ev >>
                                            Array.iter (fun msg -> msg |> dispatch)
                                        ))
                                    prop.id "inlineFormCustomSelect"
                                    prop.children(
                                        getBranches model dispatch
                                        |> Array.append standardBranchAlt    
                                    )
                                ]
                            ]
                        ]
                    ]
                ]

            ]
        ]
    | _ -> Html.none
    
let currentBranchInfo model =
    match model.CurrFile with
    | Yes_Rco_File _ ->
        match model.Info with
        | Yes_Git_info info ->
            Html.div[
                prop.text ("Current branch: " + info.CurrBranch)
                prop.style[
                    style.fontSize 20
                    style.fontWeight.bold
                    style.color "black"
                ]
            ]
        | _ -> Html.none
    | _ -> Html.none

let updateRcoButton model dispatch =
    match model.CurrFile with
    | Yes_Rco_File _ ->
        match model.Info with
        | Yes_Git_info info ->
            Html.div[
                prop.className "button"
                prop.text "Update RCO List"
                prop.style[
                    style.fontSize 17
                    style.fontWeight.bold
                    style.color "black"
                ]
                prop.onClick (fun ev -> Types.Msg.Get_Rco_Data_Msg(dispatch,ev) |> dispatch)
            ]
        | _ -> Html.none
    | _ -> Html.none

let faultyLines model dispatch =
    match model.CurrRcoInfo with
    | Yes_Rco_Info needsCorrection ->
        match needsCorrection with
        | NeedsCorrection.Correction_Needed(info,faults) ->
            Html.div[
                Rco_Update.RcoFaultTable.root
                    faults
                    info
                    dispatch
            ]
        | NeedsCorrection.No_Correction_Needed _ ->
            Html.none
    | _ -> Html.none


let RetryRcoUpdate model dispatch =
    match model.CurrRcoInfo with
    | Yes_Rco_Info needsCorrection ->
        match needsCorrection with
        | NeedsCorrection.Correction_Needed(info,faults) ->
            Html.div[
                prop.className "button"
                prop.text "Update modified RCO List"
                prop.style[
                    style.fontSize 17
                    style.fontWeight.bold
                    style.color "black"
                ]
                prop.onClick (fun ev -> Types.Msg.Update_Rco_Changes(info,faults,ev |> Global.Types.getPositions,dispatch) |> dispatch)
            ]
        | NeedsCorrection.No_Correction_Needed _ ->
            Html.none
    | _ -> Html.none


    
let root model dispatch =
    Html.div[
        prop.children[
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.children[
                            fileUpload model dispatch
                        ]
                    ]
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.children[
                            branchDropDown model dispatch
                        ]
                    ]
                    Html.div[
                        prop.className "column"
                        prop.children[
                            updateRcoButton model dispatch 
                        ]
                    ]
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.children[
                            currentBranchInfo model
                        ]
                    ]
                ]
            ]
            Html.div[
                prop.className "columns is-centered"
                prop.children[
                    Html.div[
                        prop.className "column"
                        prop.children[
                            faultyLines model dispatch
                        ]
                    ]
                    Html.div[
                        prop.className "column"
                        prop.children[
                            RetryRcoUpdate model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
