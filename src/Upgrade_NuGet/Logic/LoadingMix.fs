module Upgrade_NuGet.Logic.LoadingMix

open JsInterop
open Elmish
open Fable.Import
open System
open Upgrade_NuGet.Types
open Global.Types
open Feliz
open Fable.Core.JsInterop
open SharedTypes
open Fable.Remoting.Client


let changeLoadingMix model result dispatch =
    let projName =
        match result with
        | Loganalyzer_Projects_Table_Result.Loading_Was_Not_Successfull(name,_) ->
            name
        | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull proj ->
            proj.Name

    match model.Projects_Table with
    | Loganalyzer_Projects_Table_Status.Info_Is_Loading mix ->
        let newMix =
            mix
            |> Array.map (fun mix_object ->
                match mix_object with
                | Loganalyzer_Projects_Table_Mix.Project_Loading info ->
                    if info.Project_Name = projName
                    then
                        result
                        |> Loganalyzer_Projects_Table_Mix.Project_Not_Loading
                    else
                        mix_object
                | _ -> mix_object)

        let standardAnswer =
            let newStatusMsg  =
                newMix |>
                (
                    Loganalyzer_Projects_Table_Status.Info_Is_Loading >>
                    Change_NuGet_Status
                )
            let msgWithSentPopup =
                (newStatusMsg,dispatch) |>
                (
                    Send_Popup_With_New_State >>
                    Cmd.ofMsg
                )

            model, Global.Types.MsgNone, msgWithSentPopup


        match (Upgrade_NuGet.Logic.Common.haveProjectsBeenLoaded newMix) with
        | true ->
            let successProjsOpt =
                newMix
                |> Array.choose (fun mix_object ->
                    match mix_object with
                    | Loganalyzer_Projects_Table_Mix.Project_Not_Loading res ->
                        match res with
                        | Loganalyzer_Projects_Table_Result.Loading_Was_Successfull proj ->
                            Some proj
                        | _ -> None
                    | _ -> None)
                |> function
                    | res when (res |> Array.length) <> 0 ->
                        Some res
                    | _ -> None

            match successProjsOpt with
            | Some succesProjs ->
                let finishedStatusMsg  =
                    (
                        succesProjs |>
                        (
                            Loganalyzer_Projects_Table.Yes_Projects_Table_Info >>
                            Loganalyzer_Projects_Table_Status.Info_Has_Been_Loaded >>
                            Change_NuGet_Status
                        ),
                        dispatch
                    )
                    |> Send_Popup_With_New_State
                    

                let newStatusMsg =
                    (
                        newMix |>
                        (
                            Loganalyzer_Projects_Table_Status.Info_Is_Loading >>
                            Change_NuGet_Status
                        ),
                        dispatch
                    )
                    |> Send_Popup_With_New_State

                let msgWithSentPopup =
                    [|
                        newStatusMsg
                        finishedStatusMsg
                    |] |>
                    (
                        Upgrade_NuGet.Types.Batch >>
                        Cmd.ofMsg
                    )

                model, Global.Types.MsgNone, msgWithSentPopup
            | _ -> 
                standardAnswer
            
        | _ ->
            standardAnswer
    | _ ->
        model, Global.Types.MsgNone, []
