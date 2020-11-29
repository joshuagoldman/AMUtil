module Upgrade_NuGet.Logic.CheckDotnet

open Global.Types


let checkIfDotNetInstalled dispatch = async {

    let popupMsg =
        "Checking if Dotnet is installed"
        |> Popup.View.getPopupMsgSpinner
        |> Upgrade_NuGet.Logic.Miscellaneous.checkingProcessPopupMsg Upgrade_NuGet.Logic.Miscellaneous.standardPositions
        |> dispatch

    popupMsg

    do! Async.Sleep 2000

    let prms = [|
        "dotnet --list-sdks" |> SharedTypes.IsResponse
    |]

    let! responses = Global.Types.apis.Command prms

    let responsesAll =
        responses
        |> Array.map (fun resp ->
                resp.Answer
            )
        |> String.concat "\n"


    let dotnetRegex = JsInterop.Regex.IsMatch "dotnet" responsesAll

    match dotnetRegex with
    | Some regResult ->
        match regResult with
        | true ->
            let getNugetInfoMsg =
                dispatch |>
                (
                    Upgrade_NuGet.Types.Check_Nuget_Server >>
                    dispatch
                )

            getNugetInfoMsg
        | _ ->
            let exitMsg =
                "Dotnet is not installed on the server side. Please install it and retry."
                |> Popup.View.getPopupMsg

            let button =
                Popup.View.simpleOkButton
                                Upgrade_NuGet.Logic.Miscellaneous.killPopupMsg
                                dispatch

            let kickedOutMsg =
                (button,exitMsg) |>
                (
                    GlobalMsg.Go_To_Failed_Page >>
                    Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                    dispatch
                )

            kickedOutMsg

    | _ ->
        let exitMsg =
            "Dotnet is not installed on the server side. Please install it and retry."
            |> Popup.View.getPopupMsg

        let button =
            Popup.View.simpleOkButton
                            Upgrade_NuGet.Logic.Miscellaneous.killPopupMsg
                            dispatch

        let kickedOutMsg =
            (button,exitMsg) |>
            (
                GlobalMsg.Go_To_Failed_Page >>
                Upgrade_NuGet.Types.GlobalMsg_Upgrade_Nuget >>
                dispatch
            )

        kickedOutMsg
}