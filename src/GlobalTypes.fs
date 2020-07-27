module Global.Types

open Fable.React
open Fable.Core.JsInterop
open Fable.Import

let getPositions ev =
    {
        Popup.Types.PosX = ( ev?pageX : float )
        Popup.Types.PosY = ( ev?pageY : float )
    }

let delayedMessage time ( msg : 'msg ) =
    async{
        if time < 30000 && time > 0
        then do! Async.Sleep time
        else do! Async.Sleep 0

        return(msg)
    }

let request ( data : obj ) = 
    Async.FromContinuations <| fun (resolve,_,_) ->
        let xhr = Browser.XMLHttpRequest.Create()
        xhr.``open``(method = "POST", url = "http://localhost:3001/shellcommand")
        xhr.setRequestHeader("Content-Type","application/x-www-form-urlencoded")
        

        xhr.onreadystatechange <- fun _ ->
            if xhr.readyState = (4 |> float)
            then
                resolve(xhr)

        xhr.send(data)

type App_Activity =
    | Activity_None
    | RCOUpdate

type PageOption =
    | RcoUpdate
    | Home

type Page =
    | VerificationPage
    | HomePage of PageOption

let tohashPageoptions page =
    match page with
    | RcoUpdate -> "#RcoUpdate"
    | Home -> "#Home"

let toHash page =
    match page with
    | VerificationPage -> "#verify"
    | HomePage option -> option |> tohashPageoptions

type CheckProcess<'a,'b> =
    | CheckProcessStarted of 'a
    | CheckProcessEnded of 'b

type Process<'a> =
    | ProcessInitiated
    | CheckProcessEnded of 'a

type ProcessNoResult<'a> =
    | ProcessInitiated of 'a
    | ProcessEnded 

type VerifyFailedMsgOptions =
    | Verify_Str_Msg_Not_Determined
    | Verify_Str_Msg of string

type Git_Repository = {
    UserName : string
    Email : string
    Branches : string[]
    CurrBranch : string
}

type Git_Repo_Parsing_Result =
    | Parsing_Succeded 
    | Parsing_Failed of string

type Git_Repo_Parsing_Options =
    | Parsing_Has_Been_Performed of Git_Repo_Parsing_Result
    | Parsing_Has_Not_Been_Performed

type Git_Repo_Cloned_Result =
    | Repository_Not_Cloned 
    | Repository_Cloned of Git_Repo_Parsing_Options
    | Git_Error

type Git_Repo_Cloned_Options =
    | Repo_Cloned_Has_Been_Checked of Git_Repo_Cloned_Result
    | Repo_Cloned_Has_Not_Been_Checked

type Origin_Access_Result =
    | Origin_accessbile of Git_Repo_Cloned_Options
    | Origin_Not_Accessible

type Origin_Accessibility_Options =
    | Origin_Accessibility_Has_Been_Checked of Origin_Access_Result
    | Origin_Accessibility_Has_Not_Been_Checked

type Git_Installed_Result =
    | Git_Installed of Origin_Accessibility_Options
    | Git_Not_Installed
    | Git_Installed_Result_Error of string

type GitDecision =
    | Git_Installed_Check_Performed of Git_Installed_Result
    | Git_Installed_Check_Not_Performed

type GlobalMsg =
    | MsgNone
    | SetNewPage of Page
    | VerifyFailesMsg of VerifyFailedMsgOptions
    | Change_Git_Installed_Status_Msg of GitDecision
    | Change_Git_Origin_Access_Msg of Origin_Accessibility_Options
    | Change_Repo_Cloned_Msg of Git_Repo_Cloned_Result
    | Change_Repo_Parsing_Result of Git_Repo_Parsing_Result
    | Popup_Msg_Global of Popup.Types.PopupStyle
    | Batch of array<GlobalMsg>
    | Go_To_Failed_Page of Button : ReactElement * Message : ReactElement []


