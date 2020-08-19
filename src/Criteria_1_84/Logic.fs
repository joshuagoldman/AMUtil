module Criteria_1_84.Logic

open JsInterop
open Elmish
open Fable.Import
open System
open Global.Types
open Feliz

let tu =
    let ArgsRegex = "(?<=Arg(.|\n)*(is\s|=))(.|\n)*?(?=((,|\n)(\s|\n)*Arg|\sand))"

    ArgsRegex

