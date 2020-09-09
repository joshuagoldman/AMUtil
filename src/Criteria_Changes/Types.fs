module Criteria_Changes.Types

open Global.Types
open Browser
open Thoth.Json

type Msg =
    | Batch_Criteria_Changes of Msg []

type Git_Info_Criteria_Changes =
    | Yes_Git_Info_Criteria_Changes of Global.Types.Git_Repository
    | No_Git_Info_Criteria_Changes

type Model = {
    Info : Git_Info_Criteria_Changes
}

