module Criteria_1_84.Types

open Global.Types
open Browser
open Thoth.Json

type Msg =
    | Batch_Criteria_1_84 of Msg []

type Model = {
    Something : string
}

