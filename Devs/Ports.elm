port module Devs.Ports exposing (..)

import Devs.Objects as Objects exposing (..)

port pushDataToClipboard: String -> Cmd msg
port pushDataToStore: TransferObj -> Cmd msg
port openWindow: (String, Bool) -> Cmd msg

port setDataFromStore: (TransferObj2 -> msg) -> Sub msg
