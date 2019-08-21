port module Devs.Ports exposing (..)

import Devs.Objects as Objects exposing (..)

port pushDataToStore: TransferObj -> Cmd msg
port openWindow: (String, Bool) -> Cmd msg

port setDataFromStore: (TransferObj2 -> msg) -> Sub msg
