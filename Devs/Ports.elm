port module Devs.Ports exposing (..)

import Devs.Objects as Objects exposing (..)

port pushDataToStore: (List MyTask, Bool, Bool) -> Cmd msg

port setDataFromStore: ((List MyTask, Bool, Int) -> msg) -> Sub msg
