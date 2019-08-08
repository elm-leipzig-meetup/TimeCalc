module Devs.TypeObject exposing (..)

import Time exposing (Posix, Zone)

import Devs.Objects as Objects exposing (..)

-- Types

type Msg =
  NoOp
  | NoOpStr String
  | NoOpInt Int
  | ReadDataFromPublish (List MyTask, Bool, Int)
  | ToggleTasknameForm
  | SetTempTaskname String
  | EditTaskname String
  | SetTask String
  | SetTaskWithEnter String Int
  | AddTask
  | AddTaskWithEnter Int
  | RemoveTask String
  | SetTimeAndAddBooking String
  | SetTimezoneAndAddBooking String Posix
  | AddBooking String Posix Zone
  | RemoveBooking String String
  | SetFrom String String String
  | SetTo String String String
  | PreSetTo String String
  | PreSetTo_Int1 String String Posix
  | PreSetTo_Int2 String String Posix Zone
  | RoundUp String
  | DeRound String
