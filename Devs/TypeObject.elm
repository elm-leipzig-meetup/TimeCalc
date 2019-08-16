module Devs.TypeObject exposing (..)

import Time exposing (Posix, Zone)

import Devs.Objects as Objects exposing (..)

-- Types

type Msg =
  NoOp
  | NoOpStr String
  | NoOpInt Int
  | ReadDataFromPublish TransferObj2
  | ToggleConfigApiForm
  | AddConfigApi
  | RemoveApi String
  | SetApiType String
  | SetUrl String
  | SetUser String
  | SetPwd String
  | SetTimeZone Zone
  | ToggleTasknameForm
  | SetTempTaskname String
  | SetTempApi String
  | EditTaskname String
  | SetTask String
  | SetTaskWithEnter String Int
  | AddTask
  | AddTask_Int Posix
  | AddTaskWithEnter Int
  | CancelRemoveTask
  | RemoveTask String
  | RemoveTaskConfirmed
  | ToggleSaveTask String
  | SetTimeAndAddBooking String
  | AddBooking String Posix
  | RemoveBooking String String
  | SetFrom String String String
  | SetTo String String String
  | PreSetTo String String
  | PreSetTo_Int String String Posix
  | RoundUp String
  | DeRound String
  | SyncToExtern String String
