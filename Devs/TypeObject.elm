module Devs.TypeObject exposing (..)

import Time exposing (Posix, Zone)

import Devs.Objects as Objects exposing (..)

-- Types

type Msg =
  NoOp
  | NoOpStr String
  | NoOpInt Int
  | GoTo String Bool
  | ReadDataFromPublish TransferObj2
  | CopyToClipboard String
  | ToggleConfigApiForm
  | AddConfigApi
  | RemoveApi String
  | SetApiType String
  | SetUrl String
  | SetTicketUrl String
  | SetUser String
  | SetPwd String
  | SetTimeZone Zone
  | ToggleTasknameForm
  | SetTempTaskname String
  | SetTempApi String
  | SetTempIsTicket Bool
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
  | SetNr String String String
  | SetFrom String String String
  | SetTo String String String
  | PreSetTo String String
  | PreSetTo_Int String String Posix
  | RoundUp String
  | DeRound String
  | SyncToExtern String String
  | SetT1 String String
  | SetT2 String String
  | SetT3 String
  | ClearTimes
  | ToggleCommentForm (Maybe (String, String))
  | SetBookingComment String String String
