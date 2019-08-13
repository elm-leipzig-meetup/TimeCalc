module Devs.Objects exposing (..)

import UUID exposing (UUID)
import Random
import Time exposing (Zone, utc)

type TimeFormat = ForDisplay | ForInput

-- Model
type alias Model = {
  random: Int
  , currentSeed : Maybe Random.Seed
  , taskList: List MyTask
  , tempTaskName: Maybe String
  , tempTaskUuid: Maybe String
  , showTaskNameForm: Bool
  , timeZone: Zone
  }

type alias MyTask = {
  calcedTime: Maybe MyTime
  , timeList: List Booking
  , taskName: String
  , uuid: String
  , rounded: Bool
  , saved: Bool
  }

type alias Booking = {
  from: Maybe MyTime
  , to: Maybe MyTime
  , rounded: Maybe MyTime
  , uuid: String
  }

type alias MyTime = {
  hour: Int
  , minute: Int
  }

--Model
initialModel: Model
initialModel = {
  random = 123456789
  , currentSeed = Nothing
  , taskList = []
  , tempTaskName = Nothing
  , tempTaskUuid = Nothing
  , showTaskNameForm = True
  , timeZone = Time.utc
  }

getEmptyTask: MyTask
getEmptyTask = {
  taskName = "TestTask"
  , calcedTime = Nothing
  , timeList = []
  , uuid = UUID.toString UUID.nil
  , rounded = False
  , saved = False
  }

getEmptyTime: MyTime
getEmptyTime = {hour=0,minute=0}

getEmptyBooking: Booking
getEmptyBooking = {
  from = Nothing
  , to = Nothing
  , uuid = UUID.toString UUID.nil
  , rounded = Nothing
  }
