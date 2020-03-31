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
  , tempTaskApi: Maybe Api
  , tempTaskUuid: Maybe String
  , tempTaskIsTicket: Bool
  , showTaskNameForm: Bool
  , timeZone: Zone
  , taskUuidForDel: Maybe String
  , showConfigApiForm: Bool
  , apiList: List Api
  , apiTypeList: List ApiType
  , apiForAdd: Maybe Api
  , ticketUrl: Maybe String
  , t1: Maybe MyTime
  , t2: Maybe MyTime
  , t3: Maybe Float
  }

type alias TransferObj = {
    tasks: List MyTask
    , showTaskNameForm: Bool
    , initialise: Bool
    , apiList: List Api
  }

type alias TransferObj2 = {
    tasks: List MyTask
    , showTaskNameForm: Bool
    , random: Int
    , apiList: List Api
  }

type alias MyTask = {
  calcedTime: Maybe MyTime
  , timeList: List Booking
  , taskName: String
  , uuid: String
  , rounded: Bool
  , saved: Bool
  , api: Maybe Api
  , isTicket: Bool
  }

type alias Booking = {
  from: Maybe MyTime
  , to: Maybe MyTime
  , rounded: Maybe MyTime
  , uuid: String
  , nr: Maybe String
  }

type alias MyTime = {
  hour: Int
  , minute: Int
  }

type alias Api = {
  apiType: ApiType
  , apiUrl: String
  , user: String
  , password: String
  , uuid: String
  }

type alias ApiType = {
  id: Int
  , name: String
  , uuid: String
  }

--Model
initialModel: Model
initialModel = {
  random = 123456789
  , currentSeed = Nothing
  , taskList = []
  , tempTaskName = Nothing
  , tempTaskApi = Nothing
  , tempTaskUuid = Nothing
  , tempTaskIsTicket = False
  , showTaskNameForm = True
  , timeZone = Time.utc
  , taskUuidForDel = Nothing
  , showConfigApiForm = False
  , apiList = []
  , apiTypeList = getApiTypList
  , apiForAdd = Nothing
  , ticketUrl = Just "https://hiszilla.his.de/hiszilla/show_bug.cgi?id="
  , t1 = Nothing
  , t2 = Nothing
  , t3 = Nothing
  }

getEmptyApi: Api
getEmptyApi = {
  apiType=getEmptyApiType
  , apiUrl=""
  , user=""
  , password=""
  , uuid=UUID.toString UUID.nil
  }

getTransferObj: List MyTask -> List Api -> Bool -> Bool -> TransferObj
getTransferObj taskList apiList showTaskNameForm initialise = {
    tasks=taskList
    , showTaskNameForm=showTaskNameForm
    , initialise=initialise
    , apiList=apiList
  }

getEmptyApiType: ApiType
getEmptyApiType = {
  id=-1
  , name=""
  , uuid=UUID.toString UUID.nil
  }

getApiTypList: List ApiType
getApiTypList = [
    { id=0
      , name="CU Jira"
      , uuid="1c977be1-6f66-4e50-87c8-a506ff776847"
    }
    , { id=1
      , name="LYSS Jira"
      , uuid="5bd7162a-a9e4-4c4c-96c5-3c8a25e9b615"
    }
  ]

getEmptyTask: MyTask
getEmptyTask = {
  taskName = "TestTask"
  , calcedTime = Nothing
  , timeList = []
  , uuid = UUID.toString UUID.nil
  , rounded = False
  , saved = False
  , api = Nothing
  , isTicket = False
  }

getEmptyTime: MyTime
getEmptyTime = {hour=0,minute=0}

getEmptyBooking: Booking
getEmptyBooking = {
  from = Nothing
  , to = Nothing
  , uuid = UUID.toString UUID.nil
  , rounded = Nothing
  , nr = Nothing
  }
