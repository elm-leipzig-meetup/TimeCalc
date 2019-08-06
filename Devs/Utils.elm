module Devs.Utils exposing (getSeed, toTime, calculateTime, getFormatedTime, getHoursOfTime, focusSearchBox, getTaskForEdit, roundUpTime, getMyTimeFromPosix)

import List exposing (..)
import List.Extra as ListE
import Round as R exposing (..)
import Random
import Browser.Dom as Dom
import Task
import Time exposing (Posix, Zone, utc, toHour, toMinute)
import Debug exposing (log)

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)

roundUpTime: Booking -> Booking
roundUpTime b =
  let
    from = case b.from of
      Just time -> time
      Nothing -> O.getEmptyTime
    to = case b.to of
      Just time -> time
      Nothing -> O.getEmptyTime
    diff = getMinutes (diffFromTimes to from)
    newDiff = if (modBy 15 diff) == 0
      then diff
      else (Basics.ceiling ((toFloat diff) / 15)) * 15
  in
    { b | rounded = Just (addTimes from (getTimeFromInt newDiff)) }

getTaskForEdit: Model -> String -> MyTask
getTaskForEdit model tUuid =
  case List.head (List.filter (\item -> (item.uuid == tUuid)) model.taskList) of
    Just t -> t
    Nothing -> O.getEmptyTask

focusSearchBox: String -> Cmd Msg
focusSearchBox id = Task.attempt (\_ -> NoOp) (Dom.focus id)

getHoursOfTime: MyTime -> Float
getHoursOfTime t =
  case String.toFloat (R.round 2 ((toFloat t.hour) + ((toFloat t.minute) / 60))) of
    Just f -> f
    Nothing -> 0.0

getFormatedTime: MyTime -> TimeFormat -> String
getFormatedTime t tf =
  case tf of
      ForDisplay -> (String.fromInt t.hour) ++ "h " ++ (String.padLeft 2 '0' (String.fromInt t.minute)) ++ "m"
      ForInput -> (String.padLeft 2 '0' (String.fromInt t.hour)) ++ ":" ++ (String.padLeft 2 '0' (String.fromInt t.minute))

getMyTimeFromPosix: Posix -> Zone -> MyTime
getMyTimeFromPosix t z =
  let
    time = O.getEmptyTime
  in
    { time | hour = Time.toHour z t, minute = Time.toMinute z t }

calculateTime: List Booking -> Bool -> Maybe MyTime
calculateTime timeList useRounded =
  let
    times = List.map (getTimes useRounded) timeList
  in
    ListE.foldl1 addTimes times

addTimes: MyTime -> MyTime -> MyTime
addTimes a b =
  let
    diffMin = (getMinutes a) + (getMinutes b)
    hours = Basics.floor ((toFloat diffMin) / 60)
    minutes = diffMin - (hours * 60)
  in
    { hour = hours, minute = minutes }

getTimes: Bool -> Booking -> MyTime
getTimes useRounded booking =
  let
    from = case booking.from of
        Just time -> time
        Nothing -> O.getEmptyTime
    to = case booking.to of
        Just time -> time
        Nothing -> O.getEmptyTime
    rounded = case booking.rounded of
        Just time -> time
        Nothing -> O.getEmptyTime
  in
    case useRounded of
        True -> diffFromTimes rounded from
        False -> diffFromTimes to from

diffFromTimes: MyTime -> MyTime -> MyTime
diffFromTimes a b =
  let
    diffMin = (getMinutes a) - (getMinutes b)
    hours = Basics.floor ((toFloat diffMin) / 60)
    minutes = diffMin - (hours * 60)
  in
    { hour = hours, minute = minutes }

getTimeFromInt: Int -> MyTime
getTimeFromInt diffMin =
  let
    hours = Basics.floor ((toFloat diffMin) / 60)
    minutes = diffMin - (hours * 60)
  in
    { hour = hours, minute = minutes }

getMinutes: MyTime -> Int
getMinutes t = (t.hour * 60) + t.minute

toTime: String -> MyTime
toTime str =
  let
    timeArr = String.split ":" str
    hour = case List.head timeArr of
      Just h -> case String.toInt h of
        Just h2 -> h2
        Nothing -> 0
      Nothing -> 0
    minute = case ListE.last timeArr of
      Just m -> case String.toInt m of
        Just m2 -> m2
        Nothing -> 0
      Nothing -> 0
  in
    { hour = hour, minute = minute }

getSeed: Model -> Random.Seed
getSeed model =
  case model.currentSeed of
      Just seed ->  seed
      Nothing -> Random.initialSeed model.random
