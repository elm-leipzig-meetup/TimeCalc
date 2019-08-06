module Devs.Templates exposing (getTaskNameForm,getActionButton,getTask)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Ev exposing (onClick, onInput, on, keyCode, onFocus)
import Json.Decode as Json
import List exposing (..)

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Utils as DU exposing (getFormatedTime, getHoursOfTime)

getBookingRow: Bool -> String -> Booking -> Html Msg
getBookingRow useRounded tUuid booking =
  let
    from = case booking.from of
        Just time -> DU.getFormatedTime time ForInput
        Nothing -> DU.getFormatedTime O.getEmptyTime ForInput
    to = if useRounded
      then
        case booking.rounded of
          Just time -> DU.getFormatedTime time ForInput
          Nothing -> DU.getFormatedTime O.getEmptyTime ForInput
      else
        case booking.to of
          Just time -> DU.getFormatedTime time ForInput
          Nothing -> DU.getFormatedTime O.getEmptyTime ForInput
  in
    Html.tr [][
      Html.td [][
        Html.input [
          Ev.onInput (TO.SetFrom tUuid booking.uuid)
          , Attr.type_ "time"
          , Attr.value from
          , Attr.id ("from_" ++ booking.uuid)
        ][]
      ]
      , Html.td [][
        Html.input [
          Ev.onInput (TO.SetTo tUuid booking.uuid)
          , Ev.onFocus (TO.PreSetTo tUuid booking.uuid)
          , Attr.type_ "time"
          , Attr.value to
          , Attr.id "to"
        ][]
      ]
      , Html.td [][ getActionButton "-" [Attr.style "width" "20px", Attr.style "height" "20px"] (TO.RemoveBooking tUuid booking.uuid) ]
    ]

getActionButton: String -> List (Html.Attribute Msg) -> Msg -> Html Msg
getActionButton sign styles event =
  Html.input (List.append [ Ev.onClick event, Attr.type_ "button", Attr.value sign, Attr.style "cursor" "pointer" ] styles) []

getTaskNameForm: Model -> Html Msg
getTaskNameForm model =
  let
    button = case model.tempTaskName of
      Just n -> if not (String.isEmpty n)
        then getActionButton "ok" [] (TO.AddTask)
        else getActionButton "schließen" [] (TO.ToggleTasknameForm)
      Nothing -> getActionButton "schließen" [] (TO.ToggleTasknameForm)
    returnEvent = case model.tempTaskName of
      Just n -> if not (String.isEmpty n)
        then TO.AddTaskWithEnter
        else TO.NoOpInt
      Nothing -> TO.NoOpInt
  in
    Html.div[
      Attr.style "border" "black solid 0.5pt"
      , Attr.style "padding" "5px"
    ][
      Html.div [][
        Html.input [
          Attr.type_ "text"
          , on "keydown" (Json.map returnEvent keyCode)
          , Ev.onInput TO.SetTempTaskname
          , Attr.placeholder "Taskname"
          , Attr.maxlength 15
        ][]
      ]
      , Html.div [][ button ]
    ]

getTask: MyTask -> Html Msg
getTask t =
  let
    calcedTime = case t.calcedTime of
        Just time -> DU.getFormatedTime time ForDisplay
        Nothing -> DU.getFormatedTime O.getEmptyTime ForDisplay
    calcedH = case t.calcedTime of
      Just time -> DU.getHoursOfTime time
      Nothing -> 0
    calcedHours = (String.fromFloat calcedH) ++ " h"
    roundBtn = if calcedH > 0
      then case t.rounded of
        True -> getActionButton "x" [Attr.style "width" "20px", Attr.style "height" "20px"] (TO.DeRound t.uuid)
        False -> getActionButton "r" [Attr.style "width" "20px", Attr.style "height" "20px"] (TO.RoundUp t.uuid)
      else Html.text ""
    header = if calcedH > 0 then (calcedTime ++ " = " ++ calcedHours) else ""
  in
    Html.fieldset [
      Attr.style "padding-right" "0px"
      , Attr.style "padding-top" "0px"
    ][
      Html.legend [][
        Html.span [ Attr.style "padding-right" "5px" ][ Html.text t.taskName ]
        , getActionButton "-" [Attr.style "width" "20px", Attr.style "height" "20px"] (TO.RemoveTask t.uuid)
        , Html.span [ Attr.style "padding-right" "5px" ][ Html.text "" ]
        , roundBtn
      ]
      , Html.table [ Attr.style "width" "100%" ][
        Html.thead [][
          Html.tr [][
            Html.td [ Attr.colspan 2, Attr.style "text-align" "center", Attr.style "width" "100%" ][ Html.text header ]
            , Html.td [][ getActionButton "+" [Attr.style "width" "20px", Attr.style "height" "20px"] (TO.SetTimeAndAddBooking t.uuid) ]
          ]
        ]
        , Html.tbody [] (List.map (getBookingRow t.rounded t.uuid) t.timeList)
      ]
    ]
