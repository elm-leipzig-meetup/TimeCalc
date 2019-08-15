module Devs.Templates exposing (getTaskNameForm,getActionButton,getTask, getConfigForm)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Ev exposing (onClick, onInput, on, keyCode, onFocus)
import Json.Decode as Json
import List exposing (..)

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Utils as DU exposing (getFormatedTime, getHoursOfTime)

getBookingRow: MyTask -> Booking -> Html Msg
getBookingRow t booking =
  let
    useRounded = t.rounded
    tUuid = t.uuid
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
    fromField = if t.saved
      then Html.text (from ++ " -")
      else Html.input [
          Ev.onInput (TO.SetFrom tUuid booking.uuid)
          , Attr.type_ "time"
          , Attr.value from
          , Attr.id ("from_" ++ booking.uuid)
        ][]
    toField = if t.saved
      then Html.text to
      else Html.input [
          Ev.onInput (TO.SetTo tUuid booking.uuid)
          , Ev.onFocus (TO.PreSetTo tUuid booking.uuid)
          , Attr.type_ "time"
          , Attr.value to
          , Attr.id ("to_" ++ booking.uuid)
        ][]
  in
    Html.tr [][
      Html.td [ Attr.style "text-align" "right" ][ fromField ]
      , Html.td [][ toField ]
      , Html.td [][ showActionButtonInTask t (getActionButton "minus_rect" "löschen" [Attr.style "width" "20px"] (TO.RemoveBooking tUuid booking.uuid)) ]
    ]

getActionButton: String -> String -> List (Html.Attribute Msg) -> Msg -> Html Msg
getActionButton sign title styles event =
--  Html.input (List.append [ Ev.onClick event, Attr.type_ "button", Attr.value sign, Attr.style "cursor" "pointer" ] styles) []
  Html.button (List.append [
      Ev.onClick event
      , Attr.style "cursor" "pointer"
      , Attr.style "width" "20px"
      , Attr.style "padding" "1px"
      , Attr.style "height" "20px"
      , Attr.title title
    ] styles) [
    Html.img [
      Attr.src ("img/" ++ sign ++ ".svg")
      , Attr.style "height" "15px"
--      , Attr.style "margin-left" "-7px"
--      , Attr.style "margin-top" "-1px"
    ][]
  ]

getConfigForm: Model -> Html Msg
getConfigForm model =
    Html.div[
      Attr.style "border" "black solid 0.5pt"
      , Attr.style "padding" "5px"
    ][
      Html.div [][]
      , Html.div [][ getActionButton "panel_close" "schließen" [] (TO.ToggleConfigApiForm) ]
    ]

getTaskNameForm: Model -> Html Msg
getTaskNameForm model =
  let
    taskName = case model.tempTaskName of
      Just name -> name
      Nothing -> ""
    taskUuid = case model.tempTaskUuid of
      Just uuid -> uuid
      Nothing -> ""
    button = case model.tempTaskName of
      Just n -> if not (String.isEmpty n)
        then if String.isEmpty taskUuid
          then getActionButton "tick" "ok" [] (TO.AddTask)
          else getActionButton "tick" "ok" [] (TO.SetTask taskUuid)
        else getActionButton "panel_close" "schließen" [] (TO.ToggleTasknameForm)
      Nothing -> getActionButton "panel_close" "schließen" [] (TO.ToggleTasknameForm)
    returnEvent = case model.tempTaskName of
      Just n -> if not (String.isEmpty n)
        then if String.isEmpty taskUuid
          then TO.AddTaskWithEnter
          else (TO.SetTaskWithEnter taskUuid)
        else TO.NoOpInt
      Nothing -> TO.NoOpInt
  in
    Html.div[
      Attr.style "border" "black solid 0.5pt"
      , Attr.style "padding" "5px"
    ][
      Html.div [][
        Html.input [
          Attr.id "taskName"
          , Attr.type_ "text"
          , Attr.value taskName
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
        True -> getActionButton "arrow_rotate_back" "entrunden" [Attr.style "width" "20px"] (TO.DeRound t.uuid)
        False -> getActionButton "arrow_rotate_clockwise" "runden" [Attr.style "width" "20px"] (TO.RoundUp t.uuid)
      else Html.text ""
    saveBtn = if t.saved
      then getActionButton "editable" "freigeben" [Attr.style "width" "20px"] (TO.ToggleSaveTask t.uuid)
      else getActionButton "editable_not" "sichern" [Attr.style "width" "20px"] (TO.ToggleSaveTask t.uuid)
    header = if calcedH > 0 then (calcedTime ++ " = " ++ calcedHours) else ""
    taskNameAttr = if t.saved
      then [ Attr.style "padding-right" "5px" ]
      else [ Attr.style "padding-right" "5px", Attr.style "cursor" "pointer", Ev.onClick (EditTaskname t.uuid) ]
  in
    Html.fieldset [
      Attr.style "padding-right" "0px"
      , Attr.style "padding-top" "0px"
    ][
      Html.legend [][
        Html.span taskNameAttr [ Html.text t.taskName ]
        , showActionButtonInTask t (getActionButton "delete" "löschen" [Attr.style "width" "20px"] (TO.RemoveTask t.uuid))
        , Html.span [ Attr.style "padding-right" "5px" ][ Html.text "" ]
        , showActionButtonInTask t roundBtn
        , Html.span [ Attr.style "padding-right" "5px" ][ Html.text "" ]
        , saveBtn
      ]
      , Html.table [ Attr.style "width" "100%" ][
        Html.thead [][
          Html.tr [][
            Html.td [ Attr.colspan 2, Attr.style "text-align" "center", Attr.style "width" "100%" ][ Html.text header ]
            , Html.td [][ showActionButtonInTask t (getActionButton "plus_rect" "hinzufügen" [Attr.style "width" "20px"] (TO.SetTimeAndAddBooking t.uuid)) ]
          ]
        ]
        , Html.tbody [] (List.map (getBookingRow t) t.timeList)
      ]
    ]

showActionButtonInTask: MyTask -> Html Msg -> Html Msg
showActionButtonInTask t btn = if not t.saved then btn else Html.text ""
