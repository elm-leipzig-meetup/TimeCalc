port module TimeCalc exposing (..)
--elm make CreateNewLecture.elm --output=pub/createNewLecture/createNewLecture_HSH.js
--elm make --debug TimeCalc.elm --output=/var/www/html/TimeCalc/timeCalc.js

import Devs.Ports as Ports exposing (setDataFromStore)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Ev exposing (onClick, onInput, on, keyCode)
import Html.Extra as HtmlE exposing (..)
import Time
import Task
import Round
import FormatNumber as FN exposing (format)
import FormatNumber.Locales as FNL

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Update as U exposing (..)
import Devs.Utils as DU exposing (getTaskForEdit)
import Devs.Templates as T exposing (getTaskNameForm,getActionButton,getTask, getConfigForm, getFormDiv)

import Debug exposing (log)

-- Methods

-- View
view : Model -> Html Msg
view model =
  let
    t1 = case model.t1 of
      Just myTime -> myTime
      Nothing -> O.getEmptyTime
    t2 = case model.t2 of
      Just myTime -> myTime
      Nothing -> O.getEmptyTime
    t3 = case model.t3 of
      Just myTime -> myTime
      Nothing -> 0.0
    germanLoc = { decimals = FNL.Exact 2, thousandSeparator = ".", decimalSeparator = ",", negativePrefix = "−", negativeSuffix = "", positivePrefix = "", positiveSuffix = "", zeroPrefix = "", zeroSuffix = "" }
    --times = Round.round 2 ((((toFloat t1.hour) + ((toFloat t1.minute) / 60)) + ((toFloat t2.hour) + ((toFloat t2.minute) / 60)) + t3) / 8 * 800)
    times = FN.format germanLoc ((((toFloat t1.hour) + ((toFloat t1.minute) / 60)) + ((toFloat t2.hour) + ((toFloat t2.minute) / 60)) + t3) / 8 * 800)
    calcFormBtn = if (not model.showCalcForm)
      then T.getActionButton "euro_list" "Zeitformular anzeigen" [] (TO.ClearTimes)
      else T.getActionButton "euro_list_i" "Zeitformular leeren" [] (TO.ClearTimes)
    calcForm = if (not model.showCalcForm)
      then Html.div[][ HtmlE.nothing ]
      else Html.div[ ][
          Html.label [Attr.for "t1h", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "CU" ]
          , Html.input [Attr.id "t1h", Attr.placeholder "hhh", Attr.style "width" "3em", Attr.type_ "number", Attr.min "0", Attr.value (String.fromInt t1.hour) , Ev.onInput (TO.SetT1 "h")][]
          , Html.input [Attr.id "t1m", Attr.placeholder "mm", Attr.style "width" "2.5em", Attr.type_ "number", Attr.min "0", Attr.max "59", Attr.value (String.fromInt t1.minute), Ev.onInput (TO.SetT1 "m")][]
          , Html.br[][]
          , Html.label [Attr.for "t2", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "LYSS" ]
          , Html.input [Attr.id "t2h", Attr.placeholder "hhh", Attr.style "width" "3em", Attr.type_ "number", Attr.min "0", Attr.value (String.fromInt t2.hour), Ev.onInput (TO.SetT2 "h")][]
          , Html.input [Attr.id "t2m", Attr.placeholder "mm", Attr.style "width" "2.5em", Attr.type_ "number", Attr.min "0", Attr.max "59", Attr.value (String.fromInt t2.minute), Ev.onInput (TO.SetT2 "m")][]
          , Html.br[][]
          , Html.label [Attr.for "t3", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "Redmine" ]
          , Html.input [Attr.id "t3", Attr.style "width" "5em", Attr.style "text-align" "right", Attr.type_ "number", Attr.step "0.01", Attr.value (String.fromFloat t3), Ev.onInput TO.SetT3][]
          , Html.div[ Attr.style "display" "inline-block", Attr.style "position" "relative", Attr.style "left" "20pt", Attr.style "top" "-37pt" ][ Html.text (times ++ " €") ]
        ]
    taskDiv = case model.taskUuidForDel of
        Just tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
          in
            Html.div [][
              Html.text (tForEdit.taskName ++ " löschen?")
              , T.getActionButton "tick" "löschen" [] (TO.RemoveTaskConfirmed)
              , Html.span [ Attr.style "padding-right" "5px" ][ Html.text "" ]
              , T.getActionButton "panel_close" "abbrechen" [] (TO.CancelRemoveTask)
              ]
        Nothing -> if model.showTaskNameForm then T.getTaskNameForm model
          else if model.showConfigApiForm then T.getConfigForm model
          else Html.div [] (
            List.append
              [ Html.div [ Attr.style "text-align" "right", Attr.style "width" "225pt" ][
                calcFormBtn
                , Html.span [ Attr.style "padding-right" "10px" ][ Html.text "" ]
                , T.getActionButton "cog" "Konfigurieren" [] (TO.ToggleConfigApiForm)
                , Html.span [ Attr.style "padding-right" "10px" ][ Html.text "" ]
                , T.getActionButton "plus_rect" "neuer Task" [] (TO.ToggleTasknameForm)
              ]
              , calcForm ]
              (List.map (T.getTask model) (List.sortBy .taskName model.taskList))
            )
    comment = case model.commentID of
        Nothing -> HtmlE.nothing
        Just ( tUuid, bUuid ) ->
          let
            bList = List.filter (\b -> b.uuid == bUuid) (DU.getTaskForEdit model tUuid).timeList
            booking = Maybe.withDefault O.getEmptyBooking (List.head bList)
            delCommentBtn = T.getActionButton "delete" "Kommentar löschen" [Attr.style "width" "20px", Attr.style "margin-left" "5px"] (TO.SetBookingComment tUuid bUuid "")
            --commentForm = Html.textarea [ Attr.style "width" "480px", Attr.style "height" "80px", Ev.onInput (TO.SetBookingComment tUuid bUuid) ][ Html.text (Maybe.withDefault "" booking.comment) ]
            commentForm = Html.textarea [ Attr.value (Maybe.withDefault "" booking.comment), Attr.style "width" "480px", Attr.style "height" "80px", Ev.onInput (TO.SetBookingComment tUuid bUuid) ][]
          in
            T.getFormDiv model 100 (Html.div[][commentForm]) (not (model.commentID == Nothing)) [delCommentBtn] (ToggleCommentForm Nothing)
  in
    Html.div [
      Attr.style "margin-bottom" "10px"
      , Attr.style "width" "225pt"
    ][comment, taskDiv]

main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model = Ports.setDataFromStore ReadDataFromPublish

init : ( Model, Cmd Msg )
init =  ( initialModel,
    Cmd.batch [
      Ports.pushDataToStore (O.getTransferObj initialModel.taskList initialModel.apiList initialModel.showTaskNameForm initialModel.showCalcForm True)
      , Task.perform SetTimeZone Time.here
    ]
  )
--init =  ( initialModel, Cmd.none )
