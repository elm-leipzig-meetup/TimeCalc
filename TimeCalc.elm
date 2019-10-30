port module TimeCalc exposing (..)
--elm make CreateNewLecture.elm --output=pub/createNewLecture/createNewLecture_HSH.js
--elm make --debug TimeCalc.elm --output=/var/www/html/TimeCalc/timeCalc.js

import Devs.Ports as Ports exposing (setDataFromStore)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Time
import Task

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Update as U exposing (..)
import Devs.Utils as DU exposing (getTaskForEdit)
import Devs.Templates as T exposing (getTaskNameForm,getActionButton,getTask, getConfigForm)

import Debug exposing (log)

-- Methods

-- View
view : Model -> Html Msg
view model =
  let
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
                T.getActionButton "cog" "Konfigurieren" [] (TO.ToggleConfigApiForm)
                , Html.span [ Attr.style "padding-right" "10px" ][ Html.text "" ]
                , T.getActionButton "plus_rect" "neuer Task" [] (TO.ToggleTasknameForm)
              ] ]
              (List.map (T.getTask model) (List.sortBy .taskName model.taskList))
            )
  in
    Html.div [
      Attr.style "margin-bottom" "10px"
      , Attr.style "width" "225pt"
    ][ taskDiv ]

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
      Ports.pushDataToStore (O.getTransferObj initialModel.taskList initialModel.apiList initialModel.showTaskNameForm True)
      , Task.perform SetTimeZone Time.here
    ]
  )
--init =  ( initialModel, Cmd.none )
