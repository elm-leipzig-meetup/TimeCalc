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
import Devs.Templates as T exposing (getTaskNameForm,getActionButton,getTask)

import Debug exposing (log)

-- Methods

-- View
view : Model -> Html Msg
view model =
  let
    taskDiv = if model.showTaskNameForm
      then getTaskNameForm model
      else Html.div [] (
        List.append
          [ Html.div [ Attr.style "text-align" "right" ][ getActionButton "neuer Task" [] (TO.ToggleTasknameForm) ] ]
          (List.map getTask model.taskList)
        )
  in
    Html.div [
      Attr.style "margin-bottom" "10px"
      , Attr.style "width" "190px"
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
      Ports.pushDataToStore (initialModel.taskList, initialModel.showTaskNameForm, True)
      , Task.perform SetTimeZone Time.here
    ]
  )
--init =  ( initialModel, Cmd.none )
