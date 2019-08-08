module Devs.Update exposing (..)

import Devs.Ports as Ports exposing (pushDataToStore)

import List exposing (..)
import List.Extra as ListE
import Debug exposing (log)
import UUID exposing (UUID)
import Random
import Task
import Time

import Devs.Objects as O exposing (..)
import Devs.TypeObject as TO exposing (..)
import Devs.Utils as DU exposing (getSeed, toTime, calculateTime, focusSearchBox, getTaskForEdit, roundUpTime, getMyTimeFromPosix)

--            _ = Debug.log "newUuid: " newUuid
-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model , Cmd.none)
        NoOpStr val -> ( model , Cmd.none)
        NoOpInt val -> ( model , Cmd.none)
        ReadDataFromPublish (taskList, showTaskNameForm, random) -> ( { model | showTaskNameForm = showTaskNameForm, taskList = taskList, random = random } , Cmd.none)
        ToggleTasknameForm ->
          let
            cmds = if model.showTaskNameForm
              then Ports.pushDataToStore (model.taskList, not model.showTaskNameForm, False)
              else Cmd.batch [
                  DU.focusSearchBox ("taskName")
                  , Ports.pushDataToStore (model.taskList, not model.showTaskNameForm, False)
                ]
          in
          ( { model | showTaskNameForm = not model.showTaskNameForm } , cmds)
        SetTempTaskname name -> ( { model | tempTaskName = Just name } , Cmd.none)
        EditTaskname tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
          in
            ( { model | tempTaskUuid = Just tUuid,
              tempTaskName = Just tForEdit.taskName,
              showTaskNameForm = True } , DU.focusSearchBox ("taskName"))
        SetTask tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            tempTaskName = case model.tempTaskName of
                Just string -> string
                Nothing -> ""
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | taskName = tempTaskName}) model.taskList
          in
            ( {model | taskList = newTaskList,
                tempTaskName = Nothing
                , tempTaskUuid = Nothing
                , showTaskNameForm = False
              }
              , Ports.pushDataToStore (newTaskList, False, False)
            )
        SetTaskWithEnter tUuid key ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            tempTaskName = case model.tempTaskName of
                Just string -> string
                Nothing -> ""
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | taskName = tempTaskName}) model.taskList
          in
            if key == 13
              then ( {model | taskList = newTaskList,
                  tempTaskName = Nothing
                  , tempTaskUuid = Nothing
                  , showTaskNameForm = False
                }
                , Ports.pushDataToStore (newTaskList, False, False)
              )
            else ( model, Cmd.none )
        AddTask ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            emptyTask = O.getEmptyTask
            tempTaskName = case model.tempTaskName of
                Just string -> string
                Nothing -> ""
            newTaskList = List.append model.taskList [{ emptyTask | taskName = tempTaskName, uuid = (UUID.toString newUuid) }]
          in
            ( {model | taskList = newTaskList,
                tempTaskName = Nothing,
                showTaskNameForm = False,
                currentSeed = Just newSeed
              }
              , Ports.pushDataToStore (newTaskList, False, False)
            )
        AddTaskWithEnter key ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            emptyTask = O.getEmptyTask
            tempTaskName = case model.tempTaskName of
                Just string -> string
                Nothing -> ""
            newTaskList = List.append model.taskList [{ emptyTask | taskName = tempTaskName, uuid = (UUID.toString newUuid) }]
          in
            if key == 13
              then ( {model | taskList = newTaskList, tempTaskName = Nothing, showTaskNameForm = False, currentSeed = Just newSeed} , Ports.pushDataToStore (newTaskList, False, False))
              else ( model , Cmd.none)
        RemoveTask tUuid ->
          let
            newTaskList = List.filter (\item -> (item.uuid /= tUuid)) model.taskList
            showTaskNameForm = if List.length newTaskList == 0 then True else False
          in
            ( {model | taskList = newTaskList, showTaskNameForm = showTaskNameForm} , Ports.pushDataToStore (newTaskList, showTaskNameForm, False))
        SetTimeAndAddBooking tUuid -> (model, Task.perform (SetTimezoneAndAddBooking tUuid) Time.now)
        SetTimezoneAndAddBooking tUuid time -> (model, Task.perform (AddBooking tUuid time) Time.here)
        AddBooking tUuid time zone ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            tForEdit = DU.getTaskForEdit model tUuid
            emptyBooking = O.getEmptyBooking
            now = DU.getMyTimeFromPosix time zone
            newTimeList = List.append tForEdit.timeList [{ emptyBooking | from = Just now, uuid = (UUID.toString newUuid) }]
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList}) model.taskList
          in
            ( {model | taskList = newTaskList, currentSeed = Just newSeed}
            ,
              Cmd.batch [
                DU.focusSearchBox ("from_" ++ (UUID.toString newUuid))
                , Ports.pushDataToStore (newTaskList, model.showTaskNameForm, False)
              ]
            )

        RemoveBooking tUuid uuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = List.filter (\item -> (item.uuid /= uuid)) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (newTaskList, model.showTaskNameForm, False) )
        SetFrom tUuid uuid newTime ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = ListE.updateIf (\item -> item.uuid == uuid) (\item -> {item | from = Just (DU.toTime newTime)}) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (newTaskList, model.showTaskNameForm, False) )
        SetTo tUuid uuid newTime ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = ListE.updateIf (\item -> item.uuid == uuid) (\item -> {item | to = Just (DU.toTime newTime)}) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (newTaskList, model.showTaskNameForm, False) )
        PreSetTo tUuid bUuid -> (model, Task.perform (PreSetTo_Int1 tUuid bUuid) Time.now)
        PreSetTo_Int1 tUuid bUuid time -> (model, Task.perform (PreSetTo_Int2 tUuid bUuid time) Time.here)
        PreSetTo_Int2 tUuid bUuid time zone ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            now = DU.getMyTimeFromPosix time zone
            newTimeList = ListE.updateIf (\item -> item.uuid == bUuid) (\item -> {item | to = Just now}) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (newTaskList, model.showTaskNameForm, False) )
        RoundUp tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = map DU.roundUpTime tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, rounded = True, calcedTime = DU.calculateTime newTimeList True }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (newTaskList, model.showTaskNameForm, False) )
        DeRound tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = map (\item -> { item | rounded = Nothing }) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, rounded = False, calcedTime = DU.calculateTime newTimeList False }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (newTaskList, model.showTaskNameForm, False) )
