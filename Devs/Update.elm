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
import Devs.Utils as DU exposing (getSeed, toTime, calculateTime, focusSearchBox, getTaskForEdit, roundUpTime, getMyTimeFromPosix, getApiType, getApiForEdit)

--            _ = Debug.log "newUuid: " newUuid
-- Update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model , Cmd.none)
        NoOpStr val -> ( model , Cmd.none)
        NoOpInt val -> ( model , Cmd.none)
        ReadDataFromPublish tO2 -> ( { model | showTaskNameForm = tO2.showTaskNameForm, taskList = tO2.tasks, random = tO2.random, apiList = tO2.apiList } , Cmd.none)
        ToggleConfigApiForm ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            newApi = O.getEmptyApi
            return = if model.showConfigApiForm
              -- ausblenden
              then ( { model | showConfigApiForm = not model.showConfigApiForm, apiForAdd=Nothing } , Cmd.none)
              -- anzeigen
              else ( { model | showConfigApiForm = not model.showConfigApiForm
                , currentSeed = Just newSeed
                , apiForAdd = Just { newApi | uuid = UUID.toString newUuid }
                } , Cmd.none)
          in
            return
        AddConfigApi ->
          case model.apiForAdd of
              Just api -> if not (String.isEmpty api.apiUrl)
                then ( { model | showConfigApiForm = not model.showConfigApiForm, apiList = List.append model.apiList [api], apiForAdd=Nothing }
                , Ports.pushDataToStore (O.getTransferObj model.taskList (List.append model.apiList [api]) model.showTaskNameForm False))
                else ( model, Cmd.none)
              Nothing -> ( { model | showConfigApiForm = not model.showConfigApiForm, apiForAdd=Nothing } , Cmd.none)
        RemoveApi aUuid ->
          let
            newApiList = List.filter (\item -> (item.uuid /= aUuid)) model.apiList
          in
            ( {model | apiList = newApiList}
            , Ports.pushDataToStore (O.getTransferObj model.taskList newApiList model.showTaskNameForm False))
        SetApiType aUuid ->
          let
            afa = case model.apiForAdd of
              Just api -> Just { api | apiType = DU.getApiType model aUuid }
              Nothing -> Nothing
          in
            ( { model | apiForAdd = afa} , Cmd.none)
        SetUrl val ->
          let
            afa = case model.apiForAdd of
              Just api -> Just { api | apiUrl = val }
              Nothing -> Nothing
          in
            ( { model | apiForAdd = afa} , Cmd.none)
        SetUser val ->
          let
            afa = case model.apiForAdd of
              Just api -> Just { api | user = val }
              Nothing -> Nothing
          in
            ( { model | apiForAdd = afa} , Cmd.none)
        SetPwd val ->
          let
            afa = case model.apiForAdd of
              Just api -> Just { api | password = val }
              Nothing -> Nothing
          in
            ( { model | apiForAdd = afa} , Cmd.none)
        SetTimeZone zone -> ( { model | timeZone = zone } , Cmd.none)
        ToggleTasknameForm ->
          let
            cmds = if model.showTaskNameForm
              then Ports.pushDataToStore (O.getTransferObj model.taskList model.apiList (not model.showTaskNameForm) False)
              else Cmd.batch [
                  DU.focusSearchBox ("taskName")
                  , Ports.pushDataToStore (O.getTransferObj model.taskList model.apiList (not model.showTaskNameForm) False)
                ]
          in
          ( { model | showTaskNameForm = not model.showTaskNameForm } , cmds)
        SetTempTaskname name -> ( { model | tempTaskName = Just name } , Cmd.none)
        SetTempApi aUuid -> ( { model | tempTaskApi = DU.getApiForEdit model aUuid } , Cmd.none)
        EditTaskname tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
          in
            ( { model | tempTaskUuid = Just tUuid
              , tempTaskName = Just tForEdit.taskName
              , tempTaskApi = tForEdit.api
              , showTaskNameForm = True } , DU.focusSearchBox ("taskName"))
        SetTask tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            tempTaskName = case model.tempTaskName of
                Just string -> string
                Nothing -> ""
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | taskName = tempTaskName, api=model.tempTaskApi}) model.taskList
          in
            ( {model | taskList = newTaskList,
                tempTaskName = Nothing
                , tempTaskApi = Nothing
                , tempTaskUuid = Nothing
                , showTaskNameForm = False
              }
              , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList False False)
            )
        SetTaskWithEnter tUuid key ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            tempTaskName = case model.tempTaskName of
                Just string -> string
                Nothing -> ""
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | taskName = tempTaskName, api=model.tempTaskApi}) model.taskList
          in
            if key == 13
              then ( {model | taskList = newTaskList,
                  tempTaskName = Nothing
                  , tempTaskApi = Nothing
                  , tempTaskUuid = Nothing
                  , showTaskNameForm = False
                }
                , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList False False)
              )
            else ( model, Cmd.none )
        AddTask -> (model, Task.perform AddTask_Int Time.now)
        AddTask_Int time ->
          let
            ( newUuidT, newSeedT ) = Random.step UUID.generator (DU.getSeed model)
            ( newUuidB, newSeedB ) = Random.step UUID.generator newSeedT
            emptyTask = O.getEmptyTask
            emptyBooking = O.getEmptyBooking
            now = DU.getMyTimeFromPosix time model.timeZone
            tempTaskName = case model.tempTaskName of
                Just string -> string
                Nothing -> ""
            newTimeList = [{ emptyBooking | from = Just now, uuid = (UUID.toString newUuidB) }]
            newTask = { emptyTask | taskName = tempTaskName, api=model.tempTaskApi, timeList = newTimeList, uuid = (UUID.toString newUuidT) }
            newTaskList = List.append model.taskList [newTask]
          in
            ( {model | taskList = newTaskList
                , tempTaskName = Nothing
                , tempTaskApi = Nothing
                , showTaskNameForm = False
                , currentSeed = Just newSeedB
              }
              , Cmd.batch [
                  DU.focusSearchBox ("from_" ++ (UUID.toString newUuidB))
                  , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList False False)
                ]
            )
        AddTaskWithEnter key ->
          if key == 13
            then (model, Task.perform AddTask_Int Time.now)
            else ( model , Cmd.none)
        RemoveTask tUuid -> ( {model | taskUuidForDel = Just tUuid} , Cmd.none)
        CancelRemoveTask -> ( {model | taskUuidForDel = Nothing} , Cmd.none)
        RemoveTaskConfirmed ->
          let
            tUuid = case model.taskUuidForDel of
                Just uuid -> uuid
                Nothing -> UUID.toString UUID.nil
            newTaskList = List.filter (\item -> (item.uuid /= tUuid)) model.taskList
            showTaskNameForm = if List.length newTaskList == 0 then True else False
          in
            ( {model | taskUuidForDel = Nothing, taskList = newTaskList, showTaskNameForm = showTaskNameForm}
            , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList showTaskNameForm False))
        ToggleSaveTask tUuid ->
          let
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | saved = not(item.saved) }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False) )
        SetTimeAndAddBooking tUuid -> (model, Task.perform (AddBooking tUuid) Time.now)
        AddBooking tUuid time ->
          let
            ( newUuid, newSeed ) = Random.step UUID.generator (DU.getSeed model)
            tForEdit = DU.getTaskForEdit model tUuid
            emptyBooking = O.getEmptyBooking
            now = DU.getMyTimeFromPosix time model.timeZone
            newTimeList = List.append tForEdit.timeList [{ emptyBooking | from = Just now, uuid = (UUID.toString newUuid) }]
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList}) model.taskList
          in
            ( {model | taskList = newTaskList, currentSeed = Just newSeed}
            ,
              Cmd.batch [
                DU.focusSearchBox ("from_" ++ (UUID.toString newUuid))
                , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False)
              ]
            )

        RemoveBooking tUuid uuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = List.filter (\item -> (item.uuid /= uuid)) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False) )
        SetFrom tUuid uuid newTime ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = ListE.updateIf (\item -> item.uuid == uuid) (\item -> {item | from = Just (DU.toTime newTime)}) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False) )
        SetTo tUuid uuid newTime ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = ListE.updateIf (\item -> item.uuid == uuid) (\item -> {item | to = Just (DU.toTime newTime)}) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False) )
        PreSetTo tUuid bUuid -> (model, Task.perform (PreSetTo_Int tUuid bUuid) Time.now)
        PreSetTo_Int tUuid bUuid time ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            now = DU.getMyTimeFromPosix time model.timeZone
            newTimeList = ListE.updateIf (\item -> item.uuid == bUuid) (\item -> {item | to = Just now}) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, calcedTime = DU.calculateTime newTimeList tForEdit.rounded }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False) )
        RoundUp tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = map DU.roundUpTime tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, rounded = True, calcedTime = DU.calculateTime newTimeList True }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False) )
        DeRound tUuid ->
          let
            tForEdit = DU.getTaskForEdit model tUuid
            newTimeList = map (\item -> { item | rounded = Nothing }) tForEdit.timeList
            newTaskList = ListE.updateIf (\item -> item.uuid == tUuid) (\item -> {item | timeList = newTimeList, rounded = False, calcedTime = DU.calculateTime newTimeList False }) model.taskList
          in
            ( { model | taskList = newTaskList } , Ports.pushDataToStore (O.getTransferObj newTaskList model.apiList model.showTaskNameForm False) )
