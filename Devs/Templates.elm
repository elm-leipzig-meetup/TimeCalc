module Devs.Templates exposing (getTaskNameForm,getActionButton,getTask,getConfigForm, getFormDiv)

import Html exposing (..)
import Html.Extra as HtmlE exposing ( .. )
import Html.Attributes as Attr exposing (..)
import Html.Events as Ev exposing (onClick, onInput, on, keyCode)
import Html.Events.Extra as EvE exposing (onChange)
import Json.Decode as Json
import List exposing (..)
import List.Extra as ListE exposing (..)

import Devs.TypeObject as TO exposing (..)
import Devs.Objects as O exposing (..)
import Devs.Utils as DU exposing (getFormatedTime, getHoursOfTime)

getBookingRow: MyTask -> Booking -> Html Msg
getBookingRow t booking =
  let
    useRounded = t.rounded
    tUuid = t.uuid
    number = case booking.nr of
        Just nr -> nr
        Nothing -> ""
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
    nrField = case t.api of
      Just apit -> if t.saved
        then Html.text number
        else Html.input [
          Ev.onInput (TO.SetNr tUuid booking.uuid)
          , Attr.value number
          , Attr.id ("nr_" ++ booking.uuid)
          , Attr.style "width" "60px"
        ][]
      Nothing -> Html.text ""
    syncBtn = case t.api of
      Just api -> showActionButtonInTask t (getActionButton Nothing "arrow_two_directions" "sync" [Attr.style "width" "20px"] (TO.SyncToExtern tUuid booking.uuid))
      Nothing -> Html.text ""
    comment = Maybe.withDefault "leer" booking.comment
  in
    Html.tr [][
      Html.td [ Attr.style "text-align" "right" ][ fromField ]
      , Html.td [][ toField ]
      , Html.td [][ nrField ]
      , Html.td [ Attr.style "white-space" "nowrap" ][
        Html.input [ Attr.id ("copy_" ++ booking.uuid), Attr.value comment, Attr.style "position" "absolute", Attr.style "left" "-300px" ][]
        , syncBtn
        , if t.saved && comment /= "leer"
          --then Html.img [Attr.title comment, Attr.style "height" "15px", Attr.src ("img/comment.svg")][]
          then getActionButton Nothing "comment" ("Kommentar: " ++ comment) [Attr.style "width" "20px"] (TO.CopyToClipboard ("copy_" ++ booking.uuid))
          else HtmlE.nothing
        , showActionButtonInTask t (getActionButton Nothing "comment" ("Kommentar: " ++ comment) [Attr.style "width" "20px"] (TO.ToggleCommentForm (Just (tUuid, booking.uuid))))
        , showActionButtonInTask t (getActionButton Nothing "minus_rect" "löschen" [Attr.style "width" "20px"] (TO.RemoveBooking tUuid booking.uuid))
      ]
    ]

getActionButton: Maybe String -> String -> String -> List (Html.Attribute Msg) -> Msg -> Html Msg
getActionButton idObj sign title styles event =
--  Html.input (List.append [ Ev.onClick event, Attr.type_ "button", Attr.value sign, Attr.style "cursor" "pointer" ] styles) []
  Html.button (List.append [
      Ev.onClick event
      , Attr.id (Maybe.withDefault "" idObj)
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
  let
    ticketUrl = case model.ticketUrl of
      Just url -> url
      Nothing -> ""
  in
    Html.div[
      Attr.style "border" "black solid 0.5pt"
      , Attr.style "padding" "5px"
      , Attr.style "width" "215pt"
    ][
      Html.div[][
        Html.label [Attr.for "tUrl", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "Ticket-URL" ]
        , Html.input [Attr.id "tUrl", Attr.type_ "url", Attr.value ticketUrl, Ev.onInput TO.SetTicketUrl][]
      ]
      , Html.div [][
        Html.label [Attr.for "typ", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "Typ" ]
        , Html.select [
          Attr.id "typ"
          , Attr.style "width" "156px"
          , EvE.onChange TO.SetApiType
        ] (List.append [getEmptyOption] (List.map (\at -> Html.option[Attr.value at.uuid][Html.text at.name]) model.apiTypeList))
        , Html.br[][]
        , Html.label [Attr.for "url", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "URL" ]
        , Html.input [Attr.id "url", Attr.type_ "url", Ev.onInput TO.SetUrl][]
        , Html.br[][]
        , Html.label [Attr.for "user", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "User" ]
        , Html.input [Attr.id "user", Ev.onInput TO.SetUser][]
        , Html.br[][]
        , Html.label [Attr.for "pwd", Attr.style "width" "40px", Attr.style "display" "inline-block"][ Html.text "Pwd" ]
        , Html.input [Attr.id "pwd", Attr.type_ "password", Ev.onInput TO.SetPwd][]
        , Html.br[][]
      ]
      , Html.div [][
        getActionButton Nothing "tick" "hinzufügen" [] (TO.AddConfigApi)
        , getActionButton Nothing "panel_close" "schließen" [] (TO.ToggleConfigApiForm)
      ], Html.div [][
        Html.table[](List.map (\a -> Html.tr[][ Html.td[][ if showDelBtn model.taskList a.uuid then getActionButton Nothing "delete" "löschen" [] (TO.RemoveApi a.uuid) else Html.text "" ], Html.td[][ Html.text (a.apiUrl ++ " (" ++ a.user ++ ")") ] ]) model.apiList)
      ]
    ]

showDelBtn: List MyTask -> String -> Bool
showDelBtn taskList aUuid =
  case List.head (List.filter (\item -> (hasTaskThisApi item aUuid)) taskList) of
    Just tl -> False
    Nothing -> True
hasTaskThisApi: MyTask -> String -> Bool
hasTaskThisApi mt aUuid =
  case mt.api of
    Just api -> if api.uuid == aUuid then True else False
    Nothing -> False

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
          then getActionButton Nothing "tick" "ok" [] (TO.AddTask)
          else getActionButton Nothing "tick" "ok" [] (TO.SetTask taskUuid)
        else getActionButton Nothing "panel_close" "schließen" [] (TO.ToggleTasknameForm)
      Nothing -> getActionButton Nothing "panel_close" "schließen" [] (TO.ToggleTasknameForm)
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
          , Ev.on "keydown" (Json.map returnEvent Ev.keyCode)
          , Ev.onInput TO.SetTempTaskname
          , Attr.placeholder "Taskname"
          --, Attr.maxlength 15
        ][]
        , Html.select [
          Attr.id "typ"
          , Attr.style "width" "156px"
          , EvE.onChange TO.SetTempApi
        ] (List.append [getEmptyOption] (List.map (getApiOption model.tempTaskApi) model.apiList))
        , Html.input [
          Attr.type_ "checkbox"
          , Attr.id "isTicket"
          , Attr.checked model.tempTaskIsTicket
          , Attr.title "Ist dies eine Ticketnummer?"
          , Ev.onCheck TO.SetTempIsTicket
        ][]
      ]
      , Html.div [][ button ]
    ]

getEmptyOption: Html Msg
getEmptyOption = Html.option[Attr.value ""][Html.text ""]

getApiOption: Maybe Api -> Api -> Html Msg
getApiOption sApi api =
  let
    selected = case sApi of
      Just a -> if a.uuid == api.uuid
        then True
        else False
      Nothing -> False
  in
    Html.option[Attr.value api.uuid, Attr.selected selected][Html.text api.apiType.name]

getTask: Model -> MyTask -> Html Msg
getTask model t =
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
        True -> getActionButton Nothing "arrow_rotate_back" "entrunden" [Attr.style "width" "20px"] (TO.DeRound t.uuid)
        False -> getActionButton Nothing "arrow_rotate_clockwise" "runden" [Attr.style "width" "20px"] (TO.RoundUp t.uuid)
      else Html.text ""
    saveBtn = if t.saved
      then getActionButton Nothing "editable" "freigeben" [Attr.style "width" "20px"] (TO.ToggleSaveTask t.uuid)
      else getActionButton Nothing "editable_not" "sichern" [Attr.style "width" "20px"] (TO.ToggleSaveTask t.uuid)
    delBtn = if List.length t.timeList > 0
      then HtmlE.nothing
      else (getActionButton Nothing "delete" "löschen" [Attr.style "width" "20px"] (TO.RemoveTask t.uuid))
    header = if calcedH > 0 then (calcedTime ++ " = " ++ calcedHours) else ""
    ticketUrl = case model.ticketUrl of
      Just url -> url
      Nothing -> ""
    taskApi = case t.api of
      Just api -> api
      Nothing -> O.getEmptyApi
    taskNameList = String.split "|" t.taskName
    taskName = String.trim (Maybe.withDefault "" (List.head taskNameList))
    taskNumber = String.trim (Maybe.withDefault taskName (ListE.last taskNameList))
    taskNameAttr = if t.saved
      then
        if t.isTicket && not (String.isEmpty ticketUrl)
          then [ Attr.style "padding-right" "5px", Attr.style "cursor" "pointer" , Ev.onClick (TO.GoTo (ticketUrl ++ taskNumber) True ) ]
        else if not(String.isEmpty taskApi.apiUrl)
          then [ Attr.style "padding-right" "5px", Attr.style "cursor" "pointer" , Ev.onClick (TO.GoTo (taskApi.apiUrl ++ taskNumber) True ) ]
        else [ Attr.style "padding-right" "5px" ]
      else [ Attr.style "padding-right" "5px", Attr.style "cursor" "pointer", Ev.onClick (TO.EditTaskname t.uuid) ]
    bgColor = if List.length (List.filter (\a -> (Maybe.withDefault O.getEmptyTime a.to) == O.getEmptyTime) t.timeList) > 0 then "#ffa5006e" else "white"
    --displayList = if List.length (List.filter (\a -> (Maybe.withDefault O.getEmptyTime a.to) == O.getEmptyTime) t.timeList) > 0 then "block" else "none"
    displayList = "block"
  in
    Html.fieldset [
      Attr.style "padding-right" "0px"
      , Attr.style "padding-top" "0px"
      , Attr.style "background-color" bgColor
    ][
      Html.legend [][
        Html.span taskNameAttr [ Html.text taskName ]
        , showActionButtonInTask t delBtn
        , Html.span [ Attr.style "padding-right" "5px" ][ Html.text "" ]
        , showActionButtonInTask t roundBtn
        , Html.span [ Attr.style "padding-right" "5px" ][ Html.text "" ]
        , saveBtn
      ]
      , Html.table [ Attr.style "width" "100%", Attr.style "display" displayList ][
        Html.thead [][
          Html.tr [][
            Html.td [ Attr.colspan 3, Attr.style "text-align" "center", Attr.style "width" "100%" ][ Html.text header ]
            , Html.td [][ showActionButtonInTask t (getActionButton Nothing "plus_rect" "hinzufügen" [Attr.style "width" "20px"] (TO.SetTimeAndAddBooking t.uuid)) ]
          ]
        ]
        , Html.tbody [] (List.map (getBookingRow t) t.timeList)
      ]
    ]

showActionButtonInTask: MyTask -> Html Msg -> Html Msg
showActionButtonInTask t btn = if not t.saved then btn else HtmlE.nothing

getFormDiv: Model -> Int -> Html Msg -> Bool -> List (Html Msg) -> Msg -> Html Msg
getFormDiv model width subForm showForm addButtons event =
  let
    leftSpace = (width - 524) // 2
    display = if showForm then "block" else "none"
  in
    Html.div (List.append [ Attr.style "display" display] formBG )[
      Html.div (List.append [ Attr.style "margin-left" ((String.fromInt leftSpace) ++ "px") ] formDiv) [
        subForm
        , Html.div[] (List.append [getActionButton Nothing "panel_close" "schließen" [] event] addButtons)
      ]
    ]

formDiv = [
    Attr.style "background-color" "#fff"
    , Attr.style "margin-bottom" "30px"
    , Attr.style "padding" "15px"
    , Attr.style "-webkit-border-radius" "8px"
    , Attr.style "-moz-border-radius" "8px"
    , Attr.style "-ms-border-radius" "8px"
    , Attr.style "-o-border-radius" "8px"
    , Attr.style "border-radius" "8px"
    , Attr.style "-webkit-box-shadow" "0px 5px 20px rgba(0,0,0,0.3)"
    , Attr.style "-moz-box-shadow" "0px 5px 20px rgba(0,0,0,0.3)"
    , Attr.style "-ms-box-shadow" "0px 5px 20px rgba(0,0,0,0.3)"
    , Attr.style "-o-box-shadow" "0px 5px 20px rgba(0,0,0,0.3)"
    , Attr.style "box-shadow" "0px 5px 20px rgba(0,0,0,0.3)"
    , Attr.style "margin-top" "120px"
    , Attr.style "margin-left" "30%"
    , Attr.style "margin-right" "30%"
    , Attr.style "width" "490px"
    , Attr.style "border" "2px solid #a6b3b3"
    , Attr.style "max-height" "80%"
    , Attr.style "overflow-y" "auto"
    , Attr.style "margin-top" "50px"
  ]

formBG = [
    Attr.style "z-index" "1010"
    , Attr.style "position" "fixed"
    , Attr.style "top" "0"
    , Attr.style "left" "0"
    , Attr.style "width" "100%"
    , Attr.style "height" "100%"
    , Attr.style "background-color" "#ffffff85"
  ]
