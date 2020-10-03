module Main exposing (main)

import Browser
import CourseCredits exposing (Credit, CreditSubject(..), Credits, Subject, Subjects, decodeSubjects)
import Html exposing (Html, button, div, input, option, text)
import Html.Attributes exposing (disabled, name, placeholder, selected, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed
import IndexedList exposing (Index)
import Json.Decode



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { allSubjects : Subjects
    , availableSubjects : Subjects
    , credits : Credits
    }


init : Json.Decode.Value -> ( Model, Cmd Msg )
init subjects =
    let
        allSubjects =
            Json.Decode.decodeValue decodeSubjects subjects |> Result.withDefault []
    in
    ( { allSubjects = allSubjects
      , availableSubjects = allSubjects
      , credits = CourseCredits.empty
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = ChangedCreditSubject Index (Maybe Subject)
    | HoursChanged Index Float
    | SubjectAdded
    | SubjectRemoved Index



-- update : Msg -> Model -> Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedCreditSubject index maybeSubject ->
            ( changeCreditSubject index maybeSubject model, Cmd.none )

        HoursChanged index hours ->
            ( updateCredits (CourseCredits.updateHours index hours) model, Cmd.none )

        SubjectAdded ->
            ( updateCredits (CourseCredits.addCredit (Credit NotSelected 0)) model, Cmd.none )

        SubjectRemoved index ->
            ( removeCreditSubject index model, Cmd.none )


removeCreditSubject : Index -> Model -> Model
removeCreditSubject index model =
    model
        |> updateCredits (CourseCredits.remove index)
        |> updateAvailableSubjects


updateCredits : (Credits -> Credits) -> Model -> Model
updateCredits updatefn model =
    { model | credits = updatefn model.credits }


changeCreditSubject : Index -> Maybe Subject -> Model -> Model
changeCreditSubject index maybeSubject model =
    model
        |> updateSubject index maybeSubject
        |> updateAvailableSubjects


updateSubject : Index -> Maybe Subject -> Model -> Model
updateSubject index maybeSubject model =
    case maybeSubject of
        Nothing ->
            model

        Just subject ->
            { model | credits = CourseCredits.updateSubject index subject model.credits }


updateAvailableSubjects : Model -> Model
updateAvailableSubjects model =
    { model | availableSubjects = CourseCredits.diffSubjects (CourseCredits.getSelectedSubjects model.credits) model.allSubjects }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (model.credits |> IndexedList.map2List (renderCredit model))
        , div [] [ button [ onClick SubjectAdded, disabled (model |> canAddSubject |> not) ] [ text "Add subject" ] ]
        ]


canAddSubject : Model -> Bool
canAddSubject model =
    (model.availableSubjects |> List.length |> (/=) 0) && (model.credits |> CourseCredits.anyMissingSubject |> not)


renderCredit : Model -> Index -> Credit -> Html Msg
renderCredit model index credit =
    div []
        [ Keyed.node "select"
            [ name "subjects", onInput (subjectChanged model.allSubjects index) ]
            (subjectOptions credit model.availableSubjects)
        , input
            [ placeholder "Hours"
            , type_ "number"
            , name ("subjectHours-" ++ String.fromInt index)
            , value
                (if credit.hours > 0 then
                    String.fromFloat credit.hours

                 else
                    ""
                )
            , onInput (parseHourInput >> HoursChanged index)
            ]
            []
        , button [ onClick (SubjectRemoved index) ] [ text "Remove" ]
        ]


subjectOptions : Credit -> Subjects -> List ( String, Html Msg )
subjectOptions credit availableSubjects =
    let
        options =
            availableSubjects |> List.map subjectOption

        selectedSubject =
            case credit.subject of
                NotSelected ->
                    ( "empty", option [ value "0" ] [ text "-- Choose subject --" ] )

                Selected subject ->
                    ( "subject.name", option [ value (String.fromInt subject.id), selected True ] [ text subject.name ] )
    in
    selectedSubject :: options


subjectOption : Subject -> ( String, Html Msg )
subjectOption subject =
    ( subject.name, option [ value <| String.fromInt subject.id, selected False ] [ text subject.name ] )


subjectChanged : List Subject -> Index -> String -> Msg
subjectChanged allSubjects index stringId =
    stringId
        |> parseSubjectId
        |> CourseCredits.getSubjectById allSubjects
        |> ChangedCreditSubject index


parseSubjectId : String -> Int
parseSubjectId id =
    id
        |> String.toInt
        |> Maybe.withDefault 0


parseHourInput : String -> Float
parseHourInput str =
    let
        converted =
            String.toFloat str
    in
    case converted of
        Just hours ->
            if hours > 0 then
                hours

            else
                0

        Nothing ->
            0
