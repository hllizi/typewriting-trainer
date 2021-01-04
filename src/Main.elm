module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (field, map, string)
import List exposing (head)
import Random
import Task exposing (perform)
import Time exposing (millisToPosix)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Change
    | CorrectKeyPressed
    | IncorrectKeyPressed
    | ResetCorrectnessMarking
    | NewChar Char
    | LastUpdateTime Time.Posix
    | Pass
    | TimedChange
    | SetMode Mode
    | InduceRunning
    | ToggleRunning
    | StartTime Time.Posix
    | CountDown Int


type alias Model =
    { mode : List CharacterGroup
    , interval : Int
    , subIntervalLength : Int
    , character : Char
    , mistakeCount : Int
    , successCount : Int
    , missedCount : Int
    , lastUpdateTime : Time.Posix
    , startTime : Maybe Time.Posix
    , correctnessState : CorrectnessState
    , running : Bool
    , countDown: Int
    }


type CharacterGroup
    = Upper
    | Middle
    | Lower
    | Numbers


type alias Mode =
    List CharacterGroup


type CorrectnessState
    = Correct
    | Incorrect
    | Neutral


letters : List Char
letters =
    List.map
        Char.fromCode
    <|
        List.range (Char.toCode 'a') (Char.toCode 'z')


lowerRow : List Char
lowerRow =
    [ 'y', 'x', 'v', 'b', 'n', 'm' ]


middleRow : List Char
middleRow =
    [ 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l' ]


middleRowUmlauts : List Char
middleRowUmlauts =
    [ 'ö', 'ä' ]


upperRow : List Char
upperRow =
    [ 'q', 'w', 'e', 'r', 't', 'z', 'u', 'i', 'o', 'p' ]


numbers : List Char
numbers =
    [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]


upperRowUmlauts : List Char
upperRowUmlauts =
    [ 'ü' ]


umlauts : List Char
umlauts =
    [ 'ä', 'ö', 'ü' ]


capital : List Char -> List Char
capital =
    List.map Char.toUpper


characterList : Mode -> List Char
characterList mode =
    List.concat <|
        List.map
            (\group ->
                case group of
                    Upper ->
                        List.append upperRow upperRowUmlauts

                    Middle ->
                        List.append middleRow middleRowUmlauts

                    Lower ->
                        lowerRow

                    Numbers ->
                        numbers
            )
            mode


setToRandomCharacter: Mode -> Cmd Msg
setToRandomCharacter mode =
    let
        characters =
            characterList mode

        length =
            List.length characters
    in
    Random.generate
        (NewChar << Maybe.withDefault 'a' << (\i -> elementAt i characters))
        (Random.int
            0
            (length - 1)
        )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ Middle ] 1000 100 'A' 0 0 0 (millisToPosix 0) Nothing Neutral False 0, readTime LastUpdateTime )


timeDifference : Time.Posix -> Time.Posix -> Int
timeDifference s t =
    Time.posixToMillis s - Time.posixToMillis t


timedChange : Int -> Time.Posix -> Time.Posix -> Msg
timedChange factor last now =
    if timeDifference now last > factor * 100 then
        TimedChange

    else
        Pass


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let initMessage = case msg of
                        ToggleRunning -> True
                        InduceRunning -> True
                        CountDown _-> True
                        StartTime _ -> True
                        _ -> False
    in
    if not (model.running || initMessage)
    then
        ( model, Cmd.none )
    else
        case msg of
            CorrectKeyPressed ->
                if model.correctnessState == Correct then
                    ( model, Cmd.none )

                else
                    ( { model | successCount = model.successCount + 1, correctnessState = Correct }, Cmd.none )

            IncorrectKeyPressed ->
                ( { model
                    | correctnessState = Incorrect
                    , mistakeCount = model.mistakeCount + 1
                  }
                , Cmd.none
                )

            ResetCorrectnessMarking ->
                if model.correctnessState == Correct then
                    update Change model

                else
                    ( model, Cmd.none )

            Change ->
                ( { model | correctnessState = Neutral }
                , --Reset the correctness state for the new character
                  Cmd.batch
                    [ setToRandomCharacter model.mode
                    , -- set the character to some new randomised character and get the time of change
                      readTime LastUpdateTime
                    ]
                )

            TimedChange ->
                update Change { model | missedCount = model.missedCount + 1 }

            -- automatically update the character if it wasn't typed in in time
            SetMode mode ->
                ( resetCounters { model | mode = mode }, Cmd.none )

            InduceRunning ->
                ( model , readTime StartTime )

            ToggleRunning ->
                ( { model | running = not model.running, startTime = Nothing }, Cmd.none )

            -- Bureaucratic Messages
            CountDown c ->
                ( { model | countDown = c}, Cmd.none)
            NewChar c ->
                ( { model | character = c }, Cmd.none )

            LastUpdateTime time ->
                ( { model | lastUpdateTime = time }, Cmd.none )

            StartTime time ->
                ( { model | startTime = Just time }, Cmd.none )

            Pass ->
                ( model, Cmd.none )



resetCounters : Model -> Model
resetCounters model =
    { model
        | successCount = 0
        , mistakeCount = 0
        , missedCount = 0
    }


startLatency : Int
startLatency =
    2000


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subIntervalNumber =
            model.interval // model.subIntervalLength
    in
    Sub.batch
        [ Time.every (toFloat model.subIntervalLength) (timedChange subIntervalNumber model.lastUpdateTime)
        , Time.every 10
            (\now -> 
                case model.startTime of
                Just startTime -> 
                    let difference = timeDifference now startTime 
                    in
                    if difference > startLatency && not model.running then
                        ToggleRunning
                    else
                        CountDown <| (10 * (startLatency - difference)) // startLatency
                _ -> Pass
                )
        , Time.every 100 (\_ -> ResetCorrectnessMarking)
        , onKeyPress (Json.Decode.map (checkCharacterCorrectness model << toMaybeChar) <| field "key" string)
        ]


toMaybeChar : String -> Maybe Char
toMaybeChar string =
    case String.uncons string of
        Just ( c, "" ) ->
            Just c

        _ ->
            Nothing


checkCharacterCorrectness : Model -> Maybe Char -> Msg
checkCharacterCorrectness model maybeC =
    if maybeC == Just model.character then
        CorrectKeyPressed

    else
        IncorrectKeyPressed


readTime : (Time.Posix -> Msg) -> Cmd Msg
readTime makeMsg =
    perform makeMsg Time.now


coloring : CorrectnessState -> List (Attribute msg)
coloring correctnessState =
    let
        maybeColor =
            case correctnessState of
                Correct ->
                    Just "green"

                Incorrect ->
                    Just "red"

                Neutral ->
                    Nothing
    in
    Maybe.withDefault [] <| Maybe.map (\color -> [ style "color" color ]) maybeColor


view : Model -> Html Msg
view model =
    div []
        [ text ("Fehler: " ++ String.fromInt model.mistakeCount)
        , br [] []
        , text ("Richtig: " ++ String.fromInt model.successCount)
        , br [] []
        , text ("Verpaßt: " ++ String.fromInt model.missedCount)
        , h1 (coloring model.correctnessState) [ text (String.fromChar model.character) ]
        , optionsForm model
        , text ("Countdown: " ++ String.fromInt model.countDown)
        ]


optionsForm : Model -> Html Msg
optionsForm model =
    div []
        [ radioButton "middle" "middle"
        , radioButton "upper" "upper"
        , radioButton "middle-upper" "middle-upper"
        , radioButton "middle-upper-lower" "middle-upper-lower"
        , radioButton "middle-upper-lower-numbers" "middle-upper-lower-numbers"
        , button
            [ onClick
                (if model.running then
                    ToggleRunning

                 else
                    InduceRunning
                )
            ]
            [ text "Start" ]
        ]


elementAt : Int -> List a -> Maybe a
elementAt index list =
    case list of
        x :: xs ->
            if index == 0 then
                Just x

            else
                elementAt (index - 1) xs

        [] ->
            Nothing


readMode : String -> Mode
readMode s =
    case s of
        "middle" ->
            [ Middle ]

        "middle-upper" ->
            [ Middle, Upper ]

        "middle-upper-lower" ->
            [ Middle, Upper, Lower ]

        "middle-upper-lower-numbers" ->
            [ Middle, Upper, Lower, Numbers ]

        _ ->
            [ Middle ]



--View Elements


radioButton : String -> String -> Html Msg
radioButton inputId inputValue =
    input [ type_ "radio", id inputId, value inputValue, onInput (SetMode << readMode), name "mode" ] []
