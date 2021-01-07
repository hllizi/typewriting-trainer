module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Ev
import Element.Font as Font
import Element.Input as Input
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
    { mode : Mode
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
    , countDown : Int
    }


type CharacterGroup
    = UpperLetters
    | MiddleLetters
    | LowerLetters
    | Numbers


type alias CharacterGroups =
    List CharacterGroup


type Mode
    = Middle
    | MiddleUpper
    | UpperToLower
    | LettersAndNumbers


type alias ModeData =
    { label : String
    , id : String
    , characters : List Char
    }


modeToModeData : Mode -> ModeData
modeToModeData mode =
    case mode of
        Middle ->
            { characters = middleRow, id = "middle", label = "middle" }

        MiddleUpper ->
            { characters = List.concat [ middleRow, upperRow ], id = "middle-upper", label = "middle-upper" }

        UpperToLower ->
            { characters = List.concat [ middleRow, upperRow, lowerRow ], id = "middle-upper-lower", label = "middle-upper-lower" }

        LettersAndNumbers ->
            { characters = List.concat [ middleRow, upperRow, lowerRow, numbers ], id = "middle-upper-lower-numbers", label = "middle-upper-lower-numbers" }


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


characterList : CharacterGroups -> List Char
characterList mode =
    List.concat <|
        List.map
            (\group ->
                case group of
                    UpperLetters ->
                        List.append upperRow upperRowUmlauts

                    MiddleLetters ->
                        List.append middleRow middleRowUmlauts

                    LowerLetters ->
                        lowerRow

                    Numbers ->
                        numbers
            )
            mode


setToRandomCharacter : Mode -> Cmd Msg
setToRandomCharacter mode =
    let
        characters =
            (modeToModeData mode).characters

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
    ( Model Middle 1000 100 'A' 0 0 0 (millisToPosix 0) Nothing Neutral False 0, readTime LastUpdateTime )


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
    let
        initMessage =
            case msg of
                ToggleRunning ->
                    True

                InduceRunning ->
                    True

                CountDown _ ->
                    True

                StartTime _ ->
                    True

                SetMode _ ->
                    True

                _ ->
                    False
    in
    if not (model.running || initMessage) then
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
                ( resetCounters { model | running = False, startTime = Nothing, mode = mode }, Cmd.none )

            InduceRunning ->
                ( model, readTime StartTime )

            ToggleRunning ->
                ( { model | running = not model.running, startTime = Nothing }, Cmd.none )

            -- Bureaucratic Messages
            CountDown c ->
                ( { model | countDown = c }, Cmd.none )

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
    3000


countDownFrom : Int
countDownFrom =
    3


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
                        let
                            difference =
                                timeDifference now startTime
                        in
                        if difference > startLatency && not model.running then
                            ToggleRunning

                        else
                            CountDown <| (countDownFrom + 1) * (startLatency - difference) // startLatency

                    _ ->
                        Pass
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


coloring : CorrectnessState -> List (Attr decorative msg)
coloring correctnessState =
    List.map Font.color
        (case correctnessState of
            Correct ->
                [ rgb255 0 255 0 ]

            Incorrect ->
                [ rgb255 255 0 0 ]

            Neutral ->
                []
        )


readMode : String -> CharacterGroups
readMode s =
    case s of
        "middle" ->
            [ MiddleLetters ]

        "middle-upper" ->
            [ MiddleLetters, UpperLetters ]

        "middle-upper-lower" ->
            [ MiddleLetters, UpperLetters, LowerLetters ]

        "middle-upper-lower-numbers" ->
            [ MiddleLetters, UpperLetters, LowerLetters, Numbers ]

        _ ->
            [ MiddleLetters ]



--VIEW ELEMENTS


counter : String -> Int -> Element Msg
counter name count =
    el [] <|
        Element.text <|
            name
                ++ String.fromInt count


currentCharacter : Color -> Char -> Element Msg
currentCharacter color char =
    el [ Font.color color ] <|
        Element.text <|
            String.fromChar char

textColorMain = (rgb255 200 200 200)

counterDisplay: String -> Int -> Element Msg
counterDisplay text counterValue = el 
    [Font.color textColorMain] <|
    Element.text (text ++ ": " ++ String.fromInt counterValue)

view : Model -> Html Msg
view model =
    layout [ Element.height fill] <|
        row [Element.height fill, Element.width fill]
            [ column [Element.height fill, Element.width <| fillPortion 20, Background.color (rgb255 0 0 100)]
                [ row [Element.width fill]
                    [ row [ Element.width fill ]  [counterDisplay "Fehler" model.mistakeCount]
                    , row [ Element.width fill ]  [counterDisplay "Richtig" model.successCount]
                    , row [ Element.width fill ]  [counterDisplay "Verpaßt" model.missedCount]
                    ]
                , row [Element.width fill, Element.height fill]
                    [ el
                        (List.append
                            [ centerX, centerY, Font.size 200, Font.color (rgb 200 230 10) ]
                            (coloring model.correctnessState)
                        )
                        (Element.text (String.fromChar model.character))
                    ]
                , el [Font.color textColorMain, centerX] <| Element.text ("Countdown: " ++ String.fromInt model.countDown)
                ],
              column [Element.width <| (fillPortion 3 |> maximum 400), Element.height fill, Background.color (rgb255 0 255 0)]
                [el [centerY] <| 
                    optionsForm model]
            ]


modeSelectButtons : Model -> Element Msg
modeSelectButtons model =
    el [] <|
        Input.radio
            []
            { onChange = SetMode
            , selected = Just model.mode
            , label = Input.labelAbove [] (Element.text "Modus wählen:")
            , options =
                List.map (\mode -> Input.option mode (Element.text (modeToModeData mode).id))
                    [ Middle, MiddleUpper, UpperToLower, LettersAndNumbers ]
            }


optionsForm : Model -> Element Msg
optionsForm model =
    column []
        [ modeSelectButtons model
        , Input.button []
            { onPress =
                Just
                    (if model.running then
                        ToggleRunning

                     else
                        InduceRunning
                    )
            , label = el [centerX] <|
                Element.text <|
                    if model.running then
                        "Stop"

                    else
                        "Start"
            }
        ]



-- HELPERS


extensionallyEqual : List a -> List a -> Bool
extensionallyEqual l1 l2 =
    case l1 of
        [] ->
            l2 == []

        x :: xs ->
            let
                notEqualX =
                    \y -> x /= y
            in
            l2
                /= []
                && extensionallyEqual
                    (List.filter notEqualX xs)
                    (List.filter notEqualX l2)


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
