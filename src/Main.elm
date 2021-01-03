module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, Attribute, button, div, h1, text, br)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Time
import List exposing (head)
import Json.Decode exposing (field, string, map)
import Task exposing (perform)
import Time exposing (millisToPosix)

main = Browser.element {
    init = init, 
    update = update, 
    subscriptions = subscriptions, 
    view = view}

type Msg = 
      Change 
    | CorrectKeyPressed
    | IncorrectKeyPressed 
    | ResetCorrectnessMarking
    | NewChar Char 
    | SetLastUpdateTime Time.Posix 
    | Pass
    | TimedChange

type alias Model = 
    {
        mode: List(CharacterGroup),
        interval: Int,
        subIntervalLength: Int,
        character: Char,
        mistakeCount: Int,
        successCount: Int,
        missedCount: Int,
        lastUpdateTime: Time.Posix,
        correctnessState: CorrectnessState
        }

type CharacterGroup = Upper | Middle | Lower | Numbers
type alias Mode = List(CharacterGroup)
type CorrectnessState = Correct | Incorrect | Neutral

letters: List(Char)
letters = 
    List.map 
    Char.fromCode <| 
    List.range (Char.toCode 'a') (Char.toCode 'z')

lowerRow: List(Char)
lowerRow = ['y', 'x', 'v', 'b', 'n', 'm']

middleRow: List(Char)
middleRow = ['a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l']

middleRowUmlauts: List(Char)
middleRowUmlauts = ['ö', 'ä']

upperRow: List(Char)
upperRow = ['q', 'w', 'e', 'r', 't', 'z', 'u', 'i', 'o', 'p']

numbers: List(Char)
numbers = ['0','1','2','3','4','5','6','7','8','9']

upperRowUmlauts: List(Char)
upperRowUmlauts = ['ü']

umlauts: List(Char)
umlauts = ['ä','ö','ü']

capital: List(Char) -> List(Char)
capital = List.map Char.toUpper 

characterList: Mode -> List(Char)
characterList mode =
    List.foldl 
    (\group acc ->
        case group of
            Upper -> List.append upperRow upperRowUmlauts
            Middle -> List.append middleRow middleRowUmlauts
            Lower -> lowerRow
            Numbers -> numbers)
    []
    mode

--setToRandomCharacter: Mode -> 
setToRandomCharacter mode =
    let characters = characterList mode
        length = List.length characters
    in
        Random.generate 
               (NewChar << Maybe.withDefault 'a' << (\i -> elementAt i characters)) 
               (Random.int 
               0 
               (length - 1))

init : () -> (Model, Cmd Msg)
init _ = (Model [Middle] 1000 100 'A' 0 0 0 (millisToPosix 0) Neutral, readTime)

timedChange : Int -> Time.Posix -> Time.Posix -> Msg
timedChange factor last now = 
    if Time.posixToMillis last < Time.posixToMillis now - (factor * 100)  
    then 
        TimedChange 
    else
        Pass

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        CorrectKeyPressed -> 
            if model.correctnessState == Correct 
                then 
                    (model, Cmd.none)
                else 
                    ({model | successCount = model.successCount + 1, correctnessState = Correct}, Cmd.none)
        IncorrectKeyPressed -> 
            ({model | 
                correctnessState = Incorrect, 
                mistakeCount = model.mistakeCount + 1 }, 
                Cmd.none
                )
        ResetCorrectnessMarking -> if model.correctnessState == Correct 
         then  update Change model
         else (model, Cmd.none)
        Change ->
            (
                {model | correctnessState = Neutral}, 
                Cmd.batch [ 
                    setToRandomCharacter model.mode,
                    readTime]
                    )
        TimedChange -> 
            update Change {model | missedCount = model.missedCount + 1}
        SetLastUpdateTime time -> ({model | lastUpdateTime = time}, Cmd.none)
        NewChar c -> ({model | character = c}, Cmd.none)
        Pass -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
    let subIntervalNumber = model.interval // model.subIntervalLength
    in
    Time.every (toFloat model.subIntervalLength) (timedChange subIntervalNumber model.lastUpdateTime)
    ,
    Time.every 100 (\_ -> ResetCorrectnessMarking),
    onKeyPress (map (checkCharacterCorrectness model << toMaybeChar) <| field "key" string)
    ]

toMaybeChar: String -> Maybe Char
toMaybeChar string = 
    case String.uncons string of
        Just (c, "") -> Just c
        _ -> Nothing 

checkCharacterCorrectness: Model -> Maybe Char -> Msg
checkCharacterCorrectness model maybeC =
    if(maybeC == Just model.character) 
    then
        CorrectKeyPressed
    else
        IncorrectKeyPressed

readTime: Cmd Msg
readTime = perform SetLastUpdateTime Time.now 

coloring: CorrectnessState -> List(Attribute msg)
coloring correctnessState = 
    let maybeColor = 
            case correctnessState of
                Correct -> Just "green"
                Incorrect -> Just "red"
                Neutral -> Nothing
    in
        Maybe.withDefault [] <| Maybe.map (\color -> [style "color" color]) maybeColor

view: Model -> Html Msg
view model = 
    div []
    [ 
        text ("Fehler: " ++ (String.fromInt model.mistakeCount)),
        br [] [], 
        text ("Richtig: " ++ (String.fromInt model.successCount)),
        br [] [], 
        text ("Verpaßt: " ++ (String.fromInt model.missedCount)),
        h1 (coloring model.correctnessState) [ text  (String.fromChar model.character ) ] ]

elementAt: Int -> List(a) -> Maybe a
elementAt index list =
    case list of
        x::xs -> if index == 0 then Just x else elementAt (index-1) xs 
        [] -> Nothing
        
