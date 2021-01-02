module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, Attribute, button, div, h1, text)
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

type Msg = Change | CorrectKeyPressed Char | IncorrectKeyPressed | NewChar Char | SetLastUpdateTime Time.Posix | Pass
type alias Model = 
    {
        interval: Int,
        subIntervalLength: Int,
        character: Char,
        mistakeMade: Bool,
        lastUpdateTime: Time.Posix
        }

init : () -> (Model, Cmd Msg)
init _ = (Model 1000 100 'A' False (millisToPosix 0), readTime)

timedChange : Int -> Time.Posix -> Time.Posix -> Msg
timedChange factor last now = 
    if Time.posixToMillis last < Time.posixToMillis now - (factor * 100)  
    then 
        Change
    else
        Pass

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        CorrectKeyPressed _ -> update Change model
        IncorrectKeyPressed -> ({model | mistakeMade = True }, Cmd.none)
        Change ->
                    (
                        {model | mistakeMade = False}, 
                        Cmd.batch [ 
                            Random.generate 
                            (NewChar << Char.fromCode) 
                            (Random.int 
                            (Char.toCode 'A') 
                            (Char.toCode 'z')),
                            readTime]
                            )
        SetLastUpdateTime time -> ({model | lastUpdateTime = time}, Cmd.none)
        NewChar c -> ({model | character = c}, Cmd.none)
        Pass -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
    let subIntervalNumber = model.interval // model.subIntervalLength
    in
     Time.every (toFloat model.subIntervalLength) (timedChange subIntervalNumber model.lastUpdateTime)
     ,
    onKeyPress (map (checkCharacter model << toMaybeChar) <| field "key" string)
    ]

toMaybeChar: String -> Maybe Char
toMaybeChar string = 
    case String.uncons string of
        Just (c, "") -> Just c
        _ -> Nothing 

setCharacter: Maybe Char -> Msg
setCharacter maybeC =
    case maybeC of
        Just c -> CorrectKeyPressed c
        _ -> IncorrectKeyPressed

checkCharacter: Model -> Maybe Char -> Msg
checkCharacter model maybeC =
    if(maybeC == Just model.character) 
    then
        CorrectKeyPressed model.character 
    else
        IncorrectKeyPressed

readTime: Cmd Msg
readTime = perform SetLastUpdateTime Time.now 

coloring: Bool -> List(Attribute msg)
coloring mistakeMade = 
    if mistakeMade 
    then [style "color" "red"]
    else []

view: Model -> Html Msg
view model = 
    div []
    [ h1 (coloring model.mistakeMade) [ text  (String.fromChar model.character ) ] ]



