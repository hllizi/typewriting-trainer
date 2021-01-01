module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Random
import Time
import List exposing (head)
import Json.Decode exposing (field, string, map)

main = Browser.element {
    init = init, 
    update = update, 
    subscriptions = subscriptions, 
    view = view}

type Msg = Change | CorrectKeyPressed Char | IncorrectKeyPressed | NewChar Char
type alias Model = 
    {
        character: Char,
        mistakeMade: Bool
        }

init : () -> (Model, Cmd Msg)
init _ = (Model 'A' False, Cmd.none)

timedChange : Time.Posix -> Msg
timedChange _ = Change

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        CorrectKeyPressed _ -> ({model | character = 'Ö'}, Cmd.none) 
        IncorrectKeyPressed -> ({model | mistakeMade = True }, Cmd.none)
        Change ->
                    (
                        model, 
                        Random.generate 
                        (NewChar << Char.fromCode) 
                        (Random.int 
                        (Char.toCode 'A') 
                        (Char.toCode 'z'))
                        )
        NewChar c -> ({model | character = c}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [
    Time.every 1000 timedChange,
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
            


view: Model -> Html Msg
view model = 
    div []
    [ h1 [] [ text  (String.fromChar model.character ) ] ]



