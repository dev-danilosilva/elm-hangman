module Main exposing (..)
import Html exposing (h1, div, text, span, Html)
import Browser
import Html.Attributes exposing (class)
import Html exposing (button)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Task exposing (attempt)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model = 
    { lastGuess : String
    , pastGuesses : Set String
    , phrase : String
    , attempts : Int
    , maxTries : Int
    }


type Msg
    = NewGuess String
    | Reset



init : (Model, Cmd Msg)
init = 
    ( { lastGuess  = ""
      , pastGuesses = Set.empty
      , phrase = String.toUpper "paralelepipedo"
      , attempts = 0
      , maxTries = 10
    } , Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewGuess guess ->
            ({ model 
                | lastGuess = guess
                , pastGuesses = Set.insert guess model.pastGuesses
                
                }, Cmd.none)
        Reset ->
            init


view : Model -> Html Msg
view model =
    let
        asList =
            String.split ""
        
        asSet str =
            str
             |> asList
             |> Set.fromList
            

        phraseElement =
            model.phrase
                |> asList
                |> List.map (\char ->
                    case char of

                    " " -> " "

                    _   -> 
                        if Set.member char model.pastGuesses then
                            char
                        else
                            "_"
                )
                |> List.map (\char ->
                    span [] [text char]
                    )
                |> div [class "phraseContainer"]

        keyboardElement =
            "abcdefghijklmnopqrstuvwxyz"
                |> String.split ""
                |> List.map String.toUpper
                |> List.map (\char ->
                    button [ NewGuess char |> onClick ] [text char]
                    )
                |> div [class "keyboard"]
        
        attemptsElement =
            model.pastGuesses
                |> Set.toList
                |> List.filter
                    (\char -> not <| Set.member char (model.phrase |> asSet))
                |> List.map 
                    (\attempt ->
                        span [class "past-attempt"] [text attempt]
                    )
                |> div [class "past-attempts"]

        resetButton =
            div [class "reset-button", onClick Reset] [text "Reset"]

    in
        div []
            [ phraseElement
            , attemptsElement
            , keyboardElement
            , resetButton
            ]
