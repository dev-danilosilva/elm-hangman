module Main exposing (..)
import Bitwise
import Html exposing (Html, div, h1, h3, span, text)
import Browser
import Html.Attributes exposing (class)
import Html exposing (button)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Http
import Json.Decode as Decode
import Json.Decode exposing (Decoder)

--{ pastGuesses = Set.empty
--      , phrase = String.toUpper ""
--      , attempts = 0
--      , maxTries = 10
--      , requestState = Loading
--    }

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = Failure
    | Loading
    | Success State
    | Victory
    | Defeat

type alias State =
    { pastGuesses : Set String
    , word : String
    , attempts : Int
    , maxTries : Int
    }


type Msg
    = NewGuess String
    | Reset
    | NewWord (Result Http.Error String)
    | PlayerWins
    | PlayerDefeated

wordEndpoint : String
wordEndpoint =
    "http://127.0.0.1:5000/api/word"


wordDecoder : Decoder String
wordDecoder =
    Decode.field "word" Decode.string

requestWord : Cmd Msg
requestWord =
    Http.get
        { url = wordEndpoint
        , expect = Http.expectJson NewWord  wordDecoder
        }



init : (Model, Cmd Msg)
init =
    ( Loading , requestWord )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewGuess guess ->
            case model of
                Success state ->
                    ( Success
                        { state
                            | pastGuesses = Set.insert guess state.pastGuesses
                            , attempts = state.attempts + 1
                        }, Cmd.none
                    )
                _ -> (Failure, Cmd.none)

        NewWord response ->
            case response of
                Ok word -> (Success
                                { pastGuesses = Set.empty
                                , word = word |> String.toUpper
                                , attempts = 0
                                , maxTries = 10}, Cmd.none)

                Err err -> (Failure, Cmd.none)

        Reset ->
            init

        PlayerWins ->
            (model, Cmd.none)

        PlayerDefeated ->
            (model, Cmd.none)



view : Model -> Html Msg
view model =
    let
        asList =
            String.split ""

        asSet str =
            str
             |> asList
             |> Set.fromList


        wordElement state =
            state.word
                |> asList
                |> List.map (\char ->
                    case char of

                    " " -> " "

                    _   ->
                        if Set.member char state.pastGuesses then
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

        attemptsElement state =
            state.pastGuesses
                |> Set.toList
                |> List.filter
                    (\char -> not <| Set.member char (state.word |> asSet))
                |> List.map
                    (\attempt ->
                        span [class "past-attempt"] [text attempt]
                    )
                |> div [class "past-attempts"]

        resetButton buttonLabel =
            div [class "reset-button", onClick Reset] [text buttonLabel]

    in
        case model of
            Loading ->
                div [class "loading-page"]
                    [ h3 [] [text "Loading..."]
                    ]
            Failure ->
                div [ class "error-page"]
                    [ h3 [] [text "Error"]
                    , resetButton "Reload"
                    ]
            Success state ->
                div []
                    [ wordElement state
                    , attemptsElement state
                    , keyboardElement
                    , resetButton "Reset"
                    ]

            Victory ->
                div [class "victory-page"]
                    [ h3 [] [text "You Win!!"]
                    , resetButton "New"
                    ]


            Defeat ->
                div [class "defeat-page"]
                    [ h3 [] [text "You Lose!!"]
                    , resetButton "New"
                    ]

