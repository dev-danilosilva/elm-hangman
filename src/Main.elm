module Main exposing (..)

import Browser
import Html exposing (Html, div, h3, span, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Set exposing (Set)
import Http
import Json.Decode as Decode
import Json.Decode exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

and : Bool -> Bool -> Bool
and a b =
    case a of
        True ->
            if b == True then
                True
            else
                False
        False -> False

type Model
    = Failure
    | Loading
    | Success State
    | Victory State
    | Defeat State

type alias State =
    { pastGuesses : Set String
    , word : String
    , attempts : Int
    , maxErrors : Int
    }


type Msg
    = NewGuess String
    | Reset
    | NewWord (Result Http.Error String)

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
                    let
                        wordGuessIntersect =
                            state.word
                                |> String.split ""
                                |> Set.fromList
                                |> (\solutionSet ->
                                    let
                                        newState = { state
                                                     | pastGuesses = Set.insert guess state.pastGuesses
                                                     , attempts = state.attempts + 1
                                                 }
                                        intersect = Debug.log "Intersect: " (Set.intersect solutionSet newState.pastGuesses)
                                        intersectSize = Set.size intersect
                                        solutionSize = Set.size solutionSet
                                        minIntersectSize = Debug.log "Solution Size" solutionSize  == Debug.log "Intersect Size" intersectSize
                                    in
                                        (minIntersectSize, newState)
                                )
                                |> (\(res, newState) ->
                                        if res == True then
                                            Victory newState
                                        else
                                            Success newState)
                    in
                        (wordGuessIntersect, Cmd.none)
                _ -> (Failure, Cmd.none)

        NewWord response ->
            case response of
                Ok word -> (Success
                                { pastGuesses = Set.empty
                                , word = word |> String.toUpper
                                , attempts = 0
                                , maxErrors = 10}, Cmd.none)

                Err err -> (Failure, Cmd.none)

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

            Victory state ->
                div [class "victory-page"]
                    [ h3 [] [text "You Win!!", div [] [text <| (++) "Answer: " <| String.toLower state.word]]
                    , resetButton "New"
                    ]


            Defeat state ->
                div [class "defeat-page"]
                    [ h3 [] [text "You Lose!!", div [] [text <| (++) "Answer: " <| String.toLower state.word]]
                    , resetButton "New"
                    ]

