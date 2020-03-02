module FetchAndMap exposing (main)

{- This example makes two HTTP requests and maps them to a single value
   before sending the finished message. -}

import Api exposing (Comment, Post)
import Browser
import Html exposing (Html, div, h1, li, p, text, ul)
import Http
import Task.Parallel exposing (State3, Msg3, attempt3Map, mapState, Task3(..), update3)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Model
    = Loading (State3 Post Post Post)
    | FailedToLoad String
    | PageReady Int

numCharacters : Post -> Post -> Post -> Int
numCharacters post1 post2 post3 =
   String.length post1.title + String.length post2.title + String.length post3.title

init : () -> ( Model, Cmd Msg )
init _ =
    Task3 (Api.fetchPostById 1) (Api.fetchPostById 2) (Api.fetchPostById 3)
        |> attempt3Map numCharacters RequestUpdated NumberCalculated RequestFailed
        |> mapState Loading


type Msg
    = RequestUpdated (Msg3 Msg Http.Error Post Post Post)
    | NumberCalculated Int
    | RequestFailed Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading task ->
            case msg of
                RequestUpdated taskMsg ->
                    update3 taskMsg task
                        |> mapState Loading

                NumberCalculated result ->
                    ( PageReady result, Cmd.none )

                RequestFailed err ->
                    ( FailedToLoad <| Api.httpErrorString <| err, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            text "Loading data..."

        FailedToLoad err ->
            text <| "Failed to load: " ++ err

        PageReady result ->
            div [] [ h1 [] [ text ("Calculated result: " ++ String.fromInt result) ] ]
