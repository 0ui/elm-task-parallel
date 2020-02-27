module FetchAndMap exposing (main)

{- This example makes two HTTP requests and maps them to a single value
   before sending the finished message. -}

import Api exposing (Comment, Post)
import Browser
import Html exposing (Html, div, h1, li, p, text, ul)
import Http
import Task.Parallel exposing (State2, Msg2, attemptMap2, mapState, task2, update2)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Model
    = Loading (State2 Post (List Comment))
    | FailedToLoad String
    | PageReady Int


init : () -> ( Model, Cmd Msg )
init _ =
    task2 DownloadUpdated Api.fetchPost Api.fetchComments
        |> attemptMap2
            (\post comments -> String.length post.title + List.length comments)
            DownloadFinished
            DownloadFailed
        |> mapState Loading


type Msg
    = DownloadUpdated (Msg2 Http.Error Post (List Comment) Msg)
    | DownloadFailed Http.Error
    | DownloadFinished Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading task2 ->
            case msg of
                DownloadUpdated taskMsg ->
                    update2 taskMsg task2
                        |> mapState Loading

                DownloadFailed err ->
                    ( FailedToLoad <| Api.httpErrorString <| err, Cmd.none )

                DownloadFinished result ->
                    ( PageReady result, Cmd.none )

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
