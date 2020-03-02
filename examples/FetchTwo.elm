module FetchTwo exposing (main)

{- This example makes two HTTP requests before it can render the page content. -}

import Api exposing (Comment, Post)
import Browser
import Html exposing (Html, div, h1, li, p, text, ul)
import Http
import Task.Parallel exposing (State2, Msg2, attempt2, mapState, Task2(..), update2)


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
    | PageReady Post (List Comment)


init : () -> ( Model, Cmd Msg )
init _ =
    Task2 Api.fetchPost Api.fetchComments
        |> attempt2 DownloadUpdated DownloadFinished DownloadFailed
        |> mapState Loading -- Alias for Tuple.mapFirst

type Msg
    = DownloadUpdated (Msg2 Msg Http.Error Post (List Comment))
    | DownloadFailed Http.Error
    | DownloadFinished Post (List Comment)


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

                DownloadFinished post comments ->
                    ( PageReady post comments, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            text "Loading data..."

        FailedToLoad err ->
            text <| "Failed to load: " ++ err

        PageReady post comments ->
            div []
                [ h1 [] [ text <| String.fromInt <| List.length <| comments ]
                , p [] [ text post.title ]
                ]
