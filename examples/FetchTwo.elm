module FetchTwo exposing (main)

{- This example makes two HTTP requests before it can render the page content. -}

import Api exposing (Comment, Post)
import Browser
import Html exposing (Html, div, h1, li, p, text, ul)
import Http
import Task.Parallel as Parallel


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Model
    = Loading (Parallel.State2 Model Msg Http.Error Post (List Comment))
    | FailedToLoad String
    | PageReady Post (List Comment)


init : () -> ( Model, Cmd Msg )
init _ =
    Parallel.attempt2
        { task1 = Api.fetchPost
        , task2 = Api.fetchComments
        , onFailure = DownloadFailed
        , onSuccess = Parallel.Success2 DownloadFinished
        , onUpdates = DownloadUpdated
        , toModel = Loading
        }

type Msg
    = DownloadUpdated (Parallel.Msg2 Http.Error Post (List Comment))
    | DownloadFailed Http.Error
    | DownloadFinished Post (List Comment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading task ->
            case msg of
                DownloadUpdated taskMsg ->
                    Parallel.update2 taskMsg task

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
