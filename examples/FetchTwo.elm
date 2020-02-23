module FetchTwo exposing (main)

{- This example makes two HTTP requests before it can render the page content.
 -}

import Api exposing (Comment, Post)
import Browser
import Html exposing (Html, div, h1, li, p, text, ul)
import Http
import Task.Parallel


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Model
    = Loading (Task.Parallel.State2 Post (List Comment))
    | FailedToLoad String
    | PageReady Post (List Comment)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( loadingState, fetchCmd ) =
            Task.Parallel.attempt2 DownloadUpdated Api.fetchPost Api.fetchComments
    in
    ( Loading loadingState, fetchCmd )


type Msg
    = DownloadUpdated (Task.Parallel.Msg2 Http.Error Post (List Comment))
    | DownloadFailed Http.Error
    | DownloadFinished Post (List Comment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading downloadState ->
            case msg of
                DownloadUpdated downloadMsg ->
                    let
                        ( nextState, nextCmd ) =
                            Task.Parallel.update2 downloadState downloadMsg DownloadFinished DownloadFailed
                    in
                    ( Loading nextState, nextCmd )

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
                [ h1 [] [ text post.title ]
                , p [] [ text post.body ]
                , ul []
                    (comments
                        |> List.map
                            (\comment ->
                                li []
                                    [ p [] [ text comment.name ]
                                    , p [] [ text comment.body ]
                                    ]
                            )
                    )
                ]
