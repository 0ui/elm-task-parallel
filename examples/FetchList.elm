module FetchList exposing (main)

{- This example fetches a list of HTTP tasks. 
-}

import Api exposing (Post, Comment)
import Browser
import Html exposing (Html, div, text, h1, p, ul, li)
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
  = Loading (Parallel.ListState Msg Post)
  | FailedToLoad String
  | PageReady (List Post)


init : () -> (Model, Cmd Msg)
init _ =
    let
        ( loadingState, fetchCmd ) =
            Parallel.attemptList
              { tasks =
                [ Api.fetchPostById 1
                , Api.fetchPostById 2
                , Api.fetchPostById 42
                , Api.fetchPostById 4
                , Api.fetchPostById 5
                , Api.fetchPostById 12
                ]
              , onUpdates = DownloadUpdated
              , onFailure = DownloadFailed
              , onSuccess = DownloadFinished
              }
    in
    ( Loading loadingState, fetchCmd )


type Msg
    = DownloadUpdated (Parallel.ListMsg Post)
    | DownloadFailed Http.Error
    | DownloadFinished (List Post)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Loading downloadState ->
            case msg of
                DownloadUpdated downloadMsg ->
                  Parallel.updateList downloadState downloadMsg
                    |> Tuple.mapFirst Loading

                DownloadFailed err ->
                  ( FailedToLoad <| Api.httpErrorString <| err, Cmd.none )

                DownloadFinished posts ->
                    ( PageReady posts, Cmd.none )
        _ ->
          ( model, Cmd.none )

view : Model -> Html Msg
view model =
  case model of
    Loading _ ->
      text "Loading data..."

    FailedToLoad err ->
      text <| "Failed to load: " ++ err

    PageReady posts ->
      div []
        [ h1 [] [ text "Posts" ]
        , ul []
            (posts
                |> List.map(\post ->
                    li []
                        [ p [] [ text post.title ]
                        , p [] [ text post.body ]
                        , p [] [ text <| String.fromInt <| post.id ]
                        ]
                )
            )
        ]

