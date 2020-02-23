module FetchTwo exposing (main)

{- This example makes two HTTP requests before it can render the page content.
Notice the type annotations have () in the empty "slots" because only two of the
possible five tasks are being used. You can ignore these and 
-}

import Api exposing (Post, Comment)
import Browser
import Html exposing (Html, div, text, h1, p, ul, li)
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
  = Loading (Task.Parallel.State Post (List Comment) () () ())
  | FailedToLoad String
  | PageReady Post (List Comment)


init : () -> (Model, Cmd Msg)
init _ =
    let
        ( loadingState, fetchCmd ) =
            Task.Parallel.attempt2 DownloadUpdated Api.fetchPost Api.fetchComments
    in
    ( Loading loadingState, fetchCmd )


type Msg
    = DownloadUpdated (Task.Parallel.Msg Http.Error Post (List Comment) () () ())
    | DownloadFailed Http.Error
    | DownloadFinished Post (List Comment)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Loading downloadState ->
            case msg of
                DownloadUpdated downloadMsg ->
                    let
                        ( nextState, nextCmd ) =
                            Task.Parallel.update downloadState downloadMsg (Task.Parallel.Expect2 DownloadFinished) DownloadFailed
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
                |> List.map(\comment ->
                    li []
                        [ p [] [ text comment.name ]
                        , p [] [ text comment.body ]
                        ]
                )
            )
        ]

