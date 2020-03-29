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

type Model =
    Model { taskState : Parallel.StateList Model Msg Http.Error Post
    , loadingErr : Maybe Http.Error
    , result : Maybe (List Post)
    }

initialModel : Parallel.StateList Model Msg Http.Error Post -> Model
initialModel taskState =
    Model
        { taskState = taskState
        , loadingErr = Nothing
        , result = Nothing
        }

init : () -> ( Model, Cmd Msg )
init _ =
    Parallel.attemptList
        { tasks =
            [ Api.fetchPostById 1
            , Api.fetchPostById 2
            , Api.fetchPostById 4
            , Api.fetchPostById 5
            , Api.fetchPostById 12
            ]
        , onFailure = DownloadFailed
        , onSuccess = Parallel.SuccessList DownloadFinished
        , onUpdates =  DownloadUpdated 
        , updateModel = Parallel.UpdateModel 
            (\ state (Model model) -> Model { model | taskState = state })
            initialModel
        }


type Msg
    = DownloadUpdated (Parallel.MsgList Http.Error Post)
    | DownloadFailed Http.Error
    | DownloadFinished (List Post)


update : Msg -> Model -> (Model, Cmd Msg)
update msg ((Model model) as originalModel) =
    case msg of
        DownloadUpdated taskMsg ->
            Parallel.updateList taskMsg model.taskState originalModel

        DownloadFailed err ->
            ( Model { model | loadingErr = Just err }, Cmd.none )

        DownloadFinished posts ->
            ( Model { model | result = Just posts }, Cmd.none )

view : Model -> Html Msg
view (Model model) =
    div []
        [ p [] [ text (model.loadingErr |> Maybe.map Api.httpErrorString |> Maybe.withDefault "")]
        , case model.result of
            Nothing ->
              p [] [ text "Loading data..." ]

            Just posts ->
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
        ]

