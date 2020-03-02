module FetchFive exposing (main)

{- This example makes five HTTP requests before it can render the page content.
   It requires essentially the same number of lines as the example with only two
   requests.
-}

import Api exposing (Comment, Photo, Post, Todo)
import Browser
import Html exposing (Html, div, h1, h2, img, li, p, text, ul)
import Html.Attributes exposing (src)
import Http
import Task.Parallel exposing (Msg5, State5, attempt5, mapState, Task5(..), update5)
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Model
    = Loading (State5 Post (List Comment) Time.Posix Photo Todo)
    | FailedToLoad String
    | PageReady Post (List Comment) Time.Posix Photo Todo


init : () -> ( Model, Cmd Msg )
init _ =
    Task5 Api.fetchPost Api.fetchComments Time.now (Api.fetchPhotoById 1) Api.fetchTodo
        |> attempt5 DownloadUpdated DownloadFinished DownloadFailed
        |> mapState Loading -- Alias for Tuple.mapFirst


type Msg
    = DownloadUpdated (Msg5 Msg Http.Error Post (List Comment) Time.Posix Photo Todo)
    | DownloadFailed Http.Error
    | DownloadFinished Post (List Comment) Time.Posix Photo Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Loading taskState ->
            case msg of
                DownloadUpdated taskMsg ->
                    update5 taskMsg taskState
                        |> mapState Loading

                DownloadFailed err ->
                    ( FailedToLoad <| Api.httpErrorString <| err, Cmd.none )

                DownloadFinished post comments time photo todo ->
                    ( PageReady post comments time photo todo, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            text "Loading data..."

        FailedToLoad err ->
            text <| "Failed to load: " ++ err

        PageReady post comments time photo todo ->
            div []
                [ h1 [] [ text post.title ]
                , h2 [] [ text <| "in the year " ++ (time |> Time.toYear Time.utc |> String.fromInt) ]
                , p [] [ text post.body ]
                , p []
                    [ text <|
                        "Todo is "
                            ++ (if todo.completed then
                                    "done"

                                else
                                    "not done"
                               )
                    ]
                , img [ src photo.thumbnailUrl ] []
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
