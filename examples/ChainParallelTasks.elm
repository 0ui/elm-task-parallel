module ChainParallelTasks exposing (main)

{-  This example shows you how to chain sets of parallel tasks using
    Task.Parallel.Success[n]AndThen. Here we...

    1) Make five parallel http requests for posts and then
    2) Take the two posts with the highest user ids
    3) Make two more requests for the comments of those posts
    4) Take the number of results (which should always be 2)
    4) Make four more requests for photos based arbitrarily on that number
    
    Achieving the following sequence:

       5 parallel tasks -> 2 parallel tasks -> 4 parallel tasks

    You could use the normal Success[n] message from the other examples and
    chain the next set of tasks from your update function, but instead by using
    Success[n]AndThen, it can all be preconfigured without an intermediate
    success message. Only one update handler message is required per set of
    additional tasks.

    NOTES:

    * The attempt5 and attempt4 calls in this example use the same types
      for all of the results but that's just for simplicity. You can mix and
      match the result types and simply change the types in your State[n] to
      match those.

    * This example takes a potential 21 cases (normally required for 11
      tasks) and 3 completion checks (for three parallel sets) and reduces that
      down to only 5 cases.

-}

import Api exposing (Comment, Photo, Post)
import Browser
import Html exposing (Html, div, h1, img, li, p, text, ul)
import Html.Attributes exposing (src)
import Http
import Task exposing (Task)
import Task.Parallel as Parallel


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


type Model
    = LoadingPosts (Parallel.State5 Model Msg Http.Error Post Post Post Post Post)
    | LoadingComments (Parallel.StateList Model Msg Http.Error (List Comment))
    | LoadingPhotos (Parallel.State4 Model Msg Http.Error Photo Photo Photo Photo)
    | FailedToLoad String
    | PageReady Photo Photo Photo Photo


init : () -> ( Model, Cmd Msg )
init _ =
    Parallel.attempt5
        { task1 = Api.fetchPostById 10
        , task2 = Api.fetchPostById 20
        , task3 = Api.fetchPostById 30
        , task4 = Api.fetchPostById 40
        , task5 = Api.fetchPostById 50
        , onFailure = RequestFailed
        , onSuccess = Parallel.Success5AndThen downloadTwoCommentLists
        , onUpdates = PostsUpdated
        , updateModel = Parallel.UpdateComplete LoadingPosts
        }


downloadTwoCommentLists : Post -> Post -> Post -> Post -> Post -> ( Model, Cmd Msg )
downloadTwoCommentLists post1 post2 post3 post4 post5 =
    Parallel.attemptList
        { tasks =
            [ post1, post2, post3, post4, post5 ]
                |> List.sortBy .userId
                |> List.reverse
                |> List.take 2
                |> List.map (.id >> Api.fetchCommentsById)
        , onFailure = RequestFailed
        , onSuccess = Parallel.SuccessListAndThen downloadFourPhotos
        , onUpdates = CommentsUpdated
        , updateModel = Parallel.UpdateComplete LoadingComments
        }


downloadFourPhotos : List (List Comment) -> ( Model, Cmd Msg )
downloadFourPhotos comments =
    Parallel.attempt4
        { task1 = Api.fetchPhotoById (1 + List.length comments)
        , task2 = Api.fetchPhotoById (2 + List.length comments)
        , task3 = Api.fetchPhotoById (3 + List.length comments)
        , task4 = Api.fetchPhotoById (4 + List.length comments)
        , onUpdates = PhotosUpdated
        , onFailure = RequestFailed
        , onSuccess = Parallel.Success4 GotPhotos
        , updateModel = Parallel.UpdateComplete LoadingPhotos
        }


type Msg
    = PostsUpdated (Parallel.Msg5 Http.Error Post Post Post Post Post)
    | CommentsUpdated (Parallel.MsgList Http.Error (List Comment))
    | PhotosUpdated (Parallel.Msg4 Http.Error Photo Photo Photo Photo)
    | GotPhotos Photo Photo Photo Photo
    | RequestFailed Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( PostsUpdated taskMsg, LoadingPosts task ) ->
            Parallel.update5 taskMsg task model

        ( CommentsUpdated taskMsg, LoadingComments task ) ->
            Parallel.updateList taskMsg task model

        ( PhotosUpdated taskMsg, LoadingPhotos task ) ->
            Parallel.update4 taskMsg task model

        ( GotPhotos photo1 photo2 photo3 photo4, _ ) ->
            ( PageReady photo1 photo2 photo3 photo4, Cmd.none )

        ( RequestFailed err, _ ) ->
            ( FailedToLoad <| Api.httpErrorString <| err, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        LoadingPosts _ ->
            text "Loading posts..."

        LoadingComments _ ->
            text "Loading comments..."

        LoadingPhotos _ ->
            text "Loading photos..."

        FailedToLoad err ->
            text <| "Failed to load: " ++ err

        PageReady photo1 photo2 photo3 photo4 ->
            div []
                [ h1 [] [ text "Photos" ]
                , ul []
                    [ li [] [ img [ src photo1.url ] [] ]
                    , li [] [ img [ src photo2.url ] [] ]
                    , li [] [ img [ src photo3.url ] [] ]
                    , li [] [ img [ src photo4.url ] [] ]
                    ]
                ]


topTwo : List Post -> List Post
topTwo =
    List.sortBy .userId
        >> List.reverse
        >> List.take 2