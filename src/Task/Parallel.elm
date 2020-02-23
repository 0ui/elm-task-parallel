module Task.Parallel exposing
    ( attempt2, attempt3, attempt4, attempt5, attemptList
    , attempt
    , update2, update3, update4, update5, updateList
    , State2, State3, State4, State5, Msg2, Msg3, Msg4, Msg5, ListState, ListMsg
    )

{-| This library helps you run tasks in parallel when you only need the results
if every task finishes successfully, similar to `Promise.all()` in Javascript. A
good use case is handling the result of multiple HTTP requests.


## Task Helpers

@docs attempt2, attempt3, attempt4, attempt5, attemptList


## Less Common Helpers

@docs attempt


## Update

You will have to pass internal messages and commands along in your update
function in order to eventually get your results.

@docs update2, update3, update4, update5, updateList


## Types

@docs State2, State3, State4, State5, Msg2, Msg3, Msg4, Msg5, ListState, ListMsg

-}

import Task exposing (Task)


{-| Opaque type for storing state of tasks.
-}
type State2 a b
    = FailedState2
    | State2 (Maybe a) (Maybe b)


{-| -}
type State3 a b c
    = FailedState3
    | State3 (Maybe a) (Maybe b) (Maybe c)


{-| -}
type State4 a b c d
    = FailedState4
    | State4 (Maybe a) (Maybe b) (Maybe c) (Maybe d)


{-| -}
type State5 a b c d e
    = FailedState5
    | State5 (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e)


{-| Opaque type for updating state of tasks.
-}
type Msg2 x a b
    = LoadedA2 a
    | LoadedB2 b
    | FailedToLoad2 x


{-| -}
type Msg3 x a b c
    = LoadedA3 a
    | LoadedB3 b
    | LoadedC3 c
    | FailedToLoad3 x


{-| -}
type Msg4 x a b c d
    = LoadedA4 a
    | LoadedB4 b
    | LoadedC4 c
    | LoadedD4 d
    | FailedToLoad4 x


{-| -}
type Msg5 x a b c d e
    = LoadedA5 a
    | LoadedB5 b
    | LoadedC5 c
    | LoadedD5 d
    | LoadedE5 e
    | FailedToLoad5 x


{-| Attempt a single task. The benefit of this over Task.attempt is that it
handles routing the result to the provided success and failure messages. You can
reuse those error messages for different tasks if they're handled the same way.
Since there are no other tasks, you don't need to manage any additional updates.

    type Msg
        = ErrorOcurred Http.Error
        | FetchCompleted MyData

    doTask : Cmd Msg
    doTask =
        attempt FetchCompleted ErrorOcurred fetchMyData

-}
attempt : (a -> msg) -> (x -> msg) -> Task x a -> Cmd msg
attempt successMsg failureMsg task1 =
    task1 |> routeTo successMsg failureMsg


{-| Attempt two tasks which will send an update when either all tasks finish
successfully or one fails. The returned `State` will be used in your main
update function to call [`update`](#update) and pass internal messages.

    type Msg
        = TaskStateUpdated (Task.Parallel.Msg2 Http.Error String Int)
        | OneTaskFailed Http.Error
        | AllTasksCompleted String Int

    doTask : ( Task.Parallel.State2 String Int, Cmd Msg )
    doTask =
        attempt2
            TaskStateUpdated
            fetchString
            fetchInt

-}
attempt2 : (Msg2 x a b -> msg) -> Task x a -> Task x b -> ( State2 a b, Cmd msg )
attempt2 updateMsg task1 task2 =
    ( State2 Nothing Nothing
    , [ task1 |> routeTo (updateMsg << LoadedA2) (updateMsg << FailedToLoad2)
      , task2 |> routeTo (updateMsg << LoadedB2) (updateMsg << FailedToLoad2)
      ]
        |> Cmd.batch
    )


{-| -}
attempt3 : (Msg3 x a b c -> msg) -> Task x a -> Task x b -> Task x c -> ( State3 a b c, Cmd msg )
attempt3 updateMsg task1 task2 task3 =
    ( State3 Nothing Nothing Nothing
    , [ task1 |> routeTo (updateMsg << LoadedA3) (updateMsg << FailedToLoad3)
      , task2 |> routeTo (updateMsg << LoadedB3) (updateMsg << FailedToLoad3)
      , task3 |> routeTo (updateMsg << LoadedC3) (updateMsg << FailedToLoad3)
      ]
        |> Cmd.batch
    )


{-| -}
attempt4 : (Msg4 x a b c d -> msg) -> Task x a -> Task x b -> Task x c -> Task x d -> ( State4 a b c d, Cmd msg )
attempt4 updateMsg task1 task2 task3 task4 =
    ( State4 Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (updateMsg << LoadedA4) (updateMsg << FailedToLoad4)
      , task2 |> routeTo (updateMsg << LoadedB4) (updateMsg << FailedToLoad4)
      , task3 |> routeTo (updateMsg << LoadedC4) (updateMsg << FailedToLoad4)
      , task4 |> routeTo (updateMsg << LoadedD4) (updateMsg << FailedToLoad4)
      ]
        |> Cmd.batch
    )


{-| -}
attempt5 : (Msg5 x a b c d e -> msg) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> ( State5 a b c d e, Cmd msg )
attempt5 updateMsg task1 task2 task3 task4 task5 =
    ( State5 Nothing Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (updateMsg << LoadedA5) (updateMsg << FailedToLoad5)
      , task2 |> routeTo (updateMsg << LoadedB5) (updateMsg << FailedToLoad5)
      , task3 |> routeTo (updateMsg << LoadedC5) (updateMsg << FailedToLoad5)
      , task4 |> routeTo (updateMsg << LoadedD5) (updateMsg << FailedToLoad5)
      , task5 |> routeTo (updateMsg << LoadedE5) (updateMsg << FailedToLoad5)
      ]
        |> Cmd.batch
    )


{-| Handle updates for two tasks by calling `update2` inside of your main update
function to keep this library's internal state updated. If they have either all
finished successfully or one has failed, the corresponding message you provided
will be sent to your main `update` function. Maintain a copy of the returned
state to pass in on each subsequent `update`. This step is required with
`attempt[n]` functions.

    type Msg
        = DownloadUpdated (Task.Parallel.Msg2 Http.Error Actor Film)
        | DownloadFailed Http.Error
        | DownloadCompleted Actor Film

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DownloadUpdated taskMsg ->
                let
                    ( nextTaskState, nextCmd ) =
                        Task.Parallel.update2 model.taskState taskMsg DownloadCompleted DownloadFailed
                in
                ( { model | taskState = nextTaskState }, nextCmd )

            DownloadCompleted actor film ->
                ( { model | actor = actor, film = film, Cmd.none )

            DownloadFailed err ->
                ( { model | loadingError = Just err }, Cmd.none )

-}
update2 : State2 a b -> Msg2 x a b -> (a -> b -> msg) -> (x -> msg) -> ( State2 a b, Cmd msg )
update2 prevData msg successMsg failureMsg =
    case prevData of
        FailedState2 ->
            ( prevData, Cmd.none )

        State2 a b ->
            case msg of
                LoadedA2 data ->
                    nextCmd2 (Just data) b successMsg

                LoadedB2 data ->
                    nextCmd2 a (Just data) successMsg

                FailedToLoad2 err ->
                    ( FailedState2, failureMsg err |> toCmd )


{-| -}
update3 : State3 a b c -> Msg3 x a b c -> (a -> b -> c -> msg) -> (x -> msg) -> ( State3 a b c, Cmd msg )
update3 prevData msg successMsg failureMsg =
    case prevData of
        FailedState3 ->
            ( prevData, Cmd.none )

        State3 a b c ->
            case msg of
                LoadedA3 data ->
                    nextCmd3 (Just data) b c successMsg

                LoadedB3 data ->
                    nextCmd3 a (Just data) c successMsg

                LoadedC3 data ->
                    nextCmd3 a b (Just data) successMsg

                FailedToLoad3 err ->
                    ( FailedState3, failureMsg err |> toCmd )


{-| -}
update4 : State4 a b c d -> Msg4 x a b c d -> (a -> b -> c -> d -> msg) -> (x -> msg) -> ( State4 a b c d, Cmd msg )
update4 prevData msg successMsg failureMsg =
    case prevData of
        FailedState4 ->
            ( prevData, Cmd.none )

        State4 a b c d ->
            case msg of
                LoadedA4 data ->
                    nextCmd4 (Just data) b c d successMsg

                LoadedB4 data ->
                    nextCmd4 a (Just data) c d successMsg

                LoadedC4 data ->
                    nextCmd4 a b (Just data) d successMsg

                LoadedD4 data ->
                    nextCmd4 a b c (Just data) successMsg

                FailedToLoad4 err ->
                    ( FailedState4, failureMsg err |> toCmd )


{-| -}
update5 : State5 a b c d e -> Msg5 x a b c d e -> (a -> b -> c -> d -> e -> msg) -> (x -> msg) -> ( State5 a b c d e, Cmd msg )
update5 prevData msg successMsg failureMsg =
    case prevData of
        FailedState5 ->
            ( prevData, Cmd.none )

        State5 a b c d e ->
            case msg of
                LoadedA5 data ->
                    nextCmd5 (Just data) b c d e successMsg

                LoadedB5 data ->
                    nextCmd5 a (Just data) c d e successMsg

                LoadedC5 data ->
                    nextCmd5 a b (Just data) d e successMsg

                LoadedD5 data ->
                    nextCmd5 a b c (Just data) e successMsg

                LoadedE5 data ->
                    nextCmd5 a b c d (Just data) successMsg

                FailedToLoad5 err ->
                    ( FailedState5, failureMsg err |> toCmd )


{-| Opaque type for storing state of task lists.
-}
type ListState a
    = ListStateFailed
    | ListState (List (Maybe a))


{-| Opaque type for updating state of task lists.
-}
type ListMsg x a
    = ItemLoaded Int a
    | ItemFailed x


{-| Attempt a list of tasks which will update when all the tasks have finished
or when one fails. Similar to a `Task.sequence` except in parallel.

    type Msg
        = DownloadUpdated (ListMsg Http.Error String)
        | DownloadFailed Http.Error
        | DownloadCompleted (List String)

    fetchNames : ( ListState String, nextCmd )
    fetchNames =
        attemptList DownloadUpdated [ fetchFirstName, fetchSecondName, fetchThirdName ]

-}
attemptList : (ListMsg x a -> msg) -> List (Task x a) -> ( ListState a, Cmd msg )
attemptList updateMsg tasks =
    ( tasks |> List.map (always Nothing) |> ListState
    , tasks
        |> List.indexedMap
            (\index task ->
                task
                    |> routeTo (updateMsg << ItemLoaded index) (updateMsg << ItemFailed)
            )
        |> Cmd.batch
    )


{-| Call `updateList` inside of your main update function to check if the
tasks have failed or finished. Maintain a copy of the returned state to pass in
on each subsequent `updateList`. This step is required with
[`attemptList`](#attemptList).

    type Msg
        = DownloadUpdated (Task.Parallel.ListMsg Actor Http.Error)
        | DownloadFailed Http.Error
        | DownloadCompleted (List Actor)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DownloadUpdated taskMsg ->
                let
                    ( nextTaskState, nextCmd ) =
                        Task.Parallel.updateList model.taskState taskMsg DownloadCompleted DownloadFailed
                in
                ( { model | taskState = nextTaskState }, nextCmd )

            DownloadCompleted actors ->
                ( { model | actorList = actors, Cmd.none )

            DownloadFailed err ->
                ( { model | loadingError = Just err }, Cmd.none )

-}
updateList : ListState a -> ListMsg x a -> (List a -> msg) -> (x -> msg) -> ( ListState a, Cmd msg )
updateList prevData listMsg successMsg failureMsg =
    case prevData of
        ListStateFailed ->
            ( prevData, Cmd.none )

        ListState items ->
            case listMsg of
                ItemFailed err ->
                    ( ListStateFailed, failureMsg err |> toCmd )

                ItemLoaded index newItem ->
                    let
                        updatedItems =
                            items
                                |> List.indexedMap
                                    (\i maybeItem ->
                                        if i == index then
                                            Just newItem

                                        else
                                            maybeItem
                                    )
                    in
                    if List.any ((==) Nothing) updatedItems then
                        ( ListState updatedItems, Cmd.none )

                    else
                        ( ListState updatedItems, successMsg (updatedItems |> List.filterMap identity) |> Task.succeed >> Task.perform identity )



-- Internal


routeTo : (data -> msg) -> (x -> msg) -> Task x data -> Cmd msg
routeTo successMsg failureMsg =
    Task.andThen (Task.succeed << Result.Ok)
        >> Task.onError (Task.succeed << Result.Err)
        >> Task.perform
            (\result ->
                case result of
                    Ok data ->
                        successMsg data

                    Err err ->
                        failureMsg err
            )


toCmd : msg -> Cmd msg
toCmd =
    Task.succeed >> Task.perform identity


nextCmd2 : Maybe a -> Maybe b -> (a -> b -> msg) -> ( State2 a b, Cmd msg )
nextCmd2 a b successMsg =
    Maybe.map2 successMsg a b
        |> Maybe.map toCmd
        |> Maybe.withDefault Cmd.none
        |> (\cmd -> ( State2 a b, cmd ))


nextCmd3 : Maybe a -> Maybe b -> Maybe c -> (a -> b -> c -> msg) -> ( State3 a b c, Cmd msg )
nextCmd3 a b c successMsg =
    Maybe.map3 successMsg a b c
        |> Maybe.map toCmd
        |> Maybe.withDefault Cmd.none
        |> (\cmd -> ( State3 a b c, cmd ))


nextCmd4 : Maybe a -> Maybe b -> Maybe c -> Maybe d -> (a -> b -> c -> d -> msg) -> ( State4 a b c d, Cmd msg )
nextCmd4 a b c d successMsg =
    Maybe.map4 successMsg a b c d
        |> Maybe.map toCmd
        |> Maybe.withDefault Cmd.none
        |> (\cmd -> ( State4 a b c d, cmd ))


nextCmd5 : Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> (a -> b -> c -> d -> e -> msg) -> ( State5 a b c d e, Cmd msg )
nextCmd5 a b c d e successMsg =
    Maybe.map5 successMsg a b c d e
        |> Maybe.map toCmd
        |> Maybe.withDefault Cmd.none
        |> (\cmd -> ( State5 a b c d e, cmd ))
