module Task.Parallel exposing
    ( attempt2, attempt3, attempt4, attempt5, attemptList
    , attempt
    , attemptMap2
    , mapState
    , update2, update3, update4, update5, updateList
    , task2
    , State2, State3, State4, State5, Msg2, Msg3, Msg4, Msg5, ListState, ListMsg
    , Task3, Task4, Task5
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


type alias Task3 x a b c msg =
    { successMsg : a -> b -> c -> msg
    , failureMsg : x -> msg
    , state : State3 a b c
    }


type alias Task4 x a b c d msg =
    { successMsg : a -> b -> c -> d -> msg
    , failureMsg : x -> msg
    , state : State4 a b c d
    }


type alias Task5 x a b c d e msg =
    { successMsg : a -> b -> c -> d -> e -> msg
    , failureMsg : x -> msg
    , state : State5 a b c d e
    }

{-| Opaque type for storing state of tasks.
-}
type State2 a b
    = FailedState2
    | State2
        { a : Maybe a
        , b : Maybe b
        }


{-| -}
type State3 a b c
    = FailedState3
    | State3
        { a : Maybe a
        , b : Maybe b
        , c : Maybe c
        }


{-| -}
type State4 a b c d
    = FailedState4
    | State4
        { a : Maybe a
        , b : Maybe b
        , c : Maybe c
        , d : Maybe d
        }


{-| -}
type State5 a b c d e
    = FailedState5
    | State5
        { a : Maybe a
        , b : Maybe b
        , c : Maybe c
        , d : Maybe d
        , e : Maybe e
        }


{-| Opaque type for updating state of tasks.
-}
type Msg2 x a b msg
    = LoadedA2 (a -> b -> msg) a
    | LoadedB2 (a -> b -> msg) b
    | FailedToLoad2 (x -> msg) x


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

attempt2 :
    (a -> b -> msg)
    -> (x -> msg)
    ->  TaskConfig2 x a b msg
    -> ( State2 a b, Cmd msg )
attempt2 successMsg failureMsg (TaskConfig2 t1 t2 updateMsg) =
    ( State2 { a = Nothing, b = Nothing }
    , [ t1 |> routeTo (updateMsg << LoadedA2 successMsg) (updateMsg << FailedToLoad2 failureMsg)
      , t2 |> routeTo (updateMsg << LoadedB2 successMsg) (updateMsg << FailedToLoad2 failureMsg)
      ]
        |> Cmd.batch
    )


{-| -}
attempt3 : Task x a
    -> Task x b
    -> Task x c
    -> { successMsg : a -> b -> c -> msg
       , failureMsg : x -> msg
       , updateMsg : Msg3 x a b c -> msg
       }
    -> ( Task3 x a b c msg, Cmd msg )
attempt3 t1 t2 t3 { successMsg, failureMsg, updateMsg } =
    ( Task3 successMsg failureMsg (State3 { a = Nothing, b = Nothing, c = Nothing } )
    , [ t1 |> routeTo (updateMsg << LoadedA3) (updateMsg << FailedToLoad3)
      , t2 |> routeTo (updateMsg << LoadedB3) (updateMsg << FailedToLoad3)
      , t3 |> routeTo (updateMsg << LoadedC3) (updateMsg << FailedToLoad3)
      ]
        |> Cmd.batch
    )


{-| -}
attempt4 : Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> { successMsg : a -> b -> c -> d -> msg
       , failureMsg : x -> msg
       , updateMsg : Msg4 x a b c d -> msg
       }
    -> ( Task4 x a b c d msg, Cmd msg )
attempt4 t1 t2 t3 t4 { successMsg, failureMsg, updateMsg } =
    ( Task4 successMsg failureMsg (State4 { a = Nothing, b = Nothing, c = Nothing, d = Nothing } )
    , [ t1 |> routeTo (updateMsg << LoadedA4) (updateMsg << FailedToLoad4)
      , t2 |> routeTo (updateMsg << LoadedB4) (updateMsg << FailedToLoad4)
      , t3 |> routeTo (updateMsg << LoadedC4) (updateMsg << FailedToLoad4)
      , t4 |> routeTo (updateMsg << LoadedD4) (updateMsg << FailedToLoad4)
      ]
        |> Cmd.batch
    )


{-| -}
attempt5 : Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> { successMsg : a -> b -> c -> d -> e -> msg
       , failureMsg : x -> msg
       , updateMsg : Msg5 x a b c d e -> msg
       }
    -> ( Task5 x a b c d e msg, Cmd msg )
attempt5 t1 t2 t3 t4 t5 { successMsg, failureMsg, updateMsg } =
    ( Task5 successMsg failureMsg (State5 { a = Nothing, b = Nothing, c = Nothing, d = Nothing, e = Nothing } )
    , [ t1 |> routeTo (updateMsg << LoadedA5) (updateMsg << FailedToLoad5)
      , t2 |> routeTo (updateMsg << LoadedB5) (updateMsg << FailedToLoad5)
      , t3 |> routeTo (updateMsg << LoadedC5) (updateMsg << FailedToLoad5)
      , t4 |> routeTo (updateMsg << LoadedD5) (updateMsg << FailedToLoad5)
      , t5 |> routeTo (updateMsg << LoadedE5) (updateMsg << FailedToLoad5)
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
update2 : Msg2 x a b msg -> State2 a b -> ( State2 a b, Cmd msg )
update2 msg state =
    case state of
        FailedState2 ->
            ( state, Cmd.none )

        State2 loading ->
            let
                nextCmd nextLoading successMsg =
                    ( State2 nextLoading
                    , Maybe.map2 successMsg nextLoading.a nextLoading.b
                        |> Maybe.map toCmd
                        |> Maybe.withDefault Cmd.none
                    )
            in
                case msg of
                    LoadedA2 successMsg data ->
                        nextCmd { loading | a = Just data } successMsg

                    LoadedB2 successMsg data ->
                        nextCmd { loading | b = Just data } successMsg

                    FailedToLoad2 failureMsg err ->
                        ( FailedState2, failureMsg err |> toCmd )


{-| -}
update3 : Msg3 x a b c -> Task3 x a b c msg -> ( Task3 x a b c msg, Cmd msg )
update3 msg task =
    case task.state of
        FailedState3 ->
            ( task, Cmd.none )

        State3 loading ->
            let
                nextCmd nextLoading =
                    ( { task | state = State3 nextLoading }
                    , Maybe.map3 task.successMsg nextLoading.a nextLoading.b nextLoading.c
                        |> Maybe.map toCmd
                        |> Maybe.withDefault Cmd.none
                    )
            in
                case msg of
                    LoadedA3 data ->
                        nextCmd { loading | a = Just data }

                    LoadedB3 data ->
                        nextCmd { loading | b = Just data }

                    LoadedC3 data ->
                        nextCmd { loading | c = Just data }

                    FailedToLoad3 err ->
                        ( { task | state = FailedState3 }, task.failureMsg err |> toCmd )


{-| -}
update4 : Msg4 x a b c d -> Task4 x a b c d msg -> ( Task4 x a b c d msg, Cmd msg )
update4 msg task =
    case task.state of
        FailedState4 ->
            ( task, Cmd.none )

        State4 loading ->
            let
                nextCmd nextLoading =
                    ( { task | state = State4 nextLoading }
                    , Maybe.map4 task.successMsg nextLoading.a nextLoading.b nextLoading.c nextLoading.d
                        |> Maybe.map toCmd
                        |> Maybe.withDefault Cmd.none
                    )
            in
                case msg of
                    LoadedA4 data ->
                        nextCmd { loading | a = Just data }

                    LoadedB4 data ->
                        nextCmd { loading | b = Just data }

                    LoadedC4 data ->
                        nextCmd { loading | c = Just data }

                    LoadedD4 data ->
                        nextCmd { loading | d = Just data }

                    FailedToLoad4 err ->
                        ( { task | state = FailedState4 }, task.failureMsg err |> toCmd )

{-|-}
update5 : Msg5 x a b c d e -> Task5 x a b c d e msg -> ( Task5 x a b c d e msg, Cmd msg )
update5 msg task =
    case task.state of
        FailedState5 ->
            ( task, Cmd.none )

        State5 loading ->
            let
                nextCmd nextLoading =
                    ( { task | state = State5 nextLoading }
                    , Maybe.map5 task.successMsg nextLoading.a nextLoading.b nextLoading.c nextLoading.d nextLoading.e
                        |> Maybe.map toCmd
                        |> Maybe.withDefault Cmd.none
                    )
            in
                case msg of
                    LoadedA5 data ->
                        nextCmd { loading | a = Just data }

                    LoadedB5 data ->
                        nextCmd { loading | b = Just data }


                    LoadedC5 data ->
                        nextCmd { loading | c = Just data }


                    LoadedD5 data ->
                        nextCmd { loading | d = Just data }


                    LoadedE5 data ->
                        nextCmd { loading | e = Just data }

                    FailedToLoad5 err ->
                        ( { task | state = FailedState5 }, task.failureMsg err |> toCmd )



attemptMap2 : (a -> b -> c)
    -> (c -> msg)
    -> (x -> msg)
    -> TaskConfig2 x a b msg
    -> ( State2 a b, Cmd msg )
attemptMap2 mapFunc successMsg failureMsg config = 
    let
        mapSuccess =
            (\a b -> mapFunc a b |> successMsg)
    in
    attempt2 mapSuccess failureMsg config

type TaskConfig2 x a b msg
    = TaskConfig2 (Task x a) (Task x b) (Msg2 x a b msg -> msg)

task2 : (Msg2 x a b msg -> msg) -> Task x a -> Task x b -> TaskConfig2 x a b msg
task2 updateMsg t1 t2 =
    TaskConfig2 t1 t2 updateMsg

mapState : (a -> c) -> (a, b) -> (c, b)
mapState =
    Tuple.mapFirst


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
        = DownloadUpdated (Task.Parallel.ListMsg Http.Error Actor)
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
