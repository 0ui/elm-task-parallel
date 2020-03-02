module Task.Parallel exposing
    ( attempt2, attempt3, attempt4, attempt5, attemptList
    , attempt
    , attempt2Map, attempt3Map, attempt4Map, attempt5Map
    , attempt2AndThen, attempt3AndThen, attempt4AndThen, attempt5AndThen
    , update2, update3, update4, update5, updateList
    , mapState, mapMsg
    , Task2(..), Task3(..), Task4(..), Task5(..)
    , State2, State3, State4, State5, Msg2, Msg3, Msg4, Msg5, ListState, ListMsg
    )

{-| This library helps you run tasks in parallel when you only need the results
if every task finishes successfully, similar to `Promise.all()` in Javascript. A
good use case is handling the result of multiple HTTP requests.


## Task Helpers

@docs attempt2, attempt3, attempt4, attempt5, attemptList


## Less Common Helpers

@docs attempt


## Map Results

@docs attempt2Map, attempt3Map, attempt4Map, attempt5Map


## Chain Additional Tasks

@docs attempt2AndThen, attempt3AndThen, attempt4AndThen, attempt5AndThen


## Update

You will have to pass internal messages and commands along in your update
function in order to eventually get your results.

@docs update2, update3, update4, update5, updateList


## Convenience

@docs mapState


## Task

A type to configure your parallel tasks.

@docs Task2, Task3, Task4, Task5


## Other Types

These are the types you will have to annotate your Model and Msg with in order to
track the state of parallel tasks. These require the error type and result types of
all of your tasks as well as the Msg type from your module that it will be sending.
See [update2](#update2) for an example

@docs State2, State3, State4, State5, Msg2, Msg3, Msg4, Msg5, ListState, ListMsg

-}

import Task exposing (Task)


{-|

    Task.Parallel.Task2 fetchString fetchInt
        |> attempt2 TaskStateUpdated AllFinished OneFailed

-}
type Task2 x a b
    = Task2 (Task x a) (Task x b)


{-| -}
type Task3 x a b c
    = Task3 (Task x a) (Task x b) (Task x c)


{-| -}
type Task4 x a b c d
    = Task4 (Task x a) (Task x b) (Task x c) (Task x d)


{-| -}
type Task5 x a b c d e
    = Task5 (Task x a) (Task x b) (Task x c) (Task x d) (Task x e)



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
type Msg2 msg x a b
    = LoadedA2 (a -> b -> Cmd msg) a
    | LoadedB2 (a -> b -> Cmd msg) b
    | FailedToLoad2 (x -> Cmd msg) x


{-| -}
type Msg3 msg x a b c
    = LoadedA3 (a -> b -> c -> Cmd msg) a
    | LoadedB3 (a -> b -> c -> Cmd msg) b
    | LoadedC3 (a -> b -> c -> Cmd msg) c
    | FailedToLoad3 (x -> Cmd msg) x


{-| -}
type Msg4 msg x a b c d
    = LoadedA4 (a -> b -> c -> d -> Cmd msg) a
    | LoadedB4 (a -> b -> c -> d -> Cmd msg) b
    | LoadedC4 (a -> b -> c -> d -> Cmd msg) c
    | LoadedD4 (a -> b -> c -> d -> Cmd msg) d
    | FailedToLoad4 (x -> Cmd msg) x


{-| -}
type Msg5 msg x a b c d e
    = LoadedA5 (a -> b -> c -> d -> e -> Cmd msg) a
    | LoadedB5 (a -> b -> c -> d -> e -> Cmd msg) b
    | LoadedC5 (a -> b -> c -> d -> e -> Cmd msg) c
    | LoadedD5 (a -> b -> c -> d -> e -> Cmd msg) d
    | LoadedE5 (a -> b -> c -> d -> e -> Cmd msg) e
    | FailedToLoad5 (x -> Cmd msg) x


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
successfully or one fails. The returned [State](#State2) will be used in your main
update function to call [`update`](#update) and pass internal messages.

    type Msg
        = TaskStateUpdated (Task.Parallel.Msg2 Msg Http.Error String Int)
        | OneFailed Http.Error
        | AllFinished String Int

    doTask : ( Task.Parallel.State2 String Int, Cmd Msg )
    doTask =
        Task.Parallel.Task2 fetchString fetchInt
            |> attempt2 TaskStateUpdated AllFinished OneFailed

-}
attempt2 :
    (Msg2 msg x a b -> msg)
    -> (a -> b -> msg)
    -> (x -> msg)
    -> Task2 x a b
    -> ( State2 a b, Cmd msg )
attempt2 updateMsg successMsg failureMsg config =
    task2 updateMsg (\a b -> successMsg a b |> toCmd) (failureMsg >> toCmd) config


{-| -}
attempt3 :
    (Msg3 msg x a b c -> msg)
    -> (a -> b -> c -> msg)
    -> (x -> msg)
    -> Task3 x a b c
    -> ( State3 a b c, Cmd msg )
attempt3 updateMsg successMsg failureMsg config =
    task3 updateMsg (\a b c -> successMsg a b c |> toCmd) (failureMsg >> toCmd) config


{-| -}
attempt4 :
    (Msg4 msg x a b c d -> msg)
    -> (a -> b -> c -> d -> msg)
    -> (x -> msg)
    -> Task4 x a b c d
    -> ( State4 a b c d, Cmd msg )
attempt4 updateMsg successMsg failureMsg config =
    task4 updateMsg (\a b c d -> successMsg a b c d |> toCmd) (failureMsg >> toCmd) config


{-| -}
attempt5 :
    (Msg5 msg x a b c d e -> msg)
    -> (a -> b -> c -> d -> e -> msg)
    -> (x -> msg)
    -> Task5 x a b c d e
    -> ( State5 a b c d e, Cmd msg )
attempt5 updateMsg successMsg failureMsg config =
    task5 updateMsg (\a b c d e -> successMsg a b c d e |> toCmd) (failureMsg >> toCmd) config


{-| Like attempt2 except this will map the resulting values into one value
that you can provide to your success message.

    type Msg
        = TaskUpdated (Task.Parallel.Msg2 Msg Http.Error Int Int)
        | OneFailed Http.Error
        | GotFinalString String

    resultsToString : Int -> Int -> String
    resultsToString someNum otherNum =
        String.fromInt someNum ++ String.fromInt otherNum

    doTask : ( Task.Parallel.State2 Int Int, Cmd Msg )
    doTask =
        Task.Parallel.Task2 fetchInt fetchOtherInt
            |> attempt2Map resultsToString TaskUpdated GotFinalString OneFailed

-}
attempt2Map :
    (a -> b -> c)
    -> (Msg2 msg x a b -> msg)
    -> (c -> msg)
    -> (x -> msg)
    -> Task2 x a b
    -> ( State2 a b, Cmd msg )
attempt2Map mapFunc updateMsg successMsg failureMsg config =
    task2 updateMsg (\a b -> mapFunc a b |> successMsg |> toCmd) (failureMsg >> toCmd) config


{-| -}
attempt3Map :
    (a -> b -> c -> d)
    -> (Msg3 msg x a b c -> msg)
    -> (d -> msg)
    -> (x -> msg)
    -> Task3 x a b c
    -> ( State3 a b c, Cmd msg )
attempt3Map mapFunc updateMsg successMsg failureMsg config =
    task3 updateMsg (\a b c -> mapFunc a b c |> successMsg |> toCmd) (failureMsg >> toCmd) config


{-| -}
attempt4Map :
    (a -> b -> c -> d -> e)
    -> (Msg4 msg x a b c d -> msg)
    -> (e -> msg)
    -> (x -> msg)
    -> Task4 x a b c d
    -> ( State4 a b c d, Cmd msg )
attempt4Map mapFunc updateMsg successMsg failureMsg config =
    task4 updateMsg (\a b c d -> mapFunc a b c d |> successMsg |> toCmd) (failureMsg >> toCmd) config


{-| -}
attempt5Map :
    (a -> b -> c -> d -> e -> f)
    -> (Msg5 msg x a b c d e -> msg)
    -> (f -> msg)
    -> (x -> msg)
    -> Task5 x a b c d e
    -> ( State5 a b c d e, Cmd msg )
attempt5Map mapFunc updateMsg successMsg failureMsg config =
    task5 updateMsg (\a b c d e -> mapFunc a b c d e |> successMsg |> toCmd) (failureMsg >> toCmd) config


{-| Like attempt2 except it will attempt an additional task command after the first
2 have completed. You can call Task.andThen on this additional task to chain
sequential tasks after the parallel ones have completed.

    type Msg
        = TaskUpdated (Task.Parallel.Msg2 Msg Http.Error String Int)
        | OneFailed Http.Error
        | GotTimeWhenFinished Time.Posix

    doTask : ( Task.Parallel.State2 String Int, Cmd Msg )
    doTask =
        Task.Parallel.Task2 fetchString fetchInt
            |> Task.Parallel.attempt2AndThen getTimeWhenFinished TaskUpdated OneFailed

    getTimeWhenFinished : String -> Int -> Cmd Msg
    getTimeWhenFinished _ _ =
        Task.perform GotTimeWhenFinished Time.now

-}
attempt2AndThen :
    (a -> b -> Cmd msg)
    -> (Msg2 msg x a b -> msg)
    -> (x -> msg)
    -> Task2 x a b
    -> ( State2 a b, Cmd msg )
attempt2AndThen attemptSomeTask updateMsg failureMsg config =
    task2 updateMsg attemptSomeTask (failureMsg >> toCmd) config


{-| -}
attempt3AndThen :
    (a -> b -> c -> Cmd msg)
    -> (Msg3 msg x a b c -> msg)
    -> (x -> msg)
    -> Task3 x a b c
    -> ( State3 a b c, Cmd msg )
attempt3AndThen attemptSomeTask updateMsg failureMsg config =
    task3 updateMsg attemptSomeTask (failureMsg >> toCmd) config


{-| -}
attempt4AndThen :
    (a -> b -> c -> d -> Cmd msg)
    -> (Msg4 msg x a b c d -> msg)
    -> (x -> msg)
    -> Task4 x a b c d
    -> ( State4 a b c d, Cmd msg )
attempt4AndThen attemptSomeTask updateMsg failureMsg config =
    task4 updateMsg attemptSomeTask (failureMsg >> toCmd) config


{-| -}
attempt5AndThen :
    (a -> b -> c -> d -> e -> Cmd msg)
    -> (Msg5 msg x a b c d e -> msg)
    -> (x -> msg)
    -> Task5 x a b c d e
    -> ( State5 a b c d e, Cmd msg )
attempt5AndThen attemptSomeTask updateMsg failureMsg config =
    task5 updateMsg attemptSomeTask (failureMsg >> toCmd) config


{-| Handle updates for two tasks by calling `update2` inside of your main update
function to keep this library's internal state updated. If they have either all
finished successfully or one has failed, the corresponding message you provided
will be sent to your main `update` function. Maintain a copy of the returned
state to pass in on each subsequent `update`. This step is required with
`attempt[n]` functions.

    type Model
        = Loading (Task.Parallel.State2 Actor Film)
        | Loaded Actor Film
        | LoadFailure Http.Error

    type Msg
        = DownloadUpdated (Task.Parallel.Msg2 Msg Http.Error Actor Film)
        | DownloadCompleted Actor Film
        | DownloadFailed Http.Error

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case model of
            Loading taskState ->
                case msg of
                    DownloadUpdated taskMsg ->
                        Task.Parallel.update2 taskState taskMsg
                            |> Task.Parallel.mapState Loading

                    -- ( State2 Actor Film, Cmd Msg ) -> ( Model, Cmd Msg )
                    DownloadCompleted actor film ->
                        ( Loaded actor film, Cmd.none )

                    DownloadFailed err ->
                        ( LoadFailure err, Cmd.none )

            _ ->
                -- The rest of your update...
                ( model, Cmd.none )

-}
update2 : Msg2 msg x a b -> State2 a b -> ( State2 a b, Cmd msg )
update2 msg state =
    case state of
        FailedState2 ->
            ( state, Cmd.none )

        State2 loading ->
            let
                nextCmd nextLoading successMsg =
                    ( State2 nextLoading
                    , Maybe.map2 successMsg nextLoading.a nextLoading.b
                        |> Maybe.withDefault Cmd.none
                    )
            in
            case msg of
                LoadedA2 successMsg data ->
                    nextCmd { loading | a = Just data } successMsg

                LoadedB2 successMsg data ->
                    nextCmd { loading | b = Just data } successMsg

                FailedToLoad2 failureMsg err ->
                    ( FailedState2, failureMsg err )


{-| -}
update3 : Msg3 msg x a b c -> State3 a b c -> ( State3 a b c, Cmd msg )
update3 msg state =
    case state of
        FailedState3 ->
            ( state, Cmd.none )

        State3 loading ->
            let
                nextCmd nextLoading successMsg =
                    ( State3 nextLoading
                    , Maybe.map3 successMsg nextLoading.a nextLoading.b nextLoading.c
                        |> Maybe.withDefault Cmd.none
                    )
            in
            case msg of
                LoadedA3 successMsg data ->
                    nextCmd { loading | a = Just data } successMsg

                LoadedB3 successMsg data ->
                    nextCmd { loading | b = Just data } successMsg

                LoadedC3 successMsg data ->
                    nextCmd { loading | c = Just data } successMsg

                FailedToLoad3 failureMsg err ->
                    ( FailedState3, failureMsg err )


{-| -}
update4 : Msg4 msg x a b c d -> State4 a b c d -> ( State4 a b c d, Cmd msg )
update4 msg state =
    case state of
        FailedState4 ->
            ( state, Cmd.none )

        State4 loading ->
            let
                nextCmd nextLoading successMsg =
                    ( State4 nextLoading
                    , Maybe.map4 successMsg nextLoading.a nextLoading.b nextLoading.c nextLoading.d
                        |> Maybe.withDefault Cmd.none
                    )
            in
            case msg of
                LoadedA4 successMsg data ->
                    nextCmd { loading | a = Just data } successMsg

                LoadedB4 successMsg data ->
                    nextCmd { loading | b = Just data } successMsg

                LoadedC4 successMsg data ->
                    nextCmd { loading | c = Just data } successMsg

                LoadedD4 successMsg data ->
                    nextCmd { loading | d = Just data } successMsg

                FailedToLoad4 failureMsg err ->
                    ( FailedState4, failureMsg err )


{-| -}
update5 : Msg5 msg x a b c d e -> State5 a b c d e -> ( State5 a b c d e, Cmd msg )
update5 msg state =
    case state of
        FailedState5 ->
            ( state, Cmd.none )

        State5 loading ->
            let
                nextCmd nextLoading successMsg =
                    ( State5 nextLoading
                    , Maybe.map5 successMsg nextLoading.a nextLoading.b nextLoading.c nextLoading.d nextLoading.e
                        |> Maybe.withDefault Cmd.none
                    )
            in
            case msg of
                LoadedA5 successMsg data ->
                    nextCmd { loading | a = Just data } successMsg

                LoadedB5 successMsg data ->
                    nextCmd { loading | b = Just data } successMsg

                LoadedC5 successMsg data ->
                    nextCmd { loading | c = Just data } successMsg

                LoadedD5 successMsg data ->
                    nextCmd { loading | d = Just data } successMsg

                LoadedE5 successMsg data ->
                    nextCmd { loading | e = Just data } successMsg

                FailedToLoad5 failureMsg err ->
                    ( FailedState5, failureMsg err )


{-| Just an alias for Tuple.mapFirst but a reminder of a quick way to
tag the task state in your model as in ( State, Cmd Msg ) -> ( Model, Cmd Msg )
Perhaps it obfuscates too much but I found it helpful.
-}
mapState : (a -> c) -> ( a, b ) -> ( c, b )
mapState =
    Tuple.mapFirst


{-| Same but for commands. (childMsg -> parentMsg) -> ( State, Cmd childMsg ) -> ( State, Cmd parentMsg )
-}
mapMsg : (b -> msg) -> ( a, Cmd b ) -> ( a, Cmd msg )
mapMsg =
    Tuple.mapSecond << Cmd.map

{-| Opaque type for storing state of task lists.
-}
type ListState a
    = ListState (List (Maybe a))
    | ListStateFailed


{-| Opaque type for updating state of task lists.
-}
type ListMsg msg x a
    = ItemLoaded (List a -> msg) Int a
    | ItemFailed (x -> msg) x


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
attemptList : (ListMsg msg x a -> msg) -> (List a -> msg) -> (x -> msg) -> List (Task x a) -> ( ListState a, Cmd msg )
attemptList updateMsg successMsg failureMsg tasks =
    ( tasks |> List.map (always Nothing) |> ListState
    , tasks
        |> List.indexedMap
            (\index task ->
                task
                    |> routeTo (updateMsg << ItemLoaded successMsg index) (updateMsg << ItemFailed failureMsg)
            )
        |> Cmd.batch
    )


{-| Call `updateList` inside of your main update function to check if the
tasks have failed or finished. Maintain a copy of the returned state to pass in
on each subsequent `updateList`. This step is required with
[`attemptList`](#attemptList).

    type Msg
        = DownloadUpdated (Task.Parallel.ListMsg Msg Http.Error Actor)
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
updateList : ListState a -> ListMsg msg x a -> ( ListState a, Cmd msg )
updateList prevData listMsg =
    case prevData of
        ListStateFailed ->
            ( prevData, Cmd.none )

        ListState items ->
            case listMsg of
                ItemFailed failureMsg err ->
                    ( ListStateFailed, failureMsg err |> toCmd )

                ItemLoaded successMsg index newItem ->
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


task2 :
    (Msg2 msg x a b -> msg)
    -> (a -> b -> Cmd msg)
    -> (x -> Cmd msg)
    -> Task2 x a b
    -> ( State2 a b, Cmd msg )
task2 updateMsg successMsg failureMsg (Task2 t1 t2) =
    ( State2 { a = Nothing, b = Nothing }
    , [ t1 |> routeTo (updateMsg << LoadedA2 successMsg) (updateMsg << FailedToLoad2 failureMsg)
      , t2 |> routeTo (updateMsg << LoadedB2 successMsg) (updateMsg << FailedToLoad2 failureMsg)
      ]
        |> Cmd.batch
    )


task3 :
    (Msg3 msg x a b c -> msg)
    -> (a -> b -> c -> Cmd msg)
    -> (x -> Cmd msg)
    -> Task3 x a b c
    -> ( State3 a b c, Cmd msg )
task3 updateMsg successMsg failureMsg (Task3 t1 t2 t3) =
    ( State3 { a = Nothing, b = Nothing, c = Nothing }
    , [ t1 |> routeTo (updateMsg << LoadedA3 successMsg) (updateMsg << FailedToLoad3 failureMsg)
      , t2 |> routeTo (updateMsg << LoadedB3 successMsg) (updateMsg << FailedToLoad3 failureMsg)
      , t3 |> routeTo (updateMsg << LoadedC3 successMsg) (updateMsg << FailedToLoad3 failureMsg)
      ]
        |> Cmd.batch
    )


task4 :
    (Msg4 msg x a b c d -> msg)
    -> (a -> b -> c -> d -> Cmd msg)
    -> (x -> Cmd msg)
    -> Task4 x a b c d
    -> ( State4 a b c d, Cmd msg )
task4 updateMsg successMsg failureMsg (Task4 t1 t2 t3 t4) =
    ( State4 { a = Nothing, b = Nothing, c = Nothing, d = Nothing }
    , [ t1 |> routeTo (updateMsg << LoadedA4 successMsg) (updateMsg << FailedToLoad4 failureMsg)
      , t2 |> routeTo (updateMsg << LoadedB4 successMsg) (updateMsg << FailedToLoad4 failureMsg)
      , t3 |> routeTo (updateMsg << LoadedC4 successMsg) (updateMsg << FailedToLoad4 failureMsg)
      , t4 |> routeTo (updateMsg << LoadedD4 successMsg) (updateMsg << FailedToLoad4 failureMsg)
      ]
        |> Cmd.batch
    )


task5 :
    (Msg5 msg x a b c d e -> msg)
    -> (a -> b -> c -> d -> e -> Cmd msg)
    -> (x -> Cmd msg)
    -> Task5 x a b c d e
    -> ( State5 a b c d e, Cmd msg )
task5 updateMsg successMsg failureMsg (Task5 t1 t2 t3 t4 t5) =
    ( State5 { a = Nothing, b = Nothing, c = Nothing, d = Nothing, e = Nothing }
    , [ t1 |> routeTo (updateMsg << LoadedA5 successMsg) (updateMsg << FailedToLoad5 failureMsg)
      , t2 |> routeTo (updateMsg << LoadedB5 successMsg) (updateMsg << FailedToLoad5 failureMsg)
      , t3 |> routeTo (updateMsg << LoadedC5 successMsg) (updateMsg << FailedToLoad5 failureMsg)
      , t4 |> routeTo (updateMsg << LoadedD5 successMsg) (updateMsg << FailedToLoad5 failureMsg)
      , t5 |> routeTo (updateMsg << LoadedE5 successMsg) (updateMsg << FailedToLoad5 failureMsg)
      ]
        |> Cmd.batch
    )
