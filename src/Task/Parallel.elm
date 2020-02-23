module Task.Parallel exposing
    ( Expect(..)
    , ListMsg
    , ListState
    , Msg
    , State
    , attempt
    , attempt2
    , attempt3
    , attempt4
    , attempt5
    , attemptList
    , update
    , updateList
    )

{-| This library helps you run tasks in parallel when you only need the results
if every task finishes successfully, similar to `Promise.all()` in Javascript. A
good use case is handling the result of multiple HTTP requests.

## Task Helpers
@docs attempt2, attempt3, attempt4, attempt5, attemptList

## Less Common Helpers
@docs attempt

## Update

You will have to pass internal messages and commands along in order to
eventually get your results.

@docs update, Expect, updateList

## Types
@docs State, Msg, ListState, ListMsg
-}

import Task exposing (Task)

{-| Opaque type for storing state of tasks.
-}
type State a b c d e
    = FailedState
    | State (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e)


{-| Opaque type for updating state of tasks.
-}
type Msg x a b c d e
    = LoadedA a
    | LoadedB b
    | LoadedC c
    | LoadedD d
    | LoadedE e
    | FailedToLoad x


{-| The [`update`](#update) function requires an `Expect` to know how many
results you're expecting. Wrap your success message in one when you call `update`.

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DownloadUpdated taskMsg ->
                let
                    ( nextTaskState, nextCmd ) =
                        Task.Parallel.update model.taskState taskMsg (Expect4 DownloadCompleted) DownloadFailed
                in
                ( { model | taskState = nextTaskState }, nextCmd )

            DownloadCompleted actor film director studio ->
                ( { model | cinemaRecord = Cinema actor film director studio, Cmd.none )

            DownloadFailed err ->
                ( { model | loadingError = Just err }, Cmd.none )
-}

type Expect a b c d e msg
    = Expect1 (a -> msg)
    | Expect2 (a -> b -> msg)
    | Expect3 (a -> b -> c -> msg)
    | Expect4 (a -> b -> c -> d -> msg)
    | Expect5 (a -> b -> c -> d -> e -> msg)

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
        = TaskUpdated (Task.Parallel.Msg String Int () () () Http.Error)
        | TaskFailed Http.Error
        | TaskCompleted String Int

    doTask : ( Task.Parallel.State String Int () () (), Cmd Msg )
    doTask =
        attempt
            TaskUpdated
            fetchString
            fetchInt

-}
attempt2 : (Msg x a b c d e -> msg) -> Task x a -> Task x b -> ( State a b c d e, Cmd msg )
attempt2 updateMsg task1 task2 =
    ( initialState
    , [ task1 |> routeTo (updateMsg << LoadedA) (updateMsg << FailedToLoad)
      , task2 |> routeTo (updateMsg << LoadedB) (updateMsg << FailedToLoad)
      ]
        |> Cmd.batch
    )

{-|-}

attempt3 : (Msg x a b c d e -> msg) -> Task x a -> Task x b -> Task x c -> ( State a b c d e, Cmd msg )
attempt3 updateMsg task1 task2 task3 =
    ( initialState
    , [ task1 |> routeTo (updateMsg << LoadedA) (updateMsg << FailedToLoad)
      , task2 |> routeTo (updateMsg << LoadedB) (updateMsg << FailedToLoad)
      , task3 |> routeTo (updateMsg << LoadedC) (updateMsg << FailedToLoad)
      ]
        |> Cmd.batch
    )

{-|-}
attempt4 : (Msg x a b c d e -> msg) -> Task x a -> Task x b -> Task x c -> Task x d -> ( State a b c d e, Cmd msg )
attempt4 updateMsg task1 task2 task3 task4 =
    ( initialState
    , [ task1 |> routeTo (updateMsg << LoadedA) (updateMsg << FailedToLoad)
      , task2 |> routeTo (updateMsg << LoadedB) (updateMsg << FailedToLoad)
      , task3 |> routeTo (updateMsg << LoadedC) (updateMsg << FailedToLoad)
      , task4 |> routeTo (updateMsg << LoadedD) (updateMsg << FailedToLoad)
      ]
        |> Cmd.batch
    )

{-|-}
attempt5 : (Msg x a b c d e -> msg) -> Task x a -> Task x b -> Task x c -> Task x d -> Task x e -> ( State a b c d e, Cmd msg )
attempt5 updateMsg task1 task2 task3 task4 task5 =
    ( initialState
    , [ task1 |> routeTo (updateMsg << LoadedA) (updateMsg << FailedToLoad)
      , task2 |> routeTo (updateMsg << LoadedB) (updateMsg << FailedToLoad)
      , task3 |> routeTo (updateMsg << LoadedC) (updateMsg << FailedToLoad)
      , task4 |> routeTo (updateMsg << LoadedD) (updateMsg << FailedToLoad)
      , task5 |> routeTo (updateMsg << LoadedE) (updateMsg << FailedToLoad)
      ]
        |> Cmd.batch
    )


{-| Call `update` inside of your main update function to check if the 
tasks have failed or finished. Maintain a copy of the returned state to pass in
on each subsequent `update`. This step is required with `attempt[n]` functions.

    type Msg
        = DownloadUpdated (Task.Parallel.Msg Actor Film Director Studio Distributor Http.Error)
        | DownloadFailed Http.Error
        | DownloadCompleted Actor Film Director Studio Distributor

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DownloadUpdated taskMsg ->
                let
                    ( nextTaskState, nextCmd ) =
                        Task.Parallel.update model.taskState taskMsg (Expect5 DownloadCompleted) DownloadFailed
                in
                ( { model | taskState = nextTaskState }, nextCmd )

            DownloadCompleted actor film director studio distributor ->
                ( { model | cinemaRecord = Cinema actor film director studio distributor, Cmd.none )

            DownloadFailed err ->
                ( { model | loadingError = Just err }, Cmd.none )

-}
update : State a b c d e -> Msg x a b c d e -> Expect a b c d e msg -> (x -> msg) -> ( State a b c d e, Cmd msg )
update prevData msg successMsg failureMsg =
    case prevData of
        FailedState ->
            {- Quit handling further messages once first failure has occurred. -}
            ( prevData, Cmd.none )

        State a b c d e ->
            case msg of
                LoadedA data ->
                    getNextCmd successMsg (Just data) b c d e

                LoadedB data ->
                    getNextCmd successMsg a (Just data) c d e

                LoadedC data ->
                    getNextCmd successMsg a b (Just data) d e

                LoadedD data ->
                    getNextCmd successMsg a b c (Just data) e

                LoadedE data ->
                    getNextCmd successMsg a b c d (Just data)

                FailedToLoad err ->
                    ( FailedState, failureMsg err |> (Task.succeed >> Task.perform identity) )



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
        = DownloadUpdated (ListState String)
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
                    ( ListStateFailed, failureMsg err |> (Task.succeed >> Task.perform identity) )

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


initialState : State a b c d e
initialState =
    State Nothing Nothing Nothing Nothing Nothing


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


getNextCmd : Expect a b c d e msg -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> ( State a b c d e, Cmd msg )
getNextCmd successMsg a b c d e =
    (case successMsg of
        Expect1 func ->
            Maybe.map func a

        Expect2 func ->
            Maybe.map2 func a b

        Expect3 func ->
            Maybe.map3 func a b c

        Expect4 func ->
            Maybe.map4 func a b c d

        Expect5 func ->
            Maybe.map5 func a b c d e
    )
        |> Maybe.map (Task.succeed >> Task.perform identity)
        |> Maybe.withDefault Cmd.none
        |> (\cmd -> ( State a b c d e, cmd ))
