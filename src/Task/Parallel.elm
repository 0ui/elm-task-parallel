module Task.Parallel exposing
    ( attempt2, attempt3, attempt4, attempt5, attempt6, attempt7, attempt8, attempt9, attemptList
    , attempt
    , update2, update3, update4, update5, update6, update7, update8, update9, updateList
    , State2, State3, State4, State5, State6, State7, State8, State9, Msg2, Msg3, Msg4, Msg5, Msg6, Msg7, Msg8, Msg9, ListState, ListMsg
    )

{-| This library helps you run tasks in parallel when you only need the results
if every task finishes successfully, similar to `Promise.all()` in Javascript. A
good use case is handling the result of multiple HTTP requests.


## Task Helpers

@docs attempt2, attempt3, attempt4, attempt5, attempt6, attempt7, attempt8, attempt9, attemptList


## Less Common Helpers

@docs attempt


## Update

You will have to pass internal messages and commands along in your update
function in order to eventually get your results.

@docs update2, update3, update4, update5, update6, update7, update8, update9, updateList


## Types

@docs State2, State3, State4, State5, State6, State7, State8, State9, Msg2, Msg3, Msg4, Msg5, Msg6, Msg7, Msg8, Msg9, ListState, ListMsg

-}

import Task exposing (Task)


{-| Opaque type for storing state of tasks.
-}
type State2 msg a b
    = State2 (a -> b -> msg) (Maybe a) (Maybe b)


{-| -}
type State3 msg a b c
    = State3 (a -> b -> c -> msg) (Maybe a) (Maybe b) (Maybe c)


{-| -}
type State4 msg a b c d
    = State4 (a -> b -> c -> d -> msg) (Maybe a) (Maybe b) (Maybe c) (Maybe d)


{-| -}
type State5 msg a b c d e
    = State5 (a -> b -> c -> d -> e -> msg) (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e)


{-| -}
type State6 msg a b c d e f
    = State6 (a -> b -> c -> d -> e -> f -> msg) (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e) (Maybe f)


{-| -}
type State7 msg a b c d e f g
    = State7 (a -> b -> c -> d -> e -> f -> g -> msg) (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e) (Maybe f) (Maybe g)


{-| -}
type State8 msg a b c d e f g h
    = State8 (a -> b -> c -> d -> e -> f -> g -> h -> msg) (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e) (Maybe f) (Maybe g) (Maybe h)


{-| -}
type State9 msg a b c d e f g h i
    = State9 (a -> b -> c -> d -> e -> f -> g -> h -> i -> msg) (Maybe a) (Maybe b) (Maybe c) (Maybe d) (Maybe e) (Maybe f) (Maybe g) (Maybe h) (Maybe i)


{-| Opaque type for updating state of tasks.
-}
type Msg2 a b
    = LoadedA2 a
    | LoadedB2 b


{-| -}
type Msg3 a b c
    = LoadedA3 a
    | LoadedB3 b
    | LoadedC3 c


{-| -}
type Msg4 a b c d
    = LoadedA4 a
    | LoadedB4 b
    | LoadedC4 c
    | LoadedD4 d


{-| -}
type Msg5 a b c d e
    = LoadedA5 a
    | LoadedB5 b
    | LoadedC5 c
    | LoadedD5 d
    | LoadedE5 e


{-| -}
type Msg6 a b c d e f
    = LoadedA6 a
    | LoadedB6 b
    | LoadedC6 c
    | LoadedD6 d
    | LoadedE6 e
    | LoadedF6 f


{-| -}
type Msg7 a b c d e f g
    = LoadedA7 a
    | LoadedB7 b
    | LoadedC7 c
    | LoadedD7 d
    | LoadedE7 e
    | LoadedF7 f
    | LoadedG7 g


{-| -}
type Msg8 a b c d e f g h
    = LoadedA8 a
    | LoadedB8 b
    | LoadedC8 c
    | LoadedD8 d
    | LoadedE8 e
    | LoadedF8 f
    | LoadedG8 g
    | LoadedH8 h


{-| -}
type Msg9 a b c d e f g h i
    = LoadedA9 a
    | LoadedB9 b
    | LoadedC9 c
    | LoadedD9 d
    | LoadedE9 e
    | LoadedF9 f
    | LoadedG9 g
    | LoadedH9 h
    | LoadedI9 i


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
        = TaskStateUpdated (Task.Parallel.Msg2 String Int)
        | OneTaskFailed Http.Error
        | AllTasksCompleted String Int

    doTask : ( Task.Parallel.State2 Msg String Int, Cmd Msg )
    doTask =
        attempt2
            { task1 = fetchString
            , task2 = fetchInt
            , onUpdates = TaskStateUpdated
            , onFailure = OneTaskFailed
            , onSuccess = AllTasksCompleted
            }

-}
attempt2 :
    { task1 : Task x a
    , task2 : Task x b
    , onUpdates : Msg2 a b -> msg
    , onSuccess : a -> b -> msg
    , onFailure : x -> msg
    }
    -> ( State2 msg a b, Cmd msg )
attempt2 { task1, task2, onUpdates, onSuccess, onFailure } =
    ( State2 onSuccess Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA2) onFailure
      , task2 |> routeTo (onUpdates << LoadedB2) onFailure
      ]
        |> Cmd.batch
    )


{-| -}
attempt3 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , onUpdates : Msg3 a b c -> msg
    , onSuccess : a -> b -> c -> msg
    , onFailure : x -> msg
    }
    -> ( State3 msg a b c, Cmd msg )
attempt3 { task1, task2, task3, onUpdates, onSuccess, onFailure } =
    ( State3 onSuccess Nothing Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA3) onFailure
      , task2 |> routeTo (onUpdates << LoadedB3) onFailure
      , task3 |> routeTo (onUpdates << LoadedC3) onFailure
      ]
        |> Cmd.batch
    )


{-| -}
attempt4 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , task4 : Task x d
    , onUpdates : Msg4 a b c d -> msg
    , onSuccess : a -> b -> c -> d -> msg
    , onFailure : x -> msg
    }
    -> ( State4 msg a b c d, Cmd msg )
attempt4 { task1, task2, task3, task4, onUpdates, onSuccess, onFailure } =
    ( State4 onSuccess Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA4) onFailure
      , task2 |> routeTo (onUpdates << LoadedB4) onFailure
      , task3 |> routeTo (onUpdates << LoadedC4) onFailure
      , task4 |> routeTo (onUpdates << LoadedD4) onFailure
      ]
        |> Cmd.batch
    )


{-| -}
attempt5 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , task4 : Task x d
    , task5 : Task x e
    , onUpdates : Msg5 a b c d e -> msg
    , onSuccess : a -> b -> c -> d -> e -> msg
    , onFailure : x -> msg
    }
    -> ( State5 msg a b c d e, Cmd msg )
attempt5 { task1, task2, task3, task4, task5, onUpdates, onSuccess, onFailure } =
    ( State5 onSuccess Nothing Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA5) onFailure
      , task2 |> routeTo (onUpdates << LoadedB5) onFailure
      , task3 |> routeTo (onUpdates << LoadedC5) onFailure
      , task4 |> routeTo (onUpdates << LoadedD5) onFailure
      , task5 |> routeTo (onUpdates << LoadedE5) onFailure
      ]
        |> Cmd.batch
    )

{-| -}
attempt6 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , task4 : Task x d
    , task5 : Task x e
    , task6 : Task x f
    , onUpdates : Msg6 a b c d e f -> msg
    , onSuccess : a -> b -> c -> d -> e -> f -> msg
    , onFailure : x -> msg
    }
    -> ( State6 msg a b c d e f, Cmd msg )
attempt6 { task1, task2, task3, task4, task5, task6, onUpdates, onSuccess, onFailure } =
    ( State6 onSuccess Nothing Nothing Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA6) onFailure
      , task2 |> routeTo (onUpdates << LoadedB6) onFailure
      , task3 |> routeTo (onUpdates << LoadedC6) onFailure
      , task4 |> routeTo (onUpdates << LoadedD6) onFailure
      , task5 |> routeTo (onUpdates << LoadedE6) onFailure
      , task6 |> routeTo (onUpdates << LoadedF6) onFailure
      ]
        |> Cmd.batch
    )


{-| -}
attempt7 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , task4 : Task x d
    , task5 : Task x e
    , task6 : Task x f
    , task7 : Task x g
    , onUpdates : Msg7 a b c d e f g -> msg
    , onSuccess : a -> b -> c -> d -> e -> f -> g -> msg
    , onFailure : x -> msg
    }
    -> ( State7 msg a b c d e f g, Cmd msg )
attempt7 { task1, task2, task3, task4, task5, task6, task7, onUpdates, onSuccess, onFailure } =
    ( State7 onSuccess Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA7) onFailure
      , task2 |> routeTo (onUpdates << LoadedB7) onFailure
      , task3 |> routeTo (onUpdates << LoadedC7) onFailure
      , task4 |> routeTo (onUpdates << LoadedD7) onFailure
      , task5 |> routeTo (onUpdates << LoadedE7) onFailure
      , task6 |> routeTo (onUpdates << LoadedF7) onFailure
      , task7 |> routeTo (onUpdates << LoadedG7) onFailure
      ]
        |> Cmd.batch
    )


{-| -}
attempt8 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , task4 : Task x d
    , task5 : Task x e
    , task6 : Task x f
    , task7 : Task x g
    , task8 : Task x h
    , onUpdates : Msg8 a b c d e f g h -> msg
    , onSuccess : a -> b -> c -> d -> e -> f -> g -> h -> msg
    , onFailure : x -> msg
    }
    -> ( State8 msg a b c d e f g h, Cmd msg )
attempt8 { task1, task2, task3, task4, task5, task6, task7, task8, onUpdates, onSuccess, onFailure } =
    ( State8 onSuccess Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA8) onFailure
      , task2 |> routeTo (onUpdates << LoadedB8) onFailure
      , task3 |> routeTo (onUpdates << LoadedC8) onFailure
      , task4 |> routeTo (onUpdates << LoadedD8) onFailure
      , task5 |> routeTo (onUpdates << LoadedE8) onFailure
      , task6 |> routeTo (onUpdates << LoadedF8) onFailure
      , task7 |> routeTo (onUpdates << LoadedG8) onFailure
      , task8 |> routeTo (onUpdates << LoadedH8) onFailure
      ]
        |> Cmd.batch
    )


{-| -}
attempt9 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , task4 : Task x d
    , task5 : Task x e
    , task6 : Task x f
    , task7 : Task x g
    , task8 : Task x h
    , task9 : Task x i
    , onUpdates : Msg9 a b c d e f g h i -> msg
    , onSuccess : a -> b -> c -> d -> e -> f -> g -> h -> i -> msg
    , onFailure : x -> msg
    }
    -> ( State9 msg a b c d e f g h i, Cmd msg )
attempt9 { task1, task2, task3, task4, task5, task6, task7, task8, task9, onUpdates, onSuccess, onFailure } =
    ( State9 onSuccess Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    , [ task1 |> routeTo (onUpdates << LoadedA9) onFailure
      , task2 |> routeTo (onUpdates << LoadedB9) onFailure
      , task3 |> routeTo (onUpdates << LoadedC9) onFailure
      , task4 |> routeTo (onUpdates << LoadedD9) onFailure
      , task5 |> routeTo (onUpdates << LoadedE9) onFailure
      , task6 |> routeTo (onUpdates << LoadedF9) onFailure
      , task7 |> routeTo (onUpdates << LoadedG9) onFailure
      , task8 |> routeTo (onUpdates << LoadedH9) onFailure
      , task9 |> routeTo (onUpdates << LoadedI9) onFailure
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
        = DownloadUpdated (Task.Parallel.Msg2 Actor Film)
        | DownloadFailed Http.Error
        | DownloadCompleted Actor Film

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DownloadUpdated taskMsg ->
                let
                    ( nextTaskState, nextCmd ) =
                        Task.Parallel.update2 model.taskState taskMsg
                in
                ( { model | taskState = nextTaskState }, nextCmd )

            DownloadCompleted actor film ->
                ( { model | actor = actor, film = film, Cmd.none )

            DownloadFailed err ->
                ( { model | loadingError = Just err }, Cmd.none )

-}
update2 : State2 msg a b -> Msg2 a b -> ( State2 msg a b, Cmd msg )
update2 (State2 onSuccess a b) msg =
    let
        next a_ b_ =
            ( State2 onSuccess a_ b_, Maybe.map2 onSuccess a_ b_ |> toCmd )
    in
    case msg of
        LoadedA2 data ->
            next (Just data) b

        LoadedB2 data ->
            next a (Just data)


{-| -}
update3 : State3 msg a b c -> Msg3 a b c -> ( State3 msg a b c, Cmd msg )
update3 (State3 onSuccess a b c) msg =
    let
        next a_ b_ c_ =
            ( State3 onSuccess a_ b_ c_, Maybe.map3 onSuccess a_ b_ c_ |> toCmd )
    in
    case msg of
        LoadedA3 data ->
            next (Just data) b c

        LoadedB3 data ->
            next a (Just data) c

        LoadedC3 data ->
            next a b (Just data)


{-| -}
update4 : State4 msg a b c d -> Msg4 a b c d -> ( State4 msg a b c d, Cmd msg )
update4 (State4 onSuccess a b c d) msg =
    let
        next a_ b_ c_ d_ =
            ( State4 onSuccess a_ b_ c_ d_, Maybe.map4 onSuccess a_ b_ c_ d_ |> toCmd )
    in
    case msg of
        LoadedA4 data ->
            next (Just data) b c d

        LoadedB4 data ->
            next a (Just data) c d

        LoadedC4 data ->
            next a b (Just data) d

        LoadedD4 data ->
            next a b c (Just data)


{-| -}
update5 : State5 msg a b c d e -> Msg5 a b c d e -> ( State5 msg a b c d e, Cmd msg )
update5 (State5 onSuccess a b c d e) msg =
    let
        next a_ b_ c_ d_ e_ =
            ( State5 onSuccess a_ b_ c_ d_ e_, Maybe.map5 onSuccess a_ b_ c_ d_ e_ |> toCmd )
    in
    case msg of
        LoadedA5 data ->
            next (Just data) b c d e

        LoadedB5 data ->
            next a (Just data) c d e

        LoadedC5 data ->
            next a b (Just data) d e

        LoadedD5 data ->
            next a b c (Just data) e

        LoadedE5 data ->
            next a b c d (Just data)


{-| -}
update6 : State6 msg a b c d e f -> Msg6 a b c d e f -> ( State6 msg a b c d e f, Cmd msg )
update6 (State6 onSuccess a b c d e f) msg =
    let
        next a_ b_ c_ d_ e_ f_ =
            ( State6 onSuccess a_ b_ c_ d_ e_ f_
            , Maybe.map5
                (\a6 b6 c6 d6 e6 ->
                    Maybe.map (\f6 -> onSuccess a6 b6 c6 d6 e6 f6) f_
                )
                a_ b_ c_ d_ e_
                    |> Maybe.withDefault Nothing
                    |> toCmd
            )
    in
    case msg of
        LoadedA6 data ->
            next (Just data) b c d e f

        LoadedB6 data ->
            next a (Just data) c d e f

        LoadedC6 data ->
            next a b (Just data) d e f

        LoadedD6 data ->
            next a b c (Just data) e f

        LoadedE6 data ->
            next a b c d (Just data) f

        LoadedF6 data ->
            next a b c d e (Just data)

{-| -}
update7 : State7 msg a b c d e f g -> Msg7 a b c d e f g -> ( State7 msg a b c d e f g, Cmd msg )
update7 (State7 onSuccess a b c d e f g) msg =
    let
        next a_ b_ c_ d_ e_ f_ g_ =
            ( State7 onSuccess a_ b_ c_ d_ e_ f_ g_
            , Maybe.map5
                (\a7 b7 c7 d7 e7 ->
                    Maybe.map2 (\f7 g7 -> onSuccess a7 b7 c7 d7 e7 f7 g7) f_ g_
                )
                a_ b_ c_ d_ e_
                    |> Maybe.withDefault Nothing
                    |> toCmd
            )
    in
    case msg of
        LoadedA7 data ->
            next (Just data) b c d e f g

        LoadedB7 data ->
            next a (Just data) c d e f g

        LoadedC7 data ->
            next a b (Just data) d e f g

        LoadedD7 data ->
            next a b c (Just data) e f g

        LoadedE7 data ->
            next a b c d (Just data) f g

        LoadedF7 data ->
            next a b c d e (Just data) g

        LoadedG7 data ->
            next a b c d e f (Just data)


{-| -}
update8 : State8 msg a b c d e f g h -> Msg8 a b c d e f g h -> ( State8 msg a b c d e f g h, Cmd msg )
update8 (State8 onSuccess a b c d e f g h) msg =
    let
        next a_ b_ c_ d_ e_ f_ g_ h_ =
            ( State8 onSuccess a_ b_ c_ d_ e_ f_ g_ h_
            , Maybe.map5
                (\a8 b8 c8 d8 e8 ->
                    Maybe.map3 (\f8 g8 h8 -> onSuccess a8 b8 c8 d8 e8 f8 g8 h8) f_ g_ h_
                )
                a_ b_ c_ d_ e_
                    |> Maybe.withDefault Nothing
                    |> toCmd
            )
    in
    case msg of
        LoadedA8 data ->
            next (Just data) b c d e f g h

        LoadedB8 data ->
            next a (Just data) c d e f g h

        LoadedC8 data ->
            next a b (Just data) d e f g h

        LoadedD8 data ->
            next a b c (Just data) e f g h

        LoadedE8 data ->
            next a b c d (Just data) f g h

        LoadedF8 data ->
            next a b c d e (Just data) g h

        LoadedG8 data ->
            next a b c d e f (Just data) h

        LoadedH8 data ->
            next a b c d e f g (Just data)



{-| -}
update9 : State9 msg a b c d e f g h i -> Msg9 a b c d e f g h i -> ( State9 msg a b c d e f g h i, Cmd msg )
update9 (State9 onSuccess a b c d e f g h i) msg =
    let
        next a_ b_ c_ d_ e_ f_ g_ h_ i_ =
            ( State9 onSuccess a_ b_ c_ d_ e_ f_ g_ h_ i_
            , Maybe.map5
                (\a9 b9 c9 d9 e9 ->
                    Maybe.map4 (\f9 g9 h9 i9 -> onSuccess a9 b9 c9 d9 e9 f9 g9 h9 i9) f_ g_ h_ i_
                )
                a_ b_ c_ d_ e_
                    |> Maybe.withDefault Nothing
                    |> toCmd
            )
    in
    case msg of
        LoadedA9 data ->
            next (Just data) b c d e f g h i

        LoadedB9 data ->
            next a (Just data) c d e f g h i

        LoadedC9 data ->
            next a b (Just data) d e f g h i

        LoadedD9 data ->
            next a b c (Just data) e f g h i

        LoadedE9 data ->
            next a b c d (Just data) f g h i

        LoadedF9 data ->
            next a b c d e (Just data) g h i

        LoadedG9 data ->
            next a b c d e f (Just data) h i

        LoadedH9 data ->
            next a b c d e f g (Just data) i

        LoadedI9 data ->
            next a b c d e f g h (Just data)


{-| Opaque type for storing state of task lists.
-}
type ListState msg a
    = ListState (List a -> msg) (List (Maybe a))


{-| Opaque type for updating state of task lists.
-}
type ListMsg a
    = ItemLoaded Int a


{-| Attempt a list of tasks which will update when all the tasks have finished
or when one fails. Similar to a `Task.sequence` except in parallel.

    type Msg
        = DownloadUpdated (ListMsg String)
        | DownloadFailed Http.Error
        | DownloadCompleted (List String)

    fetchNames : ( ListState String, next )
    fetchNames =
        attemptList
            { tasks = [ fetchFirstName, fetchSecondName, fetchThirdName ]
            , onUpdates = DownloadUpdated
            , onFailure = DownloadFailed
            , onSuccess = DownloadCompleted
            }

-}
attemptList :
    { tasks : List (Task x a)
    , onUpdates : ListMsg a -> msg
    , onSuccess : List a -> msg
    , onFailure : x -> msg
    }
    -> ( ListState msg a, Cmd msg )
attemptList { tasks, onUpdates, onSuccess, onFailure } =
    ( tasks |> List.map (always Nothing) |> ListState onSuccess
    , tasks
        |> List.indexedMap
            (\index task ->
                task |> routeTo (onUpdates << ItemLoaded index) onFailure
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
                    ( nextTaskState, next ) =
                        Task.Parallel.updateList model.taskState taskMsg
                in
                ( { model | taskState = nextTaskState }, next )

            DownloadCompleted actors ->
                ( { model | actorList = actors, Cmd.none )

            DownloadFailed err ->
                ( { model | loadingError = Just err }, Cmd.none )

-}
updateList : ListState msg a -> ListMsg a -> ( ListState msg a, Cmd msg )
updateList (ListState onSuccess items) (ItemLoaded index newItem) =
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
        ( ListState onSuccess updatedItems, Cmd.none )

    else
        ( ListState onSuccess updatedItems
        , Just (onSuccess (updatedItems |> List.filterMap identity)) |> toCmd
        )



-- Internal


routeTo : (a -> msg) -> (x -> msg) -> Task x a -> Cmd msg
routeTo successMsg failureMsg =
    Task.andThen (Task.succeed << Result.Ok)
        >> Task.onError (Task.succeed << Result.Err)
        >> Task.perform
            (\result ->
                case result of
                    Ok a ->
                        successMsg a

                    Err err ->
                        failureMsg err
            )


toCmd : Maybe msg -> Cmd msg
toCmd =
    Maybe.map (Task.succeed >> Task.perform identity)
        >> Maybe.withDefault Cmd.none
