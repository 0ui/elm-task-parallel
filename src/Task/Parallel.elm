module Task.Parallel exposing
    ( attempt2, attempt3, attempt4, attempt5, attemptList
    , attempt
    , update2, update3, update4, update5, updateList
    , State2, State3, State4, State5, Msg2, Msg3, Msg4, Msg5, StateList, MsgList
    , Success2(..), Success3(..), Success4(..), Success5(..), SuccessList(..)
    , UpdateModel(..)
    )

{-| This library helps you run tasks in parallel when you only need the results
if every task finishes successfully, similar to `Promise.all()` in Javascript. A
good use case is handling the result of multiple HTTP requests.


## Task Helpers

Functions for parallelizing tasks of different result types.

@docs attempt2, attempt3, attempt4, attempt5


## Routing

Route the results of your tasks to a success message or to another task.

@docs Success2, Success3, Success4, Success5

## Update

You will have to pass internal messages and commands along in your update
function in order to eventually get your results.

@docs update2, update3, update4, update5

## List Helpers

Functions for dealing with lists of tasks. This is helpful if all of your tasks
are of the same type.

@docs attemptList, updateList, SuccessList


## Less Common Helpers

@docs attempt

## Opaque Types

These are the types you will have to annotate your Model and Msg with in order to
track the state of parallel tasks. These require the error type and result types of
all of your tasks as well as the Msg type from your module that it will be sending.
See [update2](#update2) for an example

@docs State2, State3, State4, State5, Msg2, Msg3, Msg4, Msg5, StateList, MsgList

-}

import Task exposing (Task)

{-|

`Success2` accepts a message to sent the results to

    Parallel.attempt2
        { task1 = Api.fetchPost
        , task2 = Api.fetchComments
        , onFailure = DownloadFailed
        , onSuccess = Parallel.Success2 DownloadFinished
        , onUpdates = DownloadUpdated
        , updateModel = Loading
        }

`Success2AndThen` accepts a message that creates the next set of tasks based
  on your results

    Parallel.attempt2
        { task1 = Api.fetchPost
        , task2 = Api.fetchComments
        , onFailure = DownloadFailed
        , onSuccess = Parallel.Success2AndThen (\post comments ->
            doMoreTasks post comments
        )
        , onUpdates = DownloadUpdated
        , updateModel = Loading
        }

-}

type Success2 model msg a b
    = Success2 (a -> b -> msg)
    | Success2AndThen (a -> b -> ( model, Cmd msg ))


{-|-}
type Success3 model msg a b c
    = Success3 (a -> b -> c -> msg)
    | Success3AndThen (a -> b -> c -> ( model, Cmd msg ))


{-|-}
type Success4 model msg a b c d
    = Success4 (a -> b -> c -> d -> msg)
    | Success4AndThen (a -> b -> c -> d -> ( model, Cmd msg ))


{-|-}
type Success5 model msg a b c d e
    = Success5 (a -> b -> c -> d -> e -> msg)
    | Success5AndThen (a -> b -> c -> d -> e -> ( model, Cmd msg ))


{-|-}
type SuccessList model msg a
    = SuccessList (List a -> msg)
    | SuccessListAndThen (List a -> ( model, Cmd msg ))

type UpdateModel state model
    = UpdateModel (state -> model -> model) (state -> model)
    | UpdateComplete (state -> model)

{-| Opaque type for storing state of tasks.
-}
type State2 model msg x a b
    = FailedState2
    | State2
        { a : Maybe a
        , b : Maybe b
        , onFailure : x -> msg
        , onSuccess : Success2 model msg a b
        , updateModel : UpdateModel (State2 model msg x a b) model
        }


{-| -}
type State3 model msg x a b c
    = FailedState3
    | State3
        { a : Maybe a
        , b : Maybe b
        , c : Maybe c
        , onFailure : x -> msg
        , onSuccess : Success3 model msg a b c
        , updateModel : UpdateModel (State3 model msg x a b c) model
        }


{-| -}
type State4 model msg x a b c d
    = FailedState4
    | State4
        { a : Maybe a
        , b : Maybe b
        , c : Maybe c
        , d : Maybe d
        , onFailure : x -> msg
        , onSuccess : Success4 model msg a b c d
        , updateModel : UpdateModel (State4 model msg x a b c d) model
        }


{-| -}
type State5 model msg x a b c d e
    = FailedState5
    | State5
        { a : Maybe a
        , b : Maybe b
        , c : Maybe c
        , d : Maybe d
        , e : Maybe e
        , onFailure : x -> msg
        , onSuccess : Success5 model msg a b c d e
        , updateModel : UpdateModel (State5 model msg x a b c d e) model
        }


{-| Opaque type for storing state of task lists.
-}
type StateList model msg x a
    = StateListFailed
    | StateList
        { list : List (Maybe a)
        , onFailure : x -> msg
        , onSuccess : SuccessList model msg a
        , updateModel : UpdateModel (StateList model msg x a) model
        }

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



{-| Opaque type for updating state of task lists.
-}
type MsgList x a
    = ItemLoaded Int a
    | ItemFailed x



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


{-| Attempt two tasks in parallel which will send an update when either all
tasks finish successfully or one fails. 

    type Msg
        = TaskUpdated (Task.Parallel.Msg2 Model Msg Http.Error String Int)
        | OneFailed Http.Error
        | AllFinished String Int

    doTask : ( Model, Cmd Msg )
    doTask =
        Parallel.attempt2
            { task1 = Api.getUser
            , task2 = Api.getOptions
            , onFailure = OneFailed
            , onSuccess = Parallel.OnSuccess2 AllFinished
            , onUpdates = TasUpdated
            , updateModel = LoadingState
            }

-}
attempt2 :
    { task1 : Task x a
    , task2 : Task x b
    , onFailure : x -> msg
    , onSuccess : Success2 model msg a b
    , onUpdates : Msg2 x a b -> msg
    , updateModel : UpdateModel (State2 model msg x a b) model
    }
    -> ( model, Cmd msg )
attempt2 config =
    ( getInitialFunction config.updateModel <| State2
        { a = Nothing
        , b = Nothing
        , onFailure = config.onFailure
        , onSuccess = config.onSuccess
        , updateModel = config.updateModel
        }
    , [ config.task1 |> routeTo (config.onUpdates << LoadedA2) (config.onUpdates << FailedToLoad2)
      , config.task2 |> routeTo (config.onUpdates << LoadedB2) (config.onUpdates << FailedToLoad2)
      ]
        |> Cmd.batch
    )


{-| -}
attempt3 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , onFailure : x -> msg
    , onSuccess : Success3 model msg a b c
    , onUpdates : Msg3 x a b c -> msg
    , updateModel : UpdateModel (State3 model msg x a b c) model
    }
    -> ( model, Cmd msg )
attempt3 config =
    ( getInitialFunction config.updateModel <| State3
        { a = Nothing
        , b = Nothing
        , c = Nothing
        , onFailure = config.onFailure
        , onSuccess = config.onSuccess
        , updateModel = config.updateModel
        }
    , [ config.task1 |> routeTo (config.onUpdates << LoadedA3) (config.onUpdates << FailedToLoad3)
      , config.task2 |> routeTo (config.onUpdates << LoadedB3) (config.onUpdates << FailedToLoad3)
      , config.task3 |> routeTo (config.onUpdates << LoadedC3) (config.onUpdates << FailedToLoad3)
      ]
        |> Cmd.batch
    )

{-| -}
attempt4 :
    { task1 : Task x a
    , task2 : Task x b
    , task3 : Task x c
    , task4 : Task x d
    , onFailure : x -> msg
    , onSuccess : Success4 model msg a b c d
    , onUpdates : Msg4 x a b c d -> msg
    , updateModel : UpdateModel (State4 model msg x a b c d) model
    }
    -> ( model, Cmd msg )
attempt4 config =
    ( getInitialFunction config.updateModel <| State4
        { a = Nothing
        , b = Nothing
        , c = Nothing
        , d = Nothing
        , onFailure = config.onFailure
        , onSuccess = config.onSuccess
        , updateModel = config.updateModel
        }
    , [ config.task1 |> routeTo (config.onUpdates << LoadedA4) (config.onUpdates << FailedToLoad4)
      , config.task2 |> routeTo (config.onUpdates << LoadedB4) (config.onUpdates << FailedToLoad4)
      , config.task3 |> routeTo (config.onUpdates << LoadedC4) (config.onUpdates << FailedToLoad4)
      , config.task4 |> routeTo (config.onUpdates << LoadedD4) (config.onUpdates << FailedToLoad4)
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
    , onFailure : x -> msg
    , onSuccess : Success5 model msg a b c d e
    , onUpdates : Msg5 x a b c d e -> msg
    , updateModel : UpdateModel (State5 model msg x a b c d e) model
    }
    -> ( model, Cmd msg )
attempt5 config =
    ( getInitialFunction config.updateModel <| State5
        { a = Nothing
        , b = Nothing
        , c = Nothing
        , d = Nothing
        , e = Nothing
        , onFailure = config.onFailure
        , onSuccess = config.onSuccess
        , updateModel = config.updateModel
        }
    , [ config.task1 |> routeTo (config.onUpdates << LoadedA5) (config.onUpdates << FailedToLoad5)
      , config.task2 |> routeTo (config.onUpdates << LoadedB5) (config.onUpdates << FailedToLoad5)
      , config.task3 |> routeTo (config.onUpdates << LoadedC5) (config.onUpdates << FailedToLoad5)
      , config.task4 |> routeTo (config.onUpdates << LoadedD5) (config.onUpdates << FailedToLoad5)
      , config.task5 |> routeTo (config.onUpdates << LoadedE5) (config.onUpdates << FailedToLoad5)
      ]
        |> Cmd.batch
    )

{-| Handle updates for two tasks by calling `update2` inside of your main update
function to keep this library's internal state updated. If they have either all
finished successfully or one has failed, the corresponding message you provided
will be sent to your main `update` function. Maintain a copy of the returned
state to pass in on each subsequent `update`. This step is required with
`attempt[n]` functions.

    type Model
        = Loading (State2 Model Msg Http.Error Actor Film)
        | Loaded Actor Film
        | LoadFailure Http.Error

    type Msg
        = DownloadUpdated (Msg2 Http.Error Actor Film)
        | DownloadCompleted Actor Film
        | DownloadFailed Http.Error

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case model of
            Loading taskState ->
                case msg of
                    DownloadUpdated taskMsg ->
                        update2 taskMsg taskState

                    DownloadCompleted actor film ->
                        ( Loaded actor film, Cmd.none )

                    DownloadFailed err ->
                        ( LoadFailure err, Cmd.none )

            _ ->
                -- The rest of your update...
                ( model, Cmd.none )

-}
update2 : Msg2 x a b -> State2 model msg x a b -> model -> ( model, Cmd msg )
update2 msg state model =
    case state of
        FailedState2 ->
            ( model, Cmd.none )

        State2 loadingState ->
            let
                updateFunction = getUpdateFunction loadingState.updateModel model
                nextCmd nextLoading =
                    Maybe.map2 (\a b ->
                        case loadingState.onSuccess of
                            Success2 successMsg ->
                                ( updateFunction (State2 nextLoading), successMsg a b |> toCmd )

                            Success2AndThen task ->
                                task a b
                        )
                        nextLoading.a
                        nextLoading.b
                        |> Maybe.withDefault ( updateFunction (State2 nextLoading), Cmd.none )
            in
            case msg of
                LoadedA2 data ->
                    nextCmd { loadingState | a = Just data }

                LoadedB2 data ->
                    nextCmd { loadingState | b = Just data }

                FailedToLoad2 err ->
                    ( updateFunction FailedState2
                    , loadingState.onFailure err |> toCmd
                    )

{-| -}
update3 : Msg3 x a b c -> State3 model msg x a b c -> model -> ( model, Cmd msg )
update3 msg state model =
    case state of
        FailedState3 ->
            ( model, Cmd.none )

        State3 loadingState ->
            let
                updateFunction = getUpdateFunction loadingState.updateModel model
                nextCmd nextLoading =
                    Maybe.map3 (\a b c ->
                        case loadingState.onSuccess of
                            Success3 successMsg ->
                                ( updateFunction (State3 nextLoading), successMsg a b c |> toCmd )

                            Success3AndThen task ->
                                task a b c
                        )
                        nextLoading.a
                        nextLoading.b
                        nextLoading.c
                        |> Maybe.withDefault ( updateFunction (State3 nextLoading), Cmd.none )
            in
            case msg of
                LoadedA3 data ->
                    nextCmd { loadingState | a = Just data }

                LoadedB3 data ->
                    nextCmd { loadingState | b = Just data }

                LoadedC3 data ->
                    nextCmd { loadingState | c = Just data }

                FailedToLoad3 err ->
                    ( updateFunction FailedState3
                    , loadingState.onFailure err |> toCmd
                    )

{-| -}
update4 : Msg4 x a b c d -> State4 model msg x a b c d -> model -> ( model, Cmd msg )
update4 msg state model =
    case state of
        FailedState4 ->
            ( model, Cmd.none )

        State4 loadingState ->
            let
                updateFunction = getUpdateFunction loadingState.updateModel model
                nextCmd nextLoading =
                    Maybe.map4 (\a b c d ->
                        case loadingState.onSuccess of
                            Success4 successMsg ->
                                ( updateFunction (State4 nextLoading), successMsg a b c d |> toCmd )

                            Success4AndThen task ->
                                task a b c d
                        )
                        nextLoading.a
                        nextLoading.b
                        nextLoading.c
                        nextLoading.d
                        |> Maybe.withDefault ( updateFunction (State4 nextLoading), Cmd.none )
            in
            case msg of
                LoadedA4 data ->
                    nextCmd { loadingState | a = Just data }

                LoadedB4 data ->
                    nextCmd { loadingState | b = Just data }

                LoadedC4 data ->
                    nextCmd { loadingState | c = Just data }

                LoadedD4 data ->
                    nextCmd { loadingState | d = Just data }

                FailedToLoad4 err ->
                    ( updateFunction FailedState4
                    , loadingState.onFailure err |> toCmd
                    )

{-| -}
update5 : Msg5 x a b c d e -> State5 model msg x a b c d e -> model -> ( model, Cmd msg )
update5 msg state model =
    case state of
        FailedState5 ->
            ( model, Cmd.none )

        State5 loadingState ->
            let
                updateFunction = getUpdateFunction loadingState.updateModel model
                nextCmd nextLoading =
                    Maybe.map5 (\a b c d e ->
                        case loadingState.onSuccess of
                            Success5 successMsg ->
                                ( updateFunction (State5 nextLoading), successMsg a b c d e |> toCmd )

                            Success5AndThen task ->
                                task a b c d e
                        )
                        nextLoading.a
                        nextLoading.b
                        nextLoading.c
                        nextLoading.d
                        nextLoading.e
                        |> Maybe.withDefault ( updateFunction (State5 nextLoading), Cmd.none )
            in
            case msg of
                LoadedA5 data ->
                    nextCmd { loadingState | a = Just data }

                LoadedB5 data ->
                    nextCmd { loadingState | b = Just data }

                LoadedC5 data ->
                    nextCmd { loadingState | c = Just data }

                LoadedD5 data ->
                    nextCmd { loadingState | d = Just data }

                LoadedE5 data ->
                    nextCmd { loadingState | e = Just data }

                FailedToLoad5 err ->
                    ( updateFunction FailedState5
                    , loadingState.onFailure err |> toCmd
                    )

{-| Attempt a list of tasks which will update when all the tasks have finished
or when one fails. Similar to a `Task.sequence` except in parallel.

    type alias Model =
        { taskState : StateList Model Msg Http.Error Actor
        , actorList : Maybe (List Actor)
        , loadingError : Maybe Http.Error
        }

    fetchActors : ( Model, Cmd Msg )
    fetchActors =
        attemptList
            { tasks =
                [ Api.fetchPostById 1
                , Api.fetchPostById 2
                , Api.fetchPostById 8
                ]
            , onFailure = DownloadFailed
            , onSuccess = SuccessList AllFinished
            , onUpdates = DownloadUpdated
            , updateModel = (\nextState ->
                { model | taskState = nextState }
              )
            }

-}
attemptList :
    { tasks : List (Task x a)
    , onFailure : x -> msg
    , onSuccess : SuccessList model msg a
    , onUpdates : MsgList x a -> msg
    , updateModel : UpdateModel (StateList model msg x a) model
    }
    -> ( model, Cmd msg )
attemptList config =
    ( getInitialFunction config.updateModel <|
        StateList
            { list = config.tasks |> List.map (always Nothing)
            , onFailure = config.onFailure
            , onSuccess = config.onSuccess
            , updateModel = config.updateModel 
            }
    , config.tasks
        |> List.indexedMap
            (\index task ->
                task
                    |> routeTo (config.onUpdates << ItemLoaded index) (config.onUpdates << ItemFailed)
            )
        |> Cmd.batch
    )


{-| Call `updateList` inside of your main update function to check if the
tasks have failed or finished. Maintain a copy of the returned state to pass in
on each subsequent `updateList`. This step is required with
[`attemptList`](#attemptList).

    type Msg
        = DownloadUpdated (ListMsg Http.Error Actor)
        | DownloadFailed Http.Error
        | AllFinished (List Actor)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            DownloadUpdated taskMsg ->
                updateList taskMsg model.taskState

            DownloadFailed err ->
                ( { model | loadingError = Just err }
                , Cmd.none
                )

            AllFinished actors ->
                ( { model | actorList = Just actors
                , Cmd.none
                )
-}

updateList : MsgList x a -> StateList model msg x a -> model -> ( model, Cmd msg )
updateList msg prevState model =
    case prevState of
        StateListFailed ->
            ( model, Cmd.none )

        StateList loadingState ->
            let
                updateFunction = getUpdateFunction loadingState.updateModel model
            in
            case msg of
                ItemFailed err ->
                    ( updateFunction StateListFailed
                    , loadingState.onFailure err |> toCmd
                    )

                ItemLoaded index newItem ->
                    let
                        updatedState =
                            { loadingState | list = loadingState.list
                                |> List.indexedMap
                                    (\i maybeItem ->
                                        if i == index then
                                            Just newItem

                                        else
                                            maybeItem
                                    )
                            }
                    in
                    if List.any ((==) Nothing) updatedState.list then
                        ( updateFunction (StateList updatedState), Cmd.none )

                    else
                        let
                            loadedList =
                                updatedState.list |> List.filterMap identity
                        in
                        case loadingState.onSuccess of
                            SuccessList successMsg ->
                                ( updateFunction (StateList updatedState)
                                , loadedList |> successMsg |> toCmd
                                )

                            SuccessListAndThen task ->
                                task loadedList


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

getUpdateFunction : UpdateModel state model -> model -> (state -> model)
getUpdateFunction updateModel model =
    case updateModel of
        UpdateModel update _ ->
            (\newState -> update newState model)
        UpdateComplete update ->
            (\newState -> update newState) 

getInitialFunction : UpdateModel state model -> (state -> model)
getInitialFunction updateModel =
    case updateModel of
        UpdateModel _ initialModel ->
            initialModel
        UpdateComplete update ->
            update
