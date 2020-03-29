# elm-task-parallel

Run tasks in parallel and handle the results only if every task finishes
successfully, similar to `Promise.all()` in Javascript.

See the [examples folder](https://github.com/0ui/elm-task-parallel/tree/master/examples) for
full code examples.

## Motivation

It is common to have several tasks where the results only matter together. For
example, you may need to complete multiple HTTP requests before your page can be
rendered. In order to avoid running the tasks in sequence which is slower, you
typically have to

- Batch task commands together
- Handle each task's success case
- Handle each task's error case
- Check if each one is finished every time an individual task finishes

This library is designed to do that for you.

## How to use

Instead of using `Task.attempt`, use one of the helper functions to run up to 9
tasks of different result types (or a list of the same type). It will return a
tuple with some internal state and a command.

```elm
import Task.Parallel as Parallel

init : () -> ( Model, Cmd Msg )
init _ =
    doTasks |> Tuple.mapFirst PageLoading

doTasks : ( Parallel.State5 Msg User Options Locations Chat Time.Posix, Cmd Msg )
doTasks =
    ( taskState, taskCmd ) =
        Parallel.attempt5
            { task1 = Api.fetchUser
            , task2 = Api.fetchOptions
            , task3 = Api.fetchLocations
            , task4 = Api.fetchChat
            , task5 = Time.now
            , onUpdates = RequestsUpdated
            , onFailure = OneFailed
            , onSuccess = AllFinished
            }
```

Store the state and pass along the command. Your model will need to store a
`Parallel.State[n]` matching the number of your tasks and reference your `Msg`
type as well as the types of your tasks.

```elm
type Model
    = PageLoading (Parallel.State5 Msg User Options Locations Chat Time.Posix)
    | PageError Http.Error
    | PageLoaded User Options Locations Chat Time.Posix
```
The message you passed in to the helper function will need to accept an internal
`Parallel.Msg[n]` referencing only the types of the tasks.

```elm
type Msg
    = RequestsUpdated (Parallel.Msg5 User Options Locations Chat Time.Posix)
    | OneFailed Http.Error
    | AllFinished User Options Locations Chat Time.Posix
```

and finally your update function will only need to handle three cases
- Internal updates. Just call `Parallel.update[n]`, store the state, and pass
  the command along.
- The error case of one task failing.
- The success case where all of the tasks have successfully completed.


```elm
case msg of
    RequestsUpdated taskMsg ->
        Parallel.update5 taskState taskMsg
            |> Tuple.mapFirst PageLoading

    OneFailed err ->
        ( PageError err, Cmd.none )

    AllFinished user options locations chat time ->
        ( PageLoaded user options locations chat time, Cmd.none )
```

The usage of `Tuple.mapFirst` here is simply mapping the result
`( Parallel.State5, Cmd Msg )` the library produces into a `( Model, Cmd Msg )`
that your program is expecting.

## Caveats

- If the tasks have different result types, you're limited to 9 tasks.
For HTTP requests, this is a limit I haven't run into yet. For lists of tasks,
there is no limit.
- Updating the internal state of this library adds one case to your update
function, however in the case of 9 tasks you could already have 18 cases
just to update those + a completion check. This library limits those to just
three.