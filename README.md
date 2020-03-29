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

Instead of using `Task.attempt`, use one of the helper functions to run up to 5
tasks of different result types (or a list of the same type). It will return a
tuple with some internal state and a command.

```elm
import Task.Parallel as Parallel

init : () -> ( Model, Cmd Msg )
init _ =
    Parallel.attempt5
        { task1 = Api.getUser
        , task2 = Api.getOptions
        , task3 = Api.getMaps
        , task4 = Api.getChat
        , task5 = Time.now
        , onFailure = OneFailed
        , onSuccess = Parallel.OnSuccess5 AllFinished
        , onUpdates = Updated
        , toModel = Loading
        }
```

Store the state and pass along the command. Your model will need to store a
[State](https://package.elm-lang.org/packages/0ui/elm-task-parallel/latest/Task-Parallel#State2)
type matching the number of your tasks.

```elm
type Model
    = Loading (Parallel.State5 Model Msg Error User Options Maps Chat Posix)
    | Success User Options Maps Chat Posix
    | Failure Error
```

The message you passed in to configure your tasks will need to handle internal updates via a
[Msg](https://package.elm-lang.org/packages/0ui/elm-task-parallel/latest/Task-Parallel#Msg2)
with a similar type annotation except including the Msg type of your module and the error type to handle.

```elm
type Msg
    = Updated (Parallel.Msg5 Error User Options Maps Chat Posix)
    | AllFinished User Options Maps Chat Posix
    | OneFailed Error
```

and finally your update function will only need to handle three cases
- Internal updates where you again store the state and pass the command along
- The success case where all of the tasks have successfully completed
- The error case of the first task to fail


```elm
case msg of
    Updated taskMsg ->
        Parallel.update5 taskMsg taskState

    AllFinished user options maps chat time ->
        ( Success user options maps chat time, Cmd.none )
        
    OneFailed err ->
        ( Failure err , Cmd.none )

```

## Caveats

- If the tasks have different result types, you're limited to 5 tasks (mirroring
`Maybe.map`). For HTTP requests, this is a limit I haven't run into yet. For
lists of tasks, there is no limit.
- Updating the internal state of this library adds one case to your update
function, however in the case of 5 tasks you could already have 10 cases
just to update those + a completion check. This library limits those to just
three.