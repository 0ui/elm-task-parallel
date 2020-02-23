# elm-task-parallel

Run tasks in parallel when you only need the results if every task finishes
successfully, similar to `Promise.all()` in Javascript.

See the [examples folder](https://github.com/0ui/elm-task-parallel/examples) for
fully working samples.

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
tasks. It will return a tuple with some internal state and a command. 

```
( fetchState, fetchCmd ) =
        attempt2 RequestsUpdated Api.fetchUser Api.fetchOptions
```

Store the state and pass along the command. Your model will need to save a
`State` type matching your results with `()` in the unused "slots" of the
maximum 5 tasks. 

```
State User Options () () ()
```

The message you passed in to the helper function will need to accept a
`TaskMsg` with a similar type annotation.

```
type Msg
    = RequestsUpdated (TaskMsg Http.Error User Options () () ())
```

and finally your update function will only need to handle three cases
- Internal updates where you again store the state and pass the command along
- The success case where all of the tasks have successfully completed
- The error case of the first task to fail


```
case msg of
    RequestsUpdated internalMsg ->
        let
            ( nextState, nextCmd ) =
                Task.Parallel.update
                    model.internalState internalMsg (Expect2 RequestsFinished) RequestFailed
        in
        ( { model | internalState = nextState }, nextCmd )

    RequestsFinished user options ->
        ( { model | user = user, options = options }, Cmd.none )
        
    RequestFailed err ->
        ( { model | maybeError = Just err, Cmd.none )

```

## Caveats

- If the tasks have different result types, you're limited to 5 tasks (mirroring
`Maybe.map`). For HTTP requests, this is a limit I haven't run into yet. For
lists of tasks, there is no limit 