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
( internalState, fetchCmd ) =
        attempt5
            RequestsUpdated
            Api.fetchUser
            Api.fetchOptions
            Api.fetchLocations
            Api.fetchChat
            Time.now
```

Store the state and pass along the command. Your model will need to store a
`State` type matching the number of your tasks.

```elm
State5 User Options Locations Chat Time.Posix
```

The message you passed in to the helper function will need to accept an internal
`Msg` with a similar type annotation, but including the error type to handle.

```elm
type Msg
    = RequestsUpdated (Msg5 Http.Error User Options Locations Chat Time.Posix)
    | RequestsFinished User Options Locations Chat Time.Posix
    | RequestFailed Http.Error
```

and finally your update function will only need to handle three cases
- Internal updates where you again store the state and pass the command along
- The success case where all of the tasks have successfully completed
- The error case of the first task to fail


```elm
case msg of
    RequestsUpdated internalMsg ->
        let
            ( nextState, nextCmd ) =
                update5 model.internalState internalMsg RequestsFinished RequestFailed
        in
        ( { model | internalState = nextState }, nextCmd )

    RequestsFinished user options locations chat time ->
        ( { model | user = user, options = , locations = locations, chat = chat, currentTime = time }, Cmd.none )
        
    RequestFailed err ->
        ( { model | maybeError = Just err, Cmd.none )

```

## Caveats

- If the tasks have different result types, you're limited to 5 tasks (mirroring
`Maybe.map`). For HTTP requests, this is a limit I haven't run into yet. For
lists of tasks, there is no limit.
- Updating the internal state of this library adds one case to your update
function, however in the case of 5 tasks you could already have 10 cases
just to update those + a completion check. This library limits those to just
three.