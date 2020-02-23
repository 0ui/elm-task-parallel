module Api exposing
    ( fetchPhoto
    , fetchPost
    , fetchPostById
    , fetchComments
    , httpErrorString
    , fetchTodo
    , Photo
    , Post
    , Comment
    , Todo
    )

import Http exposing (Error(..))
import Json.Decode exposing (Decoder, bool, int, list, map4, map5, string, field)
import Task exposing (Task)

getTask : String -> Decoder a -> Task Error a
getTask path decoder =
    Http.task
        { method = "get"
        , headers = []
        , url = path
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse decoder
        , timeout = Nothing
        }

fetchPhoto : Task Error Photo
fetchPhoto = 
    getTask "https://jsonplaceholder.typicode.com/photos/1" photoDecoder

fetchPost : Task Error Post
fetchPost = 
    getTask "https://jsonplaceholder.typicode.com/posts/1" postDecoder

fetchPostById : Int -> Task Error Post
fetchPostById id = 
    getTask ("https://jsonplaceholder.typicode.com/posts/" ++ (String.fromInt id)) postDecoder

fetchTodo : Task Error Todo
fetchTodo = 
    getTask "https://jsonplaceholder.typicode.com/todos/12" todoDecoder

fetchComments : Task Error (List Comment)
fetchComments = 
    getTask "https://jsonplaceholder.typicode.com/posts/1/comments" (list commentDecoder)

type alias Post =
    { userId : Int
    , id : Int
    , title : String
    , body : String
    }

postDecoder : Decoder Post
postDecoder =
    map4 Post
        (field "userId" int)
        (field "id" int)
        (field "title" string)
        (field "body" string)

type alias Todo =
    { userId : Int
    , id : Int
    , title : String
    , completed : Bool
    }

todoDecoder : Decoder Todo
todoDecoder =
    map4 Todo
        (field "userId" int)
        (field "id" int)
        (field "title" string)
        (field "completed" bool)

type alias Comment =
    { postId : Int
    , id : Int
    , name : String
    , email : String
    , body : String
    }

commentDecoder : Decoder Comment
commentDecoder =
    map5 Comment
        (field "postId" int)
        (field "id" int)
        (field "name" string)
        (field "email" string)
        (field "body" string)

type alias Photo =
    { albumId : Int
    , id : Int
    , title : String
    , url : String
    , thumbnailUrl : String
    }

photoDecoder : Decoder Photo
photoDecoder =
    map5 Photo
        (field "albumId" int)
        (field "id" int)
        (field "title" string)
        (field "url" string)
        (field "thumbnailUrl" string)


handleJsonResponse : Decoder a -> Http.Response String -> Result Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result

httpErrorString : Error -> String
httpErrorString error =
    case error of
        BadUrl url ->
            "Bad Url: " ++ url

        Timeout ->
            "Http Timeout"

        NetworkError ->
            "Network Error"

        BadStatus response ->
            "Bad response from server: " ++ String.fromInt response

        BadBody message ->
            message