import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Array exposing (Array)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { subreddit : Subreddit,
    entries : Array Entry
  }

init :(Model, Cmd Msg)
init =
  (Model (Subreddit "surfing") Array.empty, Cmd.none)


type Msg =
  NewSubReddit (Result Http.Error (Array Entry))
  | GetSubReddit
  | UpdateSubReddit String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateSubReddit  name ->
      ({model | subreddit = (updateSubRedditSelection name)}, Cmd.none)
    NewSubReddit (Ok json) ->
      ( { model | entries = json  }, Cmd.none)

    NewSubReddit (Err e) ->
      (Debug.log (toString e) model, Cmd.none)

    GetSubReddit ->
      (model, getSubRedditInfo model.subreddit.name)


-- VIEW

view : Model -> Html Msg
view model =
  div [][
    div [][
      input [ type_ "text", placeholder "Go to Subreddit", onInput UpdateSubReddit] []
      , button [onClick GetSubReddit] [text "Submit"]
    ]
    ,h2 [] [text model.subreddit.name]
    ,  h2 [] [text <| "https://www.reddit.com/r/" ++ model.subreddit.name]
    ,div [] <| Array.toList <| Array.map entryView model.entries

  ]

entryView : Entry -> Html Msg
entryView entry =
  div [] [
    div [] [ text <| toString entry.score]
    ,a [href entry.url] [ text entry.title]
    , div [] [
      a [href <| "https://www.reddit.com"++entry.permalink ] [ text "comments"]
    ]
  ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getSubRedditInfo : String -> Cmd Msg
getSubRedditInfo subredditName =
  let
    url = "https://www.reddit.com/r/" ++ subredditName ++ "/.json"

    request =
      Http.get url decodeSubReddit

  in
    Http.send NewSubReddit request

updateSubRedditSelection: String -> Subreddit
updateSubRedditSelection subredditName =
  Subreddit subredditName


decodeSubReddit : Json.Decoder (Array Entry)
decodeSubReddit =
  Json.at ["data", "children" ] (Json.array decodeEntry)

decodeEntry : Json.Decoder Entry
decodeEntry =
  Json.map4 Entry
    (Json.at ["data","title"] Json.string)
    (Json.at ["data","score"] Json.int )
    (Json.at ["data","url"] Json.string )
    (Json.at ["data","permalink"] Json.string )

type alias Entry =
  { title : String,
    score : Int,
    url : String,
    permalink : String
  }

type alias Subreddit =
  { name : String
  }
