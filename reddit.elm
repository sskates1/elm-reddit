import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Array exposing (Array)



main =
  Html.program
    { init = init "surfing"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { topic : String,
    entries : Array Entry
  }

init : String -> (Model, Cmd Msg)
init topic =
  (Model topic Array.empty, Cmd.none)


type Msg =
  NewSubReddit (Result Http.Error (Array Entry))
  | GetSubReddit

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewSubReddit (Ok json) ->
      ( { model | entries = json  }, Cmd.none)

    NewSubReddit (Err e) ->
      (Debug.log (toString e) model, Cmd.none)

    GetSubReddit ->
      (model, getSubRedditInfo model.topic)


-- VIEW

view : Model -> Html Msg
view model =
  div [][
    div [][
      -- input [placeholder "Go to Subreddit", onClick GetSubReddit] []
      button [onClick GetSubReddit] [text "Submit"]
    ]
    ,h2 [] [text model.topic]
    ,  h2 [] [text <| "https://www.reddit.com/r/" ++ model.topic]
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
  -- div []
  --   [ h2 [] [text model.topic]
  --   , img [src model.gifUrl] []
  --   , div []
  --     [ button [ onClick MorePlease ] [ text "More Please!" ] ]
  --   ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

getSubRedditInfo : String -> Cmd Msg
getSubRedditInfo topic =
  let
    url = "https://www.reddit.com/r/" ++ topic ++ "/.json"

    request =
      Http.get url decodeSubReddit

  in
    Http.send NewSubReddit request

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
