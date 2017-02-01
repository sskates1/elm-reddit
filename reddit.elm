import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import List.Extra


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
    entries : List Entry
  }

init :(Model, Cmd Msg)
init =
  (Model (Subreddit "surfing") [], Cmd.none)


type Msg =
  NewSubReddit (Result Http.Error (List Entry))
  | GetSubReddit
  | UpdateSubReddit String
  | Vote VoteStatus Entry

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

    Vote vote entry ->
      (model, Cmd.none)

type VoteStatus =
  UpVote
  | DownVote
  | NullVote

vote : VoteStatus -> Entry -> Model -> Model
vote vote entry model =
  let entries = List.Extra.updateIf (\x -> entry.id == x.id) (sendVote vote) model.entries
  in {model | entries = entries }


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
    ,div [] <| List.map entryView model.entries

  ]

entryView : Entry -> Html Msg
entryView entry =
  div [] [
    div [] [ button [onClick <| Vote UpVote entry] [text "upvote"]
      , button [onClick <| Vote DownVote entry] [text "downvote"]]
    , div [] [ text <| toString entry.score]
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


decodeSubReddit : Json.Decoder (List Entry)
decodeSubReddit =
  Json.at ["data", "children" ] (Json.list decodeEntry)

decodeEntry : Json.Decoder Entry
decodeEntry =
  Json.map6 Entry
    (Json.at ["data","title"] Json.string)
    (Json.at ["data","score"] Json.int )
    (Json.at ["data","url"] Json.string )
    (Json.at ["data","permalink"] Json.string)
    (Json.succeed NullVote)
    (Json.at ["data", "id"] Json.string)

sendVote : VoteStatus -> Entry -> Entry
sendVote vote entry =
  if vote == entry.voteStatus then
    { entry | voteStatus = NullVote }
  else
    {entry | voteStatus = vote}



type alias Entry =
  { title : String,
    score : Int,
    url : String,
    permalink : String,
    voteStatus : VoteStatus,
    id : String
  }

type alias Subreddit =
  { name : String
  }
