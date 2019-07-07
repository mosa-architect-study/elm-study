import Browser
import Html exposing (Html, button, div, text, img )
import Html.Events exposing (onClick)
import Html.Attributes exposing (src,style)
import Random
import Maybe
import Random.List as RandomList
import List.Extra as ListExtra

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- モデル
type alias CardValue = {mark:Mark,number:Int}
type Mark = H | S | C | D
type Card = Joker | NomalCard CardValue
type alias Model = (List Card,List Card)
type Msg = Draw | ShuffleResult (List Card)

-- カードセットを初期化
trump : List Card
trump = ListExtra.lift2 CardValue [H,S,C,D] (List.range 1 13) |> List.map NomalCard |> List.append [Joker,Joker]

-- init
init : () -> (Model, Cmd Msg)
init _ = (([],[]), Random.generate ShuffleResult (RandomList.shuffle trump))

-- update
update : Msg -> Model -> (Model, Cmd Msg)
update msg (deck,discard) = 
    case msg of
        ShuffleResult suffuled -> ((suffuled,discard),Cmd.none)
        Draw -> ((deck,discard) |> move |> Maybe.withDefault (deck,discard),Cmd.none)

-- fromのリストから1枚をtoのリストに移す。Maybeなのはtoのリストが空の時
move : (List a,List a) -> Maybe (List a,List a)
move (from,to) = Maybe.map2 (\head -> \tail -> (tail,head :: to)) (List.head from) (List.tail from)

-- view
view : Model -> Html Msg
view (deck,discard) =
  div [] [
      button [onClick Draw] [text "ドロー"],
      div [] (List.map cardView discard)
  ]

cardView : Card -> Html Msg
cardView card = 
    img [card |> cardToChicodezaUrl |> src,style "width" "calc(100% / 13)" ] []
 
markToText : Mark -> String
markToText mark = 
    case mark of 
        D -> "ダイヤ"
        C -> "クローバー"
        S -> "スペード"
        H -> "ハート"

cardToChicodezaUrl : Card -> String
cardToChicodezaUrl card = 
    let 
        markIndex : Mark -> Int
        markIndex mark = 
            case mark of 
                S -> 0
                C -> 1
                D -> 2
                H -> 3

        cardIndex :  Int
        cardIndex = 
            case card of 
                Joker -> 53
                NomalCard {mark,number} -> (markIndex mark) * 13 + number
    in
        "https://chicodeza.com/wordpress/wp-content/uploads/torannpu-illust" ++ (String.fromInt cardIndex) ++  ".png"


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none