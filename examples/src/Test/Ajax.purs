module Test.Ajax where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div', h4', p', text)
import Concur.React.Props (onClick)
import Control.Alt ((<|>))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), JsonDecodeError)
import Data.Array (take)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)

-- Fetches posts from reddit json
newtype Post = Post
  { id :: String
  , title :: String
  }

type PostArray = Array Post

decodePostArray :: Json -> Either JsonDecodeError PostArray
decodePostArray json = decodeJson json >>= traverse decodeJson

instance decodeJsonPost :: DecodeJson Post where
  decodeJson json = do
    obj <- decodeJson json
    d <- obj .: "data"
    id <- d .: "id"
    title <- d .: "title"
    pure (Post { id, title })

subreddits :: Array String
subreddits = ["programming", "purescript", "haskell", "javascript"]

ajaxWidget :: forall a. Widget HTML a
ajaxWidget = div'
  [ p' [text "Click button to fetch posts from reddit"]
  , div' (map fetchReddit subreddits)
  ]

fetchReddit :: forall a. String -> Widget HTML a
fetchReddit sub = div'
  [ h4' [text ("/r/" <> sub)]
  , showPosts
  ]
  where
    showPosts = button [onClick] [text "Fetch posts"] >>= \_ -> fetchPosts
    fetchPosts = do
      let url = "https://www.reddit.com/r/" <> sub <> ".json"
      liftEffect (log ("Fetching posts from subreddit - " <> sub))
      result <- (liftAff (AX.get ResponseFormat.json url)) <|> (text "Loading...")
      case result of
        Left err -> text $ "GET " <> url <> " response failed to decode: " <> AX.printError err
        Right response -> do
          let postsResp = do
                o <- decodeJson response.body
                d1 <- o .: "data"
                cs <- d1 .: "children"
                decodePostArray cs
          case postsResp of
            Left err -> text ("Error: " <> show err)
            Right posts -> do
              div'
                [ div' (map (\(Post p) -> div' [text p.title]) (take 5 posts))
                , div' [button [unit <$ onClick] [text "Refresh"]]
                ]
              fetchPosts
