module Test.Ajax where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Concur.Core (Widget)
import Concur.Core.Dado as Da
import Concur.React (HTML)
import Concur.React.DOM (button, div_, h4_, p_, text)
import Concur.React.Props (onClick)
import Control.Alt ((<|>))
import Control.MultiAlternative (orr)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
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

decodePostArray :: Json -> Either String PostArray
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
ajaxWidget = div_ Da.do
  p_ $ text "Click button to fetch posts from reddit"
  div_ $ orr $ map fetchReddit subreddits

fetchReddit :: forall a. String -> Widget HTML a
fetchReddit sub = div_ Da.do
  h4_ $ text ("/r/" <> sub)
  showPosts
  where
    showPosts = button [onClick] (text "Fetch posts") *> fetchPosts
    fetchPosts = do
      let url = "https://www.reddit.com/r/" <> sub <> ".json"
      liftEffect (log ("Fetching posts from subreddit - " <> sub))
      resp <- liftAff (AX.get ResponseFormat.json url) <|> text "Loading..."
      case resp.body of
        Left err -> text $ "GET " <> url <> " response failed to decode: " <> AX.printResponseFormatError err
        Right body -> do
          let postsResp = do
                o <- decodeJson body
                d1 <- o .: "data"
                cs <- d1 .: "children"
                decodePostArray cs
          case postsResp of
            Left err -> text ("Error: " <> err)
            Right posts -> do
              div_ Da.do
                div_ $ orr $ map (\(Post p) -> div_ (text p.title)) (take 5 posts)
                div_ $ button [unit <$ onClick] $ text "Refresh"
              fetchPosts
