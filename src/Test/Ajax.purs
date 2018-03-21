module Test.Ajax where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, div', p', h4')
import Concur.React.Widgets (textButton')
import Control.Alt ((<|>))
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.?))
import Data.Array (take)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Network.HTTP.Affjax (get)

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
    d <- obj .? "data"
    id <- d .? "id"
    title <- d .? "title"
    pure (Post { id, title })

subreddits :: Array String
subreddits = ["programming", "purescript", "haskell", "javascript"]

ajaxWidget :: forall a eff. Widget HTML a
ajaxWidget = div'
  [ p' [text "Click button to fetch posts from reddit"]
  , div' (map fetchReddit subreddits)
  ]

fetchReddit :: forall a eff. String -> Widget HTML a
fetchReddit sub = div'
  [ h4' [text ("/r/" <> sub)]
  , showPosts
  ]
  where
    showPosts = textButton' "Fetch posts" >>= \_ -> fetchPosts
    fetchPosts = do
      liftEff (log ("Fetching posts from subreddit - " <> sub))
      resp <- (liftAff (get ("https://www.reddit.com/r/" <> sub <> ".json"))) <|> (text "Loading...")
      let postsResp = do
            o <- decodeJson resp.response
            d1 <- o .? "data"
            cs <- d1 .? "children"
            decodePostArray cs
      case postsResp of
        Left err -> text ("Error: " <> err)
        Right posts -> do
          div'
            [ div' (map (\(Post p) -> div' [text p.title]) (take 5 posts))
            , div' [textButton' "Refresh"]
            ]
          fetchPosts
