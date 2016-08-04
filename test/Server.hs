{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Proxy
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Blaze
import           Servant.ServerT
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes

type Views a = "books" :> Get '[HTML] a
        :<|> "search" :> QueryParam "query" String :> Get '[HTML] a
views :: Proxy (Views Html)
views = Proxy

type Api = "api" :> "books" :> Get '[JSON] [String]
api :: Proxy Api
api = Proxy

type WholeServer = Views Html
              :<|> Api
wholeServer :: Proxy WholeServer
wholeServer = Proxy

server :: Server WholeServer
server = viewServer :<|> apiServer where
  apiServer = return ["Book Title!"]
  viewServer = constHandler views (Proxy :: Proxy Handler) $ docTypeHtml $ do
    H.head $ return ()
    body $ script ! src "app.js" $ return ()

main :: IO ()
main = run 8080 $ serve wholeServer server
