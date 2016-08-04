servant-router
---

`servant-router` routes a URI given a Servant API and an appropriate handler.
In web applications,
this is used to make single page applications (SPAs) with front-end routing,
letting you share portions of your Servant APIs between the client and server.

`servant-router` does not depend on `reflex` or any GHCJS packages.
It's intended to be a general purpose URI router on any platform.
Combined with `reflex-dom`, `servant-reflex`, and `reflex-dom-contrib`,
this makes for a very satisfactory front-end Haskell experience.

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.Except
import           Data.Proxy
import           Reflex.Dom
import           Reflex.Dom.Contrib.Router
import           Servant.API
import           Servant.Router

type MyApi = "books" :> Capture "id" Int :> View
        :<|> "search" :> QueryParam "keywords" String :> View
myApi :: Proxy MyApi
myApi = Proxy

main :: IO ()
-- reflex-dom-contrib provides a 'routeSite' function
-- for routing single page applications with reflex.
-- servant-router uses the provided uri String to perform the routing.
main = routeSite $ \uri -> do
  let handler = books :<|> search
      books i = do
        -- Here, you would get and display a book.
        -- Return a Reflex event for changing the browser location.
        return never
      search Nothing = do
        -- Here, you would display a search bar.
        return never
      search (Just keywords) = do
        -- Here you would display the search bar plus results.
        return never
  -- With the handler constructed, run the router with the uri.
  result <- runRoute uri myApi handler
  case result of
    -- If 'Left', there was no correct route for the uri.
    Left _ -> do
      el "div" $ text "No such page"
      return never
    -- If 'Right', the result of the route is returned.
    Right e -> return e
```

Serving
---

When using `servant-router` on the front-end in Single Page Applications (SPAs),
you still need to serve the application from the back-end.
To share the routing layout between the front-end and back-end,
the `View` endpoints need to be converted to a verb like `Get`.
Plus, with an SPA,
all the end-points should serve the same HTML on the back-end.
To do this, use the `ViewTransform` type family, and `constHandler` function.

```haskell
type Views = "books" :> View
        :<|> "search" :> QueryParam "query" String :> View

-- Equivalent to:
--
--   type ViewsServer = "books" :> Get '[HTML] Blaze.Html
--          :<|> "search" :> QueryParam "query" String :> Get '[HTML] Blaze.Html
type ViewsServer = ViewTransform Views (Get '[HTML] Blaze.Html)

viewsServer :: Server ViewsServer
viewsServer = constHandler
	    (Proxy :: Proxy Views)
	    (Proxy :: Proxy Handler) $
	    docTypeHtml $ do
	      H.head $ return ()
	      body $
	        script ! src "app.js" $ return ()
```
