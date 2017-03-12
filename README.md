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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as Text
import           Reflex.Dom
import           Reflex.Dom.Contrib.Router
import           Servant.API hiding (URI)
import           Servant.Router

type MyApi = "books" :> Capture "id" Int :> View
        :<|> "search" :> QueryParam "query" String :> View
myApi :: Proxy MyApi
myApi = Proxy

main :: IO ()
main = mainWidget widget

widget :: forall t m . MonadWidget t m => m ()
widget = do
  let handler :: RouteT MyApi m (Event t URI)
      handler = books :<|> search
      books i = do
        -- Here, you would get and display a book.
        -- Return a Reflex event for changing the browser location.
        el "div" $ text $ "Book: " <> Text.pack (show i)
        return never
      search Nothing         = do
        -- Here, you would display a search bar.
        el "div" $ text "You searched nothing"
        return never
      search (Just keywords) = do
        -- Here you would display the search bar plus results.
        el "div" $ text $ "You searched: " <> Text.pack keywords
        return never
      -- Use reflex-dom-contrib for handling the address bar.
      routeHandler = route' (\_ uri -> uri) (runRouteUri myApi handler)
  
  rec dynamicRoute <- routeHandler (switch (current changeRoute))
      routeWasSet  <- dyn dynamicRoute -- Will fire on postbuild
      changeRoute  <- holdDyn never $ fmap (either (const never) id) routeWasSet
  return ()
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
