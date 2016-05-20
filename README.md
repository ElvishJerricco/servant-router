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

type MyApi = "books" :> Capture "id" Int :> EmptyRoute
        :<|> "search" :> QueryParam "keywords" String :> EmptyRoute
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
  result <- runExceptT $ runRoute uri myApi handler
  case result of
    -- If 'Left', there was no correct route for the uri.
    Left _ -> do
      el "div" $ text "No such page"
      return never
    -- If 'Right', the result of the route is returned.
    Right e -> return e
```
