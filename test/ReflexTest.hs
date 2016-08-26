{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

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
        el "div" $ text $ "Book: " ++ show i
        return never
      search Nothing = do
        -- Here, you would display a search bar.
        return never
      search (Just keywords) = do
        -- Here you would display the search bar plus results.
        el "div" $ text $ "You searched: " ++ keywords
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
