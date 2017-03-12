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
