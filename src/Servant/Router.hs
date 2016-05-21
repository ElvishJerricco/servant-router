{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.Router where

import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import           Data.Proxy
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding
import           GHC.TypeLits
import           Network.HTTP.Types
import           Network.URI
import           Servant.API
import           Web.HttpApiData

-- | Router terminator.
-- The 'HasRouter' instance for 'View' finalizes the router.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "bookId" Int :> View
data View

-- | 'Location' is used to split the path and query of a URI into components.
data Location = Location
  { locPath  :: [Text]
  , locQuery :: Query
  } deriving (Show, Eq, Ord)

-- | When routing, the router may fail to match a location.
-- Either this is an unrecoverable failure,
-- such as failing to parse a query parameter,
-- or it is recoverable by trying another path.
data RoutingError = Fail | FailFatal deriving (Show, Eq, Ord)

-- | A 'Router' contains the information necessary to execute a handler.
data Router m a where
  RChoice       :: Router m a -> Router m a -> Router m a
  RCapture      :: FromHttpApiData x => (x -> Router m a) -> Router m a
  RQueryParam   :: (FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> (Maybe x -> Router m a) -> Router m a
  RQueryParams  :: (FromHttpApiData x, KnownSymbol sym)
                   => Proxy sym -> ([x] -> Router m a) -> Router m a
  RQueryFlag    :: KnownSymbol sym
                   => Proxy sym -> (Bool -> Router m a) -> Router m a
  RPath         :: KnownSymbol sym => Proxy sym -> Router m a -> Router m a
  RPage         :: m a -> Router m a

-- | This is similar to the @HasServer@ class from @servant-server@.
-- It is the class responsible for making API combinators routable.
-- 'RuoteT' is used to build up the handler types.
-- 'Router' is returned, to be interpretted by 'routeLoc'.
class HasRouter layout where
  type RouteT layout (m :: * -> *) a :: *
  route :: Proxy layout -> Proxy m -> Proxy a -> RouteT layout m a -> Router m a

instance (HasRouter x, HasRouter y) => HasRouter (x :<|> y) where
  type RouteT (x :<|> y) m a = RouteT x m a :<|> RouteT y m a
  route
    _
    (m :: Proxy m)
    (a :: Proxy a)
    ((x :: RouteT x m a) :<|> (y :: RouteT y m a))
    = RChoice (route (Proxy :: Proxy x) m a x) (route (Proxy :: Proxy y) m a y)

instance (HasRouter sublayout, FromHttpApiData x)
    => HasRouter (Capture sym x :> sublayout) where
  type RouteT (Capture sym x :> sublayout) m a = x -> RouteT sublayout m a
  route _ m a f = RCapture (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParam sym x :> sublayout) where
  type RouteT (QueryParam sym x :> sublayout) m a
    = Maybe x -> RouteT sublayout m a
  route _ m a f = RQueryParam
    (Proxy :: Proxy sym)
    (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, FromHttpApiData x, KnownSymbol sym)
    => HasRouter (QueryParams sym x :> sublayout) where
  type RouteT (QueryParams sym x :> sublayout) m a = [x] -> RouteT sublayout m a
  route _ m a f = RQueryParams
    (Proxy :: Proxy sym)
    (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, KnownSymbol sym)
    => HasRouter (QueryFlag sym :> sublayout) where
  type RouteT (QueryFlag sym :> sublayout) m a = Bool -> RouteT sublayout m a
  route _ m a f = RQueryFlag
    (Proxy :: Proxy sym)
    (route (Proxy :: Proxy sublayout) m a . f)

instance (HasRouter sublayout, KnownSymbol path)
    => HasRouter (path :> sublayout) where
  type RouteT (path :> sublayout) m a = RouteT sublayout m a
  route _ m a page = RPath
    (Proxy :: Proxy path)
    (route (Proxy :: Proxy sublayout) m a page)

instance HasRouter View where
  type RouteT View m a = m a
  route _ _ _ = RPage

-- | Use a handler to route a location, represented as a 'String'.
-- All handlers must, in the end, return @m a@.
-- 'routeLoc' will choose a route and return its result.
runRoute :: forall layout m a. (HasRouter layout, MonadError RoutingError m)
         => String -> Proxy layout -> RouteT layout m a -> m a
runRoute loc' layout page = case uriToLocation <$> parseURI loc' of
  Nothing -> throwError FailFatal
  Just uri -> let
    routing = route layout (Proxy :: Proxy m) (Proxy :: Proxy a) page
    in routeLoc uri routing

-- | Use a computed 'Router' to route a 'Location'.
routeLoc :: MonadError RoutingError m => Location -> Router m a -> m a
routeLoc loc r = case r of
  RChoice a b -> catchError (routeLoc loc a) $ \e -> case e of
    Fail -> routeLoc loc b
    FailFatal -> throwError e
  RCapture f -> case locPath loc of
    [] -> throwError Fail
    capture:paths -> maybe
      (throwError FailFatal)
      (routeLoc loc { locPath = paths })
      (f <$> parseUrlPieceMaybe capture)
  RQueryParam sym f -> case lookup (BS.pack $ symbolVal sym) (locQuery loc) of
    Nothing -> routeLoc loc $ f Nothing
    Just Nothing -> throwError FailFatal
    Just (Just text) -> case parseQueryParamMaybe (decodeUtf8 text) of
      Nothing -> throwError FailFatal
      Just x -> routeLoc loc $ f (Just x)
  RQueryParams sym f -> maybe (throwError FailFatal) (routeLoc loc . f) $ do
    ps <- sequence $ snd <$> filter
      (\(k, _) -> k == BS.pack (symbolVal sym)) (locQuery loc)
    sequence $ (parseQueryParamMaybe . decodeUtf8) <$> ps
  RQueryFlag sym f -> case lookup (BS.pack $ symbolVal sym) (locQuery loc) of
    Nothing -> routeLoc loc $ f False
    Just Nothing -> routeLoc loc $ f True
    Just (Just _) -> throwError FailFatal
  RPath sym a -> case locPath loc of
    [] -> throwError Fail
    p:paths -> if p == T.pack (symbolVal sym)
      then routeLoc (loc { locPath = paths }) a
      else throwError Fail
  RPage a -> a

-- | Convert a 'URI' to a 'Location'.
uriToLocation :: URI -> Location
uriToLocation uri = Location
  { locPath = decodePathSegments $ BS.pack (uriPath uri)
  , locQuery = parseQuery $ BS.pack (uriQuery uri)
  }
