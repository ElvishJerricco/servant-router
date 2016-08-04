{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.ServerT where

import           Data.Proxy
import           GHC.TypeLits
import           Servant.API

-- Needs a class because @path :> api@ needs a constraint
class HasServerT layout where
  type ServerT layout (m :: * -> *)

instance HasServerT (a :<|> b) where
  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

instance HasServerT (Capture capture (a :: *) :> api) where
  type ServerT (Capture capture a :> api) m = a -> ServerT api m

instance HasServerT (Verb method status ctypes (a :: *)) where
  type ServerT (Verb method status ctypes a) m = m a

instance HasServerT (Header sym a :> api) where
  type ServerT (Header sym a :> api) m = Maybe a -> ServerT api m

instance HasServerT (QueryParam sym (a :: *) :> api) where
  type ServerT (QueryParam sym a :> api) m = Maybe a -> ServerT api m

instance HasServerT (QueryParams sym (a :: *) :> api) where
  type ServerT (QueryParams sym a :> api) m = [a] -> ServerT api m

instance HasServerT (QueryFlag sym :> api) where
  type ServerT (QueryFlag sym :> api) m = Bool -> ServerT api m

instance HasServerT (ReqBody list (a :: *) :> api) where
  type ServerT (ReqBody list a :> api) m = a -> ServerT api m

instance KnownSymbol path => HasServerT (path :> api) where
  type ServerT (path :> api) m = ServerT api m

-- Const
class HasServerT layout => HasConstHandler layout a where
  constHandler :: Monad m => Proxy layout -> Proxy m -> a -> ServerT layout m

instance (HasConstHandler a c, HasConstHandler b c) => HasConstHandler (a :<|> b) c where
  constHandler _ m c = constHandler (Proxy :: Proxy a) m c :<|> constHandler (Proxy :: Proxy b) m c

instance HasConstHandler api c => HasConstHandler (Capture capture (a :: *) :> api) c where
  constHandler _ m c _ = constHandler (Proxy :: Proxy api) m c

instance HasConstHandler (Verb method status ctypes c) c where
  constHandler _ _ = return

instance HasConstHandler api c => HasConstHandler (Header sym a :> api) c where
  constHandler _ m c _ = constHandler (Proxy :: Proxy api) m c

instance HasConstHandler api c => HasConstHandler (QueryParam sym (a :: *) :> api) c where
  constHandler _ m c _ = constHandler (Proxy :: Proxy api) m c

instance HasConstHandler api c => HasConstHandler (QueryParams sym (a :: *) :> api) c where
  constHandler _ m c _ = constHandler (Proxy :: Proxy api) m c

instance HasConstHandler api c => HasConstHandler (QueryFlag sym :> api) c where
  constHandler _ m c _ = constHandler (Proxy :: Proxy api) m c

instance HasConstHandler api c => HasConstHandler (ReqBody list (a :: *) :> api) c where
  constHandler _ m c _ = constHandler (Proxy :: Proxy api) m c

instance (KnownSymbol path, HasConstHandler api c) => HasConstHandler (path :> api) c where
  constHandler _ = constHandler (Proxy :: Proxy api)
