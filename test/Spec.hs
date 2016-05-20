{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Monad.Except
import           Data.Proxy
import           Data.Traversable
import           Servant.API
import           Servant.Router

type TestApi = "root" :> Capture "cap" Int :> QueryParam "param" String :> EmptyRoute
          :<|> "other" :> Capture "othercap" String :> EmptyRoute
testApi :: Proxy TestApi
testApi = Proxy

testUris :: [String]
testUris =
  [ "https://test.com/root/4?param=hi"
  , "https://test.com/other/hi/"
  , "https://test.com/fail"
  , "https://test.com/root/fail"
  ]

main :: IO ()
main = do
  let root :: Int -> Maybe String -> ExceptT RoutingError IO ()
      root i s = liftIO $ print (i, s)
      other :: String -> ExceptT RoutingError IO ()
      other s = liftIO $ print s
  void $ for testUris $ \uri -> do
    result <- runExceptT $ runRoute uri testApi (root :<|> other)
    print result
