{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Foldable
import           Data.Proxy
import           Servant.API
import           Servant.Router

type TestApi = "root" :> Capture "cap" Int :> QueryParam "param" String :> View
          :<|> "other" :> Capture "othercap" String :> View
testApi :: Proxy TestApi
testApi = Proxy

testUris :: [String]
testUris =
  [ "https://test.com/root/4?param=hi"
  , "https://test.com/other/hi/"
  , "/other/relativeMatch"
  , "https://test.com/fail"
  , "https://test.com/root/fail"
  , "/root/relativeFail"
  ]

main :: IO ()
main = do
  let root :: Int -> Maybe String -> IO ()
      root i s = print (i, s)
      other :: String -> IO ()
      other = print
  for_ testUris $ \uri -> do
    result <- runRoute uri testApi (root :<|> other)
    print result
