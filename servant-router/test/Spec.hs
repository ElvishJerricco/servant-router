{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Data.Foldable
import Data.Proxy
import Servant.API
import Servant.Router
import URI.ByteString

type TestApi = "root" :> Capture "cap" Int :> QueryParam "param" String :> View
          :<|> "other" :> Capture "othercap" String :> View
testApi :: Proxy TestApi
testApi = Proxy

testUris :: [ByteString]
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
    result <- sequence $ withURI strictURIParserOptions uri $ \x ->
      runRouteUri testApi (root :<|> other) x
    print result

-- TODO: Maybe open a pull request with this on uri-bytestring?
withURI :: URIParserOptions -> ByteString -> (forall a . URIRef a -> b) -> Either URIParseError b
withURI opts str f = case parseRelativeRef opts str of
  Right x -> Right (f x)
  Left  _ -> f <$> parseURI opts str
