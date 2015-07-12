module Download where

import Data.ByteString.Lazy.UTF8
import Network.HTTP.Conduit

download :: String -> IO String
download url = do
  page <- simpleHttp url
  return (toString page)
