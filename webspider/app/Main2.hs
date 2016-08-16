module Main where

import Lib
import Network.HTTP.Conduit
import Data.ByteString.Lazy.UTF8(toString)
import Text.HTML.TagSoup
import Network.URI

main :: IO ()
main = someFunc

download :: String -> IO String
download url = do res <- simpleHttp url
                  return (toString res)

makeFileName :: Int -> FilePath
makeFileName k = "download-" ++ show k ++ ".html"

saveAs :: String -> Int -> IO ()
saveAs url k =
  do content <- download url
     writeFile (makeFileName k) content     

spider :: Int -> URL -> IO (Map URL [URL])
spider count url0 = go 0 Map.empty (Set.singleton url0)
  where
    go k seen queue0
        | k >= count = return seen
        | otherwise  =
      case Set.minView queue0 of
        Nothing -> return seen
        Just (url, queue) -> do
          page <- download url
          let ls       = links url page
              newSeen  = Map.insert url ls seen
              notSeen  = Set.fromList .
                         filter (`Map.notMember` newSeen) $ ls
              newQueue = queue `Set.union` notSeen
          go (k+1) newSeen newQueue
