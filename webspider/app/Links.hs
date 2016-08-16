module Links where

import Data.Maybe
--import Network
import Network.URI
import Text.HTML.TagSoup

links :: String -> String -> [String]
links url = catMaybes .
            map (canonicalizeLink url) .
            filter (not . null) .
            map (fromAttrib "href") .
            filter (\t -> fromAttrib "rel" t /= "nofollow") .
            filter (isTagOpenName "a") .
            canonicalizeTags .
            parseTags

canonicalizeLink :: String -> String -> Maybe String
canonicalizeLink referer path = 
  case (parseURI referer, parseURIReference path) of
    (Just r, Just p) -> let n = p `nonStrictRelativeTo` r
                            u = uriToString id n ""
                        in Just (takeWhile (/= '#') u)
    _                -> Nothing
