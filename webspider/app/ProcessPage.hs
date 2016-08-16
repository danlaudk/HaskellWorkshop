module ProcessPage where
import Download
import Text.HTML.TagSoup


processPage :: String -> IO [String] 
processPage url = 
   do page <- download url
      return (process page)

process :: String -> [String]
process =
  filter (not . null) .
  map (fromAttrib "href") .
  filter ( not . nofollow) .
  filter (isTagOpenName "a") .
  canonicalizeTags .
  parseTags



nofollow tag = fromAttrib "rel" tag == "nofollow"
