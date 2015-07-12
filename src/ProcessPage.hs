module ProcessPage where
import Download
import Text.HTML.TagSoup


processPage url = do
  page <- download url
  return (process page)

process =
  filter (not . null) .
  map (fromAttrib "href") .
  filter ( not . nofollow) .
  filter (isTagOpenName "a") .
  canonicalizeTags .
  parseTags



nofollow tag = fromAttrib "rel" tag == "nofollow"
