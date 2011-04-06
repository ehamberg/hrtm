{-# Language OverloadedStrings #-}

import Network.HTTP
import Data.List (sort)
import Control.Arrow
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5

apiAddr = "http://api.rememberthemilk.com/services/rest/?"
apiKey = "foo"
secret = "bar"

type APIParam = (B.ByteString,B.ByteString)

signRequest :: B.ByteString -> [APIParam] -> MD5Digest
signRequest prev =
  sort >>> map (uncurry B.append) >>> B.concat >>> B.append prev >>> md5

constrQuery :: [APIParam] -> B.ByteString
constrQuery params = apiAddr `B.append`
  B.intercalate "&" (map (\(p, v) -> p `B.append` "=" `B.append` v) params)

main = do
  let params = [("method", "rtm.auth.getFrob"),
                ("format","json"),
                ("api_key",apiKey)]
  let query = constrQuery (params ++ [("api_sig",(B.pack .show . signRequest secret) params)])
  response <- simpleHTTP (getRequest (B.unpack query)) >>= getResponseBody
  putStrLn response
