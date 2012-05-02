{-# LANGUAGE OverloadedStrings #-}
module Network.API.CrunchBase.Request where

import Network.URI (URIAuth(..), URI(..))
import Data.List (intercalate)
import Network.HTTP
import qualified Data.ByteString.Lazy as B

apiAuth :: URIAuth
apiAuth = URIAuth "" "api.crunchbase.com" ""

requestURI :: String -> [String] -> URI
requestURI path queries = URI "http:" auth path' query' ""
  where auth = Just apiAuth
        path' = "/v/1" ++ path
        query' | null queries = ""
               | otherwise = "?" ++ queries'
          where queries' = intercalate "&" queries

searchRequest :: String -> Request B.ByteString
searchRequest query = mkRequest GET uri
  where uri = requestURI "/search.js" ["query=" ++ urlEncode query]

companyRequest :: String -> Request B.ByteString
companyRequest permalink = mkRequest GET $ requestURI path []
  where path = "/company/" ++ permalink ++ ".js"

personRequest :: String -> Request B.ByteString
personRequest permalink = mkRequest GET $ requestURI path []
  where path = "/person/" ++ permalink ++ ".js"
