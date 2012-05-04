{-# LANGUAGE OverloadedStrings #-}
module Network.API.CrunchBase where

import qualified Network.API.CrunchBase.Request as Req
import qualified Network.API.CrunchBase.Response as Res
import qualified Network.API.CrunchBase.Response.Search as Search
import Network.HTTP

apiResponse request = simpleHTTP request >>= getResponseBody
