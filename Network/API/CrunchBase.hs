{-# LANGUAGE OverloadedStrings #-}
module Network.API.CrunchBase where

import qualified Network.API.CrunchBase.Request as Req
import Network.HTTP

apiResponse request = simpleHTTP request >>= getResponseBody
