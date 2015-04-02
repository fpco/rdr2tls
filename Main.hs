{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasePrelude
import qualified Data.ByteString.Char8 as C ( split )
import Network.Wai
    ( Request(rawPathInfo, rawQueryString, requestHeaderHost),
      responseBuilder )
import Network.Wai.Handler.Warp ( run )
import Network.HTTP.Types ( status406, status301 )

main :: IO ()
main = run 4321 bounce
  where bounce rq rs
          | isJust (requestHeaderHost rq) =
            let (host:_) =
                  C.split ':' (fromJust (requestHeaderHost rq))
                https =
                  "https://" <> host <>
                  (rawPathInfo rq) <>
                  (rawQueryString rq)
            in rs (responseBuilder status301
                                   [("Location",https)]
                                   mempty)
        bounce _ rs
          | otherwise =
            rs (responseBuilder status406 [] mempty)
