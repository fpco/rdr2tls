{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Main
Description : Simply redirects HTTP->HTTPS (e.g. use with AWS ELB)
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : FPComplete Developers <dev@fpcomplete.com>
Stability   : experimental
Portability : POSIX
-}

module Main where

import           BasePrelude
import qualified Data.ByteString.Char8 as C ( split )
import Data.Time ( getCurrentTime )
import Distribution.PackageDescription.TH
    ( PackageDescription(package),
      PackageIdentifier(pkgVersion),
      packageVariable )
import Language.Haskell.TH ( runIO, stringE )
import Network.HTTP.Types
    ( status406, status307, status301, methodGet, methodHead )
import Network.Wai
    ( Request(rawPathInfo, rawQueryString, requestHeaderHost,
              requestMethod),
      responseBuilder )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdout )
import Options.Applicative
    ( helper,
      execParser,
      value,
      short,
      progDesc,
      option,
      metavar,
      long,
      info,
      help,
      header,
      fullDesc,
      auto,
      showDefault )

-- | Main entry point.
main :: IO ()
main =
  join (execParser
          (info (helper <*>
                 (runApp <$>
                  option auto
                         (short 'p' <>
                          long "port" <>
                          metavar "PORT" <>
                          value 8080 <>
                          showDefault <>
                          help "Port for the webserver") <*>
                  option auto
                         (short 'd' <>
                          long "domain" <>
                          metavar "DOMAIN" <>
                          value 8080 <>
                          help "Force a particular domain")))
                (fullDesc <>
                 header ("rdr2tls " <>
                         $(packageVariable (pkgVersion . package)) <>
                         " " <>
                         $(stringE =<<
                           runIO (show `fmap` Data.Time.getCurrentTime))) <>
                 progDesc "Redirects all traffic HTTP -> HTTPS")))
  where runApp port domain =
          run port (logStdout (redirect domain))
        redirect domain rq rs
          | isJust (requestHeaderHost rq) =
            let (prefix:_) =
                  if isJust domain
                     then fromJust domain
                     else C.split ':' (fromJust (requestHeaderHost rq))
                https =
                  "https://" <> prefix <>
                  (rawPathInfo rq) <>
                  (rawQueryString rq)
                status =
                  if ((requestMethod rq == methodGet) ||
                      (requestMethod rq == methodHead))
                     then status301
                     else status307
            in rs (responseBuilder status
                                   [("Location",https)]
                                   mempty)
        redirect _ _ rs
          | otherwise =
            rs (responseBuilder status406 [] mempty)
