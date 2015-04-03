{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Main
Description : Simply redirects HTTP->HTTPS (e.g. use with AWS ELB)
Copyright   : (c) FPComplete.com, 2015
License     : BSD3
Maintainer  : Tim Dysinger <tim@fpcomplete.com>
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
    ( status406, status307, status301, methodGet )
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
                 (flip run (logStdout bounce) <$>
                  option auto
                         (short 'p' <>
                          long "port" <>
                          metavar "PORT" <>
                          value 4321 <>
                          showDefault <>
                          help "Port for the webserver")))
                (fullDesc <>
                 header ("Bouncy " <>
                         $(packageVariable (pkgVersion . package)) <>
                         " " <>
                         $(stringE =<<
                           runIO (show `fmap` Data.Time.getCurrentTime))) <>
                 progDesc "Bounces HTTP->HTTPS (e.g., AWS ELB)")))
  where bounce rq rs
          | isJust (requestHeaderHost rq) =
            let (host:_) =
                  C.split ':' (fromJust (requestHeaderHost rq))
                https =
                  "https://" <> host <>
                  (rawPathInfo rq) <>
                  (rawQueryString rq)
                status =
                  if requestMethod rq == methodGet
                     then status301
                     else status307
            in rs (responseBuilder status
                                   [("Location",https)]
                                   mempty)
        bounce _ rs
          | otherwise =
            rs (responseBuilder status406 [] mempty)
