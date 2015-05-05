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
import qualified Data.ByteString.Char8 as C ( split, pack )
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
      strOption,
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
                          help "Use a specific port") <*>
                  strOption (short 'P' <>
                             long "path" <>
                             metavar "PATH" <>
                             help "Use a specific path [eg, groups.google.com/forum]")))
                (fullDesc <>
                 header ("rdr2tls " <>
                         $(packageVariable (pkgVersion . package)) <>
                         " " <>
                         $(stringE =<<
                           runIO (show `fmap` Data.Time.getCurrentTime))) <>
                 progDesc "Redirects all traffic HTTP -> HTTPS")))
  where runApp port path =
          run port (logStdout (redirect path))
        redirect [] rq rs =
          if isJust (requestHeaderHost rq)
             then let (path:_) =
                        C.split ':' (fromJust (requestHeaderHost rq))
                  in respond path rq rs
             else rs (responseBuilder status406 [] mempty)
        redirect path rq rs =
          respond (C.pack path) rq rs
        respond path rq rs =
          let status =
                if ((requestMethod rq == methodGet) ||
                    (requestMethod rq == methodHead))
                   then status301
                   else status307
          in rs (responseBuilder
                   status
                   [("Location"
                    ,"https://" <> path <>
                     (rawPathInfo rq) <>
                     (rawQueryString rq))]
                   mempty)
