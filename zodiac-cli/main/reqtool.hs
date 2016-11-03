{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_zodiac_cli
import           DependencyInfo_ambiata_zodiac_cli

import qualified Data.ByteString as BS

import           P

import           System.IO (IO, BufferMode(..), putStrLn, print)
import           System.IO (stdout, stderr, hSetBuffering)

import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (SafeCommand(..), RunType(..))
import           X.Options.Applicative (dispatch, safeCommand)

import qualified Zodiac.Cli.ReqTool.Commands as ReqTool
import           Zodiac.Cli.ReqTool.Data
import           Zodiac.Cli.ReqTool.Error
import           Zodiac.Cli.ReqTool.Parser

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch (safeCommand reqToolCommandP) >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn $ "reqtool: " <> buildInfoVersion
      DependencyCommand ->
        mapM_ putStrLn dependencyInfo
      RunCommand DryRun c ->
        print c
      RunCommand RealRun c ->
        run c

run :: ReqToolCommand -> IO ()
run c = case c of
  ReqToolCanonise le ->
    BS.putStr =<< (orDie renderReqToolError $ ReqTool.canonise le)

