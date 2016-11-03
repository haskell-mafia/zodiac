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

import qualified Zodiac.Cli.TSRP.Commands as TSRP
import           Zodiac.Cli.TSRP.Data
import           Zodiac.Cli.TSRP.Error
import           Zodiac.Cli.TSRP.Parser

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch (safeCommand tsrpCommandP) >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn $ "tsrp: " <> buildInfoVersion
      DependencyCommand ->
        mapM_ putStrLn dependencyInfo
      RunCommand DryRun c ->
        print c
      RunCommand RealRun c ->
        run c

run :: TSRPCommand -> IO ()
run c = case c of
  TSRPAuth le re ->
    BS.putStr =<< (orDie renderTSRPError $ TSRP.authenticate le re)
  TSRPVerify le ->
    orDie renderTSRPError $ TSRP.verify le
  TSRPDebugAuthString le re ->
    BS.putStr =<< (orDie renderTSRPError $ TSRP.stringToAuthenticate le re)
