{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, renameDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, ioProperty, shrinkList)

newtype Config = Config [(Flag, Bool)]
  deriving Show

data Flag
  = FLOAT32
  | FLOAT64
  | MATH
  | INT64
  | GMP
  | IMATH
  | OVERFLOW
  | STDIO
  | FD
  | DIR
  | MEM
  | IO_POLL
  | SOCKET
  | ERRNO
  | UTF8
  | BUF
  | CRLF
  | BASE64
  | LZ77
  | LZMA
  | RLE
  | BWT
  | ARGS
  | ENV
  | TIME
  | MD5
  | TICK
  | SIGINT
  | KPERF
  | TAGNAMES
  deriving (Show, Enum, Bounded) -- all of them

allFlags :: [Flag]
allFlags = [minBound .. maxBound]

-- QuickCheck Generator

genConfig :: Gen Config
genConfig = Config <$> traverse (\f -> (,) f <$> arbitrary) allFlags -- generate arbitrary assignment of flags to all of them, every flag must be set

-- QuickCheck Shrinker

-- this may chuck away flags completely
shrinkConfig :: Config -> [Config]
shrinkConfig (Config assocs) = Config <$> shrinkList shrinkEntry assocs
  where
    shrinkEntry (flag, on) = [(flag, on') | on' <- shrink on]

-- QuickCheck Instance

instance Arbitrary Config where
    arbitrary = genConfig
    shrink = shrinkConfig

-- Render

configToHeader :: Config -> Text
configToHeader (Config assocs) = T.unlines (map defineLine assocs)
  where
    defineLine (flag, on) =
      T.concat ["#define WANT_", T.pack (show flag), " ", if on then "1" else "0"]

-- Property

-- Paths are relative to the package directory (config-test/), since that's
-- where `cabal run` leaves the working directory.
runtimeDir :: FilePath
runtimeDir = ".." </> "src" </> "runtime"

-- Kept counterexamples live here instead of the system tmpdir, so they're
-- easy to find and don't get swept away by a reboot or /tmp cleaner.
tmpRoot :: FilePath
tmpRoot = "tmp"

-- `scratchDir` is reused for every candidate the shrinker tries, win or
-- lose. `counterexampleDir` only ever gets overwritten on a failure.
--
-- QuickCheck's shrink search is a strictly decreasing chain of failures:
-- at each step it tries a failing config's shrink candidates in order,
-- stops at the first one that *also* fails, and recurses from there; once
-- none of a config's candidates fail, that config is the final, reported
-- counterexample. So "the last failing evaluation, in temporal order" is
-- always exactly the reported one - overwriting counterexampleDir on every
-- failure (and leaving it alone on every pass) converges on that, without
-- this property needing to know which call is "the last" one.
scratchDir, counterexampleDir :: FilePath
scratchDir = tmpRoot </> "scratch"
counterexampleDir = tmpRoot </> "counterexample"

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists dir = do
  exists <- doesDirectoryExist dir
  when exists (removeDirectoryRecursive dir)

-- Test invoker helper: wipe leftovers from a previous run, so a run that
-- finds no counterexample doesn't leave a stale one lying around looking
-- like a live one.
resetTmp :: IO ()
resetTmp = removeDirIfExists tmpRoot

-- Compiler invocation used both for the live QuickCheck-driven build and
-- (in Make syntax) for the leftover Makefile, so a kept-around counterexample
-- reproduces with the exact same flags. `rts` is the absolute runtime dir
-- (config-test/'s cwd assumption doesn't hold once the directory is kept
-- around and inspected later from wherever).
ccArgs :: FilePath -> FilePath -> [String]
ccArgs rts tmpDir =
  [ "-Wall",
    "-I" ++ tmpDir, -- must come first: shadows unix/config.h
    "-I" ++ rts,
    "-I" ++ (rts </> "unix") -- for extra.c, still #include'd unqualified
  ]
    ++ [rts </> "main.c", rts </> "eval.c", rts </> "comb.c"]
    ++ ["-lm", "-lgmp", "-o", tmpDir </> "mhseval"]

makefileFor :: FilePath -> Text
makefileFor rts =
  T.unlines
    [ "CC     = cc",
      "RTS    = " <> T.pack rts,
      "CFLAGS = -Wall",
      "LIBS   = -lm -lgmp",
      "",
      "mhseval: config.h",
      "\t$(CC) $(CFLAGS) -I. -I$(RTS) -I$(RTS)/unix $(RTS)/main.c $(RTS)/eval.c $(RTS)/comb.c $(LIBS) -o mhseval",
      "",
      ".PHONY: clean",
      "clean:",
      "\trm -f mhseval"
    ]

-- creates a temporary directory
-- writes the configurtion there
-- uses the createProcess crap from System.Process to build the runtime system with the rendered configuration
-- report success if build works successfully, and shrinks the configuration if it could not be built without error
-- only the final, fully-shrunk failing config is kept (with a Makefile that
-- reruns the exact same build), at counterexampleDir; every candidate
-- along the way builds in scratchDir, discarded unless it's the one that
-- sticks
prop_configuration_allowed :: Config -> Property
prop_configuration_allowed cfg = ioProperty $ do
  rts <- canonicalizePath runtimeDir
  removeDirIfExists scratchDir
  createDirectoryIfMissing True scratchDir

  TIO.writeFile (scratchDir </> "config.h") (configToHeader cfg)
  TIO.writeFile (scratchDir </> "Makefile") (makefileFor rts)

  (exitCode, _stdout, stderrOut) <- readProcessWithExitCode "cc" (ccArgs rts scratchDir) ""

  if exitCode == ExitSuccess
    then removeDirIfExists scratchDir
    else do
      removeDirIfExists counterexampleDir
      renameDirectory scratchDir counterexampleDir

  let msg
        | exitCode == ExitSuccess = stderrOut
        | otherwise = stderrOut ++ "\ncounterexample kept at " ++ counterexampleDir ++ " (cd there, then `make` to reproduce)"

  pure $ counterexample msg (exitCode == ExitSuccess)

-- Test invoker
-- call quickCheck from main, importing this file, with maxSuccess set to 10, to see if it works.
-- I expect each property take a few seconds to evaluate, so 10 is good for now.
-- If you find a counterexample, do not try to patch the runtime. I will do that myself.