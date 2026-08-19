{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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

-- We patch this file's WANT_* lines rather than writing a config.h from
-- scratch, because it also carries required boilerplate (standard includes,
-- GCRED/INTTABLE/SANITY/STACKOVL, ISLINUX/ISMACOS) that isn't gated by any
-- WANT_ flag. A bare file of just #defines would fail to build regardless of
-- the flag values, which would swamp real counterexamples with noise.
unixConfigH :: FilePath
unixConfigH = runtimeDir </> "unix" </> "config.h"

-- creates a temporary directory
-- writes the configurtion there
-- uses the createProcess crap from System.Process to build the runtime system with the rendered configuration
-- report success if build works successfully, and shrinks the configuration if it could not be built without error
-- always cleans up by deleting the temporary directory
prop_configuration_allowed :: Config -> Property
prop_configuration_allowed cfg = ioProperty $
  withSystemTempDirectory "mhs-config-test" $ \tmpDir -> do
    baseConfig <- TIO.readFile unixConfigH
    -- Appended overrides win: a redefined macro is only a -Wall warning, not
    -- an error, and this is the same unix/config.h the real build uses, so
    -- only the flags under test vary.
    TIO.writeFile (tmpDir </> "config.h") $
      T.unlines [baseConfig, "/* --- Config overrides --- */", configToHeader cfg]

    (exitCode, _stdout, stderrOut) <-
      readProcessWithExitCode
        "cc"
        ( [ "-Wall",
            "-I" ++ tmpDir, -- must come first: shadows unix/config.h
            "-I" ++ runtimeDir,
            "-I" ++ (runtimeDir </> "unix") -- for extra.c, still #include'd unqualified
          ]
            ++ [runtimeDir </> "main.c", runtimeDir </> "eval.c", runtimeDir </> "comb.c"]
            ++ ["-lm", "-lgmp", "-o", tmpDir </> "mhseval"]
        )
        ""

    pure $ counterexample stderrOut (exitCode == ExitSuccess)

-- Test invoker
-- call quickCheck from main, importing this file, with maxSuccess set to 10, to see if it works.
-- I expect each property take a few seconds to evaluate, so 10 is good for now.
-- If you find a counterexample, do not try to patch the runtime. I will do that myself.