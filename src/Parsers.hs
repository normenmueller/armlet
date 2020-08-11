{-|
Module      : Parsers
Description : n/a
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

Command line and XML parsing.
-}
module Parsers
    ( parse
    , Opt(..)
    , Cmd(..)
    , cmdln
    ) where

import           Data.Semigroup        ((<>))
import           Data.Version          (showVersion)
import           Options.Applicative
import           Paths_armlet          (version)
import           Prelude               hiding (readFile)
import           Text.XML
import           Text.XML.Stream.Parse

{------------------------------------------------------------------------------
  XML Parsing
-------------------------------------------------------------------------------}

parse :: FilePath -> IO Document
parse = readFile dft
  where
    dft = def {psRetainNamespaces = True}

{------------------------------------------------------------------------------
  Commandline Parsing
-------------------------------------------------------------------------------}

data Opt =
    Opt
        { optTgt :: Maybe FilePath
        , optCmd :: Cmd
        }

data Cmd
    = StampCmd
          { stampSrc    :: FilePath
          , stampAuthor :: String
          }
    | MarkCmd
          { markSrc :: FilePath
          , markScp :: String
          , markClr :: String
          }
    | UnmarkCmd
          { unmarkSrc :: FilePath
          }
    | NormalizeCmd
          { normalizeSrc :: FilePath
          }
    | ReleaseCmd
          { releaseSrc     :: FilePath
          , releaseVersion :: String
          }

cmdln :: IO Opt
cmdln =
    execParser $
    info
        (helper <*> veropt <*> opts)
        (fullDesc <>
         header "armlet - ARchimate Modelling LanguagE Transformations")
  where
    veropt :: Parser (a -> a)
    veropt =
        infoOption (showVersion version) (long "version" <> help "Show version")

-- Note: As @program --global-options command --local-options@ is a fairly
-- standard pattern, we do not support @program command
-- --global-and-local-options@.
--
-- Note: Global options are not shown in sub-commands.
-- (cf. [#138](https://github.com/pcapriotti/optparse-applicative/issues/138))
opts :: Parser Opt
opts =
    Opt
        <$> optional
                (strOption (long "out" <> short 'o' <> metavar "TARGET" <> help "Target file path"))
        <*> subparser
                (  command "stamp"     (stampCmd `withInfo` "Add author's stamp to all elements. An element can only be provided with one author stamp, ie., existing author stamps are silently overwritten.")
                <> command "mark"      (markCmd `withInfo` "Ink scoped diagram objects")
                <> command "unmark"    (unmarkCmd `withInfo` "Unink all diagram objects")
                <> command "normalize" (normalizeCmd `withInfo` "Normalize model representation.")
                <> command "release"   (releaseCmd `withInfo` "Release a version")
                )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (opts <**> helper) (progDesc desc)

stampCmd :: Parser Cmd
stampCmd =
    StampCmd <$> argument str (metavar "SOURCE") <*>
    strOption (long "author" <> metavar "AUTHOR" <> help "Author's stamp")

markCmd :: Parser Cmd
markCmd =
    MarkCmd <$> argument str (metavar "SOURCE") <*>
    strOption
        (long "scope" <>
         short 's' <> metavar "SCOPE" <> help "Scope to colorize") <*>
    strOption
        (long "color" <> short 'c' <> metavar "COLOR" <> help "Colore to use")

unmarkCmd :: Parser Cmd
unmarkCmd = UnmarkCmd <$> argument str (metavar "SOURCE")

normalizeCmd :: Parser Cmd
normalizeCmd = NormalizeCmd <$> argument str (metavar "SOURCE")

releaseCmd :: Parser Cmd
releaseCmd =
    ReleaseCmd <$> argument str (metavar "SOURCE") <*>
    strOption
        (long "version" <>
         short 'v' <>
         metavar "VERSION" <>
         help
             "Version string (major.minor.patch) or Version component (major, minor, patch)")
