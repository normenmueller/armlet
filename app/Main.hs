{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Main
Description : Main entry point
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

Armlet entry point.
-}
module Main where

import           Control.Monad       ((=<<))
import           Data.SemVer
import           Data.Text
import           Options.Applicative

import           Parsers
import           Renderer
import           Transformers

{--------------------------------------------------------------------------------------------------
  Entry
---------------------------------------------------------------------------------------------------}

main :: IO ()
main = armlet =<< cmdln

{--------------------------------------------------------------------------------------------------
  Punk
---------------------------------------------------------------------------------------------------}

armlet :: Opt -> IO ()
armlet (Opt to cmd) = render to =<< case cmd of
    (StampCmd from author    ) -> anno ("author", pack author) <$> parse from
    (MarkCmd from scope color) -> mark (pack scope) (pack color) <$> parse from
    (UnmarkCmd    from       ) -> unmark <$> parse from
    (NormalizeCmd from       ) -> normalize <$> parse from
    (ReleaseCmd from vc      ) -> case vc of
        "major" -> bump incrementMajor <$> parse from
        "minor" -> bump incrementMinor <$> parse from
        "patch" -> bump incrementPatch <$> parse from
        v       -> case fromText . pack $ v of
            Left  err -> error err
            Right v   -> anno ("version", toText v) <$> parse from
