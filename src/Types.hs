{-|
Module      : Types
Description : n/a
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Types where

import           Data.Map.Strict       (Map)
import           Data.Text             (Text)
import           Text.XML

{--------------------------------------------------------------------------------------------------
  Type aliases
---------------------------------------------------------------------------------------------------}

type ID = Text

type Key = Text

type Val = Text

type Props = Map Key [Val]

type Idx = Map ID Props
