{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Transformers
Description : n/a
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

XML transformers.
-}
module Transformers
    ( anno
    , mark
    , unmark
    , normalize
    , bump
    ) where

import           Data.Either
import           Data.List       hiding (delete, groupBy, insert)
import           Data.Map.Strict (Map, adjust, delete, fromList, insert, toList, (!?))
import           Data.Maybe
import           Data.SemVer
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.XML

import           Types
import           Utils

{--------------------------------------------------------------------------------------------------
  Annotation
---------------------------------------------------------------------------------------------------}

anno :: (Key, Val) -> Document -> Document
anno kv d@Document {..} = d { documentRoot = anno' kv documentRoot }

anno' :: (Key,Val) -> Element -> Element
anno' (k,v) e@(Element n as cs)
    | n `elem` [lblRoot, lblFolder, lblElement] =
        let cs' = elementNodes $ rmProp k e
         in Element n as $ prop k v : (lift (anno' (k,v)) <$> cs')
    | otherwise = Element n as (lift (anno' (k,v)) <$> cs)

{--------------------------------------------------------------------------------------------------
  Mark/ Unmark
---------------------------------------------------------------------------------------------------}

-- |Ink scoped diagram objects.
mark :: Text -> Text -> Document -> Document
mark scope color d@Document {..} = d { documentRoot = mark' (idx d) scope color documentRoot }

mark' :: Idx -> Val -> Text -> Element -> Element
mark' i s c e@(Element n@"child" as cs) =
    let inScope = (T.toLower s `elem`) <$> (askRef e >>= (!?) i >>= flip (!?) "scope")
     in if isJust inScope
            then Element n (insert "fillColor" c as) (lift (mark' i s c) <$> cs)
            else Element n as (lift (mark' i s c) <$> cs)
mark' i s c (Element n as cs) = Element n as (lift (mark' i s c) <$> cs)

-- |Remove mark from all diagram objects.
unmark :: Document -> Document
unmark d@Document{..} = d {documentRoot = unmark' documentRoot}

unmark' :: Element -> Element
unmark' (Element n as cs) = Element n (delete "fillColor" as) (lift unmark' <$> cs)

{--------------------------------------------------------------------------------------------------
  Normalization
---------------------------------------------------------------------------------------------------}

-- |Normalize.
-- - Attributes are lexicographically ordered
-- - Properties are lexicographically ordered
normalize :: Document -> Document
normalize d@Document {..} = d { documentRoot = normalize' documentRoot }

normalize' :: Element -> Element
normalize' (Element n as cs)
    | n == lblRoot = Element n (normAttrs as) (lift normalize' <$> normChilds cs)
    | otherwise = Element n (normAttrs as) (lift normalize' <$> normChilds cs)

normAttrs :: Map Name Text -> Map Name Text
normAttrs = fromList . sort . toList

-- Let's be minimal invasive! That is, instead of @sort act ++ pre ++ pos@ properties are /not/
-- structurally reordered, ie., they form /not/ the prefix in a child element sequence.
-- Note: @pre@ and @pos@ are lexicographically ordered by Archi!
normChilds :: [Node] -> [Node]
normChilds cs =
    let (pre, act, pos) = clip isProperty cs
     in pre ++ sort act ++ pos

{--------------------------------------------------------------------------------------------------
  Bump
---------------------------------------------------------------------------------------------------}

-- |Increment version.
bump :: (Version -> Version) -> Document -> Document
bump f d@Document{..} = d {documentRoot = bump' f documentRoot}

bump' :: (Version -> Version) -> Element -> Element
bump' f (Element n as cs) =
    Element
        n
        as
        ((\case
              NodeElement (Element "property" kv [])
                  | kv !? "key" == Just "version" ->
                      NodeElement
                          (Element
                               "property"
                               (adjust (either T.pack (toText . f) . fromText) "value" kv)
                               [])
              n -> n) <$>
         (lift (bump' f) <$> cs))
