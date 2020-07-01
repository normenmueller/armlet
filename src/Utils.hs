{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Utils
Description : n/a
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

...
-}
module Utils where

import           Control.Applicative
import           Data.List           hiding (delete, group, groupBy)
import           Data.Map.Strict     (Map, adjust, delete, (!?))
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.XML.Types      as X
import           Text.XML

import Types

{--------------------------------------------------------------------------------------------------
  XML fixed slots
---------------------------------------------------------------------------------------------------}

lblRoot = Name "model" (Just "http://www.archimatetool.com/archimate") (Just "archimate")

lblFolder = Name "folder" Nothing Nothing

lblElement = Name "element" Nothing Nothing

--lblType = Name "type" (Just "http://www.w3.org/2001/XMLSchema-instance") (Just "xsi")

{--------------------------------------------------------------------------------------------------
  XML predicates
---------------------------------------------------------------------------------------------------}

isProperty :: Node -> Bool
isProperty n  = n `hasLabel` "property"

isFolder :: Node -> Bool
isFolder n  = n `hasLabel` "folder"

hasLabel :: Node -> Name -> Bool
hasLabel (NodeElement (Element na _ _)) nb = na == nb
hasLabel _ _ = False

{--------------------------------------------------------------------------------------------------
  XML creators
---------------------------------------------------------------------------------------------------}

prop :: Key -> Val -> Node
prop k v = NodeElement $ Element "property" (Map.fromList [("key", k), ("value", v)]) []

{--------------------------------------------------------------------------------------------------
  XML accessors
---------------------------------------------------------------------------------------------------}

askIdn :: Element -> Maybe ID
askIdn = askAttr "id"

askRef :: Element -> Maybe ID
askRef = askAttr "archimateElement"

askAttr :: Name -> Element -> Maybe Text
askAttr n (Element _ as _) =  as !? n

askProp :: Key -> Element -> Maybe Val
askProp k (Element n as cs) =
    foldr
        (\n acc ->
             (case n of
                  NodeElement (Element "property" kv []) ->
                      if (kv !? "key") == Just k
                          then as !? "value"
                          else Nothing
                  _ -> Nothing) <|>
             acc)
        Nothing
        cs

{--------------------------------------------------------------------------------------------------
  XML modifiers
---------------------------------------------------------------------------------------------------}

rmAttr :: Name -> Node -> Node
rmAttr a (NodeElement (Element n as cs)) = NodeElement (Element n (delete a as) cs)
rmAttr _ n = n

rmProp :: Key -> Element -> Element
rmProp k (Element n as cs) = Element n as $ rmProp' k cs

rmProp' :: Key -> [Node] -> [Node]
rmProp' k =
    foldr
        (\n acc ->
             case n of
                 NodeElement (Element "property" kv []) ->
                     if (kv !? "key") /= Just k
                         then n : acc
                         else acc
                 _ -> n : acc)
        []

-- |Squash properties.
-- XXX assign dedicated namespace to props to be squashed
sqProps :: [Node] -> [Node]
sqProps [] = []
sqProps [x] = [x]
sqProps (NodeElement (Element "property" las []):NodeElement (Element "property" ras []):tl) =
    case (las !? "key", ras !? "key") of
        (Just lk, Just rk)
            | lk == rk &&
                  lk `elem` ["status", "author", "version", "relates-to", "context", "scope"] ->
                let las' =
                        adjust
                            (\v ->
                                 T.concat . intersperse ";" . sort $
                                 fromMaybe "" (ras !? "value") : T.splitOn ";" v)
                            "value"
                            las
                 in sqProps $ NodeElement (Element "property" las' []) : tl
        _ ->
            NodeElement (Element "property" las []) :
            sqProps (NodeElement (Element "property" ras []) : tl)
sqProps (hd:tl) = hd : sqProps tl

{--------------------------------------------------------------------------------------------------
  XML helpers
---------------------------------------------------------------------------------------------------}

idx :: Document -> Idx
idx (Document prologue root epilogue) = idx' root

idx' :: Element -> Idx
idx' e@(Element n _ cs) = foldl op zero cs
  where
    zero =
        case askIdn e of
            Just id -> Map.singleton id (Map.fromList . group $ cs >>= maybeToList . prp)
            Nothing -> Map.empty
    op m =
        \case
            (NodeElement e') -> Map.union m (idx' e')
            _ -> m
    prp =
        \case
            (NodeElement (Element "property" kv [])) ->
                case Map.toList kv of
                    [(_, k), (_, v)] -> Just (k, T.toLower v)
                    _ -> Nothing
            _ -> Nothing

lift :: (Element -> Element) -> Node -> Node
lift f (NodeElement e) = NodeElement $ f e
lift _ n = n

{--------------------------------------------------------------------------------------------------
  Commons
---------------------------------------------------------------------------------------------------}

clip :: (a -> Bool) -> [a] -> ([a], [a], [a])
clip p =
    foldr
        (\n (pre, act, pos) ->
             if p n
                then (pre, n : act, pos)
                else
                    if null act
                        then (n : pre, act, pos)
                        else (pre, act, n : pos))
        ([], [], [])

--sortClipOn :: Ord b => (a -> Bool) -> (a -> b) -> [a] -> [a]
--sortClipOn cf sf as =
--    let (pre, act, pos) = clip cf as
--        act' = sortOn sf act
--     in pre ++ act' ++ pos

--sortOnIdx :: Ord a => (b -> (a,b)) -> [b] -> [b]
--sortOnIdx idx = concatMap snd . sortOn fst . groupBy idx

-- |Collect snd by fst
group :: Ord a => [(a, b)] -> [(a, [b])]
group = merge []

groupBy :: Ord a => (b -> (a, b)) -> [b] -> [(a, [b])]
groupBy idx = group . fmap idx

merge :: Ord a => [(a, b)] -> [(a, b)] -> [(a, [b])]
merge l = mrg (l >>= \(k, v) -> [(k, [v])])
  where
    mrg l [] = l
    mrg l ((k, v):r) =
        mrg
            (case lookup k l of
                 Just vs -> Map.toList $ Map.adjust (v :) k (Map.fromList l)
                 Nothing -> (k, [v]) : l)
            r
