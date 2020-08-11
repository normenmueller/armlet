{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Renderer
Description : n/a
Copyright   : (c) Normen Müller, 2020
License     : BSD3
Maintainer  : normen.mueller@gmail.com
Stability   : experimental
Portability : POSIX

XML rendering.
-}
module Renderer
    ( render
    , render'
    ) where

import           Data.Maybe
import qualified Data.Text.Lazy        as TL
import           Prelude               hiding (writeFile)
import           Text.XML
import           Text.XML.Stream.Parse

render :: Maybe FilePath -> Document -> IO ()
render = maybe (print . render') (writeFile dft)

render' :: Document -> TL.Text
render' = renderText dft

-- Note: `rsPretty` disfigures formatting of documentation, ie., for example,
-- newlines are silently removed.
dft =
    def
        { rsPretty = True
        , rsNamespaces = [("xsi", "http://www.w3.org/2001/XMLSchema-instance")]
        }

-- @rsAttrOrder@ clashes with @Transformers.normalize@.
-- def
--     { rsAttrOrder =
--           orderAttrs
--               [ ("folder", ["id", "name", "type"])
--               , ( "element"
--                 , [ "id"
--                   , "name"
--                   , Name
--                         "type"
--                         (Just "http://www.w3.org/2001/XMLSchema-instance")
--                         (Just "xsi")
--                   ])
--               ]
--     , rsNamespaces = [("xsi", "http://www.w3.org/2001/XMLSchema-instance")]
--     }
