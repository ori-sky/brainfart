{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Default
import Data.Memory.Types
import Data.Memory.DB.Acid
import Text.Blaze.Html5 (Html, (!), toHtml)
import Clay ((?))
import Happstack.Foundation
import qualified Data.Map as M
import qualified Data.Text.Lazy.Encoding as LazyT
import qualified Data.ByteString.Char8 as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Clay as C

instance PathInfo a => PathInfo [a] where
    toPathSegments = concat . fmap toPathSegments
    fromPathSegments = many fromPathSegments

instance ToMessage C.Css where
    toContentType _ = B.pack "text/css; charset=UTF-8"
    toMessage = LazyT.encodeUtf8 . C.render

data Route = Search
           | Css
           | NewNode [NodeID] Text
           | ViewNode [NodeID]

$(derivePathInfo ''Route)

type Memory'    = FoundationT' Route MemoryAcid () IO
type Memory     = XMLGenT Memory'
type MemoryForm = FoundationForm Route MemoryAcid () IO

instance (Functor m, Monad m) => HasAcidState (FoundationT' url MemoryAcid s m) Node where
    getAcidState = acidMemory <$> getAcidSt

outputNode :: [NodeID] -> Node -> Html
outputNode xs (Node nData nMap) = H.div ! A.class_ "box" $ do
    H.a ! A.href href $ toHtml (nodeText nData)
    traverse_ f (M.toAscList nMap)
  where href = H.toValue $ toPathInfo (ViewNode xs)
        f (nnID, nn) = outputNode (xs ++ [nnID]) nn

route :: Route -> Memory Response
route Search = ok $ toResponse $ template "Search" $ H.h1 "Search"
route (NewNode xs text) = update (UInsertNode node xs) >>= \case
    Nothing   -> internalServerError $ toResponse $ template "Internal Server Error" $ "Failed to insert node"
    Just node -> ok                  $ toResponse $ template "New Node"  $ H.pre $ outputNode xs node
  where node = def { nodeData = def { nodeText = text } }
route (ViewNode xs) = query (QLookupNode xs) >>= \case
    Nothing   -> internalServerError $ toResponse $ template "Internal Server Error" $ "Failed to lookup node"
    Just node -> ok                  $ toResponse $ template "View Node" $ H.pre $ outputNode xs node
route Css = ok (toResponse css)

main :: IO ()
main = withAcid $ \root -> do
    simpleApp id defaultConf (AcidUsing root) () Search "" route

template :: Text -> Html -> Html
template title body = H.docTypeHtml $ do
    H.head $ do
        H.title (toHtml title)
        H.link
            ! A.type_ "text/css"
            ! A.rel "stylesheet"
            ! A.href (H.toValue $ toPathInfo Css)
    H.body $ do
        body

css :: C.Css
css = do
    ".box" ? do
        C.padding (C.px 3) (C.px 5) (C.px 3) (C.px 5)
        C.background (C.rgba 20 24 30 10)
