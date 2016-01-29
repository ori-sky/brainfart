{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List (intercalate, genericReplicate)
import Data.Text (Text)
import Data.Default
import qualified Data.Map as M
import Text.Blaze.Html5 (Html, (!), toHtml)
import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5 as H
import Happstack.Foundation
import Data.Memory.Types
import Data.Memory.DB.Acid

instance PathInfo a => PathInfo [a] where
    toPathSegments = concat . fmap toPathSegments
    fromPathSegments = many fromPathSegments

data Route = Search
           | NewNode [NodeID] Text
           | ViewNode [NodeID]

type Memory'    = FoundationT' Route MemoryAcid () IO
type Memory     = XMLGenT Memory'
type MemoryForm = FoundationForm Route MemoryAcid () IO

instance (Functor m, Monad m) => HasAcidState (FoundationT' url MemoryAcid s m) Node where
    getAcidState = acidMemory <$> getAcidSt

$(derivePathInfo ''Route)

template :: Text -> Html -> Html
template title body = H.docTypeHtml $ do
    H.head $ do
        H.title (toHtml title)
    H.body $ do
        body

outputNode :: Integer -> Node -> [String]
outputNode indent (Node nData nMap) = this : rest
  where this = genericReplicate indent ' ' ++ show (nodeText nData)
        rest = concatMap (outputNode (indent + 2) . snd) (M.toAscList nMap)

route :: Route -> Memory Response
route Search = ok $ toResponse $ template "Search" $ H.h1 "Search"
route (NewNode xs text) = update (UInsertNode node xs) >>= \case
    Nothing   -> internalServerError $ toResponse $ template "Internal Server Error" $ "Failed to insert node"
    Just node -> ok                  $ toResponse $ template "New Node"  $ H.pre $ toHtml $ intercalate "\n" (outputNode 0 node)
  where node = def { nodeData = def { nodeText = text } }
route (ViewNode xs) = query (QLookupNode xs) >>= \case
    Nothing   -> internalServerError $ toResponse $ template "Internal Server Error" $ "Failed to lookup node"
    Just node -> ok                  $ toResponse $ template "View Node" $ H.pre $ toHtml $ intercalate "\n" (outputNode 0 node)

main :: IO ()
main = withAcid $ \root -> do
    simpleApp id defaultConf (AcidUsing root) () Search "" route
