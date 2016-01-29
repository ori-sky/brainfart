{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List (intercalate, genericReplicate)
import Data.Text (Text)
import Data.Default
import qualified Data.Map as M
import Data.Acid.Local (createCheckpointAndClose)
import Control.Exception (bracket)
import Text.Blaze.Html5 (Html, (!), toHtml)
import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5 as H
import Happstack.Foundation

instance PathInfo a => PathInfo [a] where
    toPathSegments = concat . fmap toPathSegments
    fromPathSegments = many fromPathSegments

type NodeID = Integer
type NodeMap = M.Map NodeID Node

data NodeData = NodeData { nodeID      :: NodeID
                         , nextChildID :: NodeID
                         , text        :: Text
                         } deriving Show

instance Default NodeData where def = NodeData { nodeID      = def
                                               , nextChildID = def
                                               , text        = ""
                                               }

data Node = Node { nodeData :: NodeData
                 , nodeMap  :: NodeMap
                 } deriving Show

instance Default Node where def = Node { nodeData = def, nodeMap = def }

data Route = Search
           | NewNode [NodeID]
           | ViewNode [NodeID]

type Memory'    = FoundationT' Route MemoryAcid () IO
type Memory     = XMLGenT Memory'
type MemoryForm = FoundationForm Route MemoryAcid () IO

newtype MemoryAcid = MemoryAcid { acidMemory :: AcidState Node }
instance (Functor m, Monad m) => HasAcidState (FoundationT' url MemoryAcid s m) Node where
    getAcidState = acidMemory <$> getAcidSt

insertNode :: Node -> [NodeID] -> Node -> Maybe Node
insertNode node@(Node nd _) [] current@(Node cData cMap) = pure newCurrent
  where nextID = nextChildID cData
        newNode = node { nodeData = nd { nodeID = nextID } }
        newCurrent = current { nodeData = cData { nextChildID = succ nextID }
                             , nodeMap = M.insert nextID newNode cMap }
insertNode node (x:xs) current@(Node _ cMap) = case M.lookup x cMap of
    Just child -> do
        newChild <- insertNode node xs child
        pure current { nodeMap = M.insert x newChild cMap }
    Nothing    -> Nothing

uInsertNode :: Node -> [NodeID] -> Update Node (Maybe Node)
uInsertNode node xs = insertNode node xs <$> get >>= \case
    Just newNode -> put newNode >> pure (pure newNode)
    Nothing      -> pure Nothing

lookupNode :: [NodeID] -> Node -> Maybe Node
lookupNode [] _              = Nothing
lookupNode (x:[]) (Node _ m) = M.lookup x m
lookupNode (x:xs) (Node _ m) = M.lookup x m >>= lookupNode xs

qLookupNode :: [NodeID] -> Query Node (Maybe Node)
qLookupNode [] = pure <$> ask
qLookupNode xs = lookupNode xs <$> ask

$(deriveSafeCopy 0 'base ''NodeData)
$(deriveSafeCopy 0 'base ''Node)
$(derivePathInfo ''Route)
$(makeAcidic ''Node ['uInsertNode, 'qLookupNode])

withAcid :: (MemoryAcid -> IO a) -> IO a
withAcid f = bracket (openLocalState def) createCheckpointAndClose (f . MemoryAcid)

template :: Text -> Html -> Html
template title body = H.docTypeHtml $ do
    H.head $ do
        H.title (toHtml title)
    H.body $ do
        body

outputNode :: Integer -> Node -> [String]
outputNode indent (Node nData nMap) = this : rest
  where this = genericReplicate indent ' ' ++ show (nodeID nData)
        rest = concatMap (outputNode (indent + 2) . snd) (M.toAscList nMap)

route :: Route -> Memory Response
route Search        = ok $ toResponse $ template "Search" $ H.h1 "Search"
route (NewNode xs)  = update (UInsertNode def xs) >>= \case
    Nothing   -> internalServerError $ toResponse $ template "Internal Server Error" $ "Failed to insert node"
    Just node -> ok                  $ toResponse $ template "New Node"  $ H.pre $ toHtml $ intercalate "\n" (outputNode 0 node)
route (ViewNode xs) = query (QLookupNode xs) >>= \case
    Nothing   -> internalServerError $ toResponse $ template "Internal Server Error" $ "Failed to lookup node"
    Just node -> ok                  $ toResponse $ template "View Node" $ H.pre $ toHtml $ intercalate "\n" (outputNode 0 node)

main :: IO ()
main = withAcid $ \root -> do
    simpleApp id defaultConf (AcidUsing root) () Search "" route
