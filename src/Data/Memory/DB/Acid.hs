{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
  Module      : Data.Memory.Types
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : non-portable (GHC extensions)

  Memory bank system.
-}

module Data.Memory.DB.Acid where

import Data.Memory.Types
import Data.Default
import Data.SafeCopy
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import qualified Data.Map as M
import Control.Exception (bracket)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

newtype MemoryAcid = MemoryAcid { acidMemory :: AcidState Node }

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
$(makeAcidic ''Node ['uInsertNode, 'qLookupNode])

withAcid :: (MemoryAcid -> IO a) -> IO a
withAcid f = bracket (openLocalState def) createCheckpointAndClose (f . MemoryAcid)
