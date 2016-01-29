{-# LANGUAGE Trustworthy #-}

{-|
  Module      : Data.Memory.Types
  Copyright   : (c) 2016 David Farrell
  License     : Apache-2.0
  Stability   : unstable
  Portability : portable

  Memory bank system.
-}

module Data.Memory.Types where

import Data.Text (Text, empty)
import Data.Default
import qualified Data.Map as M

type NodeID = Integer
type NodeMap = M.Map NodeID Node

data NodeData = NodeData { nodeID      :: NodeID
                         , nextChildID :: NodeID
                         , nodeText    :: Text
                         } deriving Show

instance Default NodeData where def = NodeData { nodeID      = def
                                               , nextChildID = def
                                               , nodeText    = empty
                                               }

data Node = Node { nodeData :: NodeData
                 , nodeMap  :: NodeMap
                 } deriving Show

instance Default Node where def = Node { nodeData = def, nodeMap = def }
