{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Data.Aeson.Functions
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Internal.Functions
    ( mapTextKeyVal
    , mapKeyVal
    , mapKey
    ) where

import Prelude.Compat

import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M

-- | Transform a 'M.Map' into a 'KM.KeyMap' while transforming the keys.
mapTextKeyVal :: (k -> Key) -> (v1 -> v2)
              -> M.Map k v1 -> KM.KeyMap v2
mapTextKeyVal fk kv = M.foldrWithKey (\k v -> KM.insert (fk k) (kv v)) KM.empty
{-# INLINE mapTextKeyVal #-}

-- | Transform the keys and values of a 'M.Map'.
mapKeyVal :: (Eq k2, Ord k2) => (k1 -> k2) -> (v1 -> v2)
          -> M.Map k1 v1 -> M.Map k2 v2
mapKeyVal fk kv = M.foldrWithKey (\k v -> M.insert (fk k) (kv v)) M.empty
{-# INLINE mapKeyVal #-}

-- | Transform the keys of a 'M.Map'.
mapKey :: (Eq k2, Ord k2) => (k1 -> k2) -> M.Map k1 v -> M.Map k2 v
mapKey fk = mapKeyVal fk id
{-# INLINE mapKey #-}
