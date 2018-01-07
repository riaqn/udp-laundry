{-# LANGUAGE ConstraintKinds #-}
-- modified version from stm-containers that supports adjunctive data
module Bimap
(
  Bimap,
  Association,
  Key,
  new,
  newIO,
  insert1,
  insert2,
  delete1,
  delete2,
  deleteAll,
  lookup1,
  lookup2,
  focus1,
  focus2,
  null,
  size,
  stream,
  findInsert1
)
where

import qualified Prelude as P 
import BasePrelude hiding (null)
import ListT hiding (null)

import qualified Focus
import qualified STMContainers.Map as Map
import Control.Monad.Trans.Class
import Data.Hashable
import Focus


-- |
-- A bidirectional map.
-- Essentially a bijection between subsets of its two argument types.
-- 
-- For one value of a left-hand type this map contains one value 
-- of the right-hand type and vice versa.
data Bimap a b c = 
  Bimap {m1 :: !(Map.Map a (b, c)), m2 :: !(Map.Map b (a, c))}
  deriving (Typeable)

-- |
-- A constraint for associations.
type Association a b = (Key a, Key b)

-- |
-- A constraint for keys.
type Key k = Map.Key k

-- |
-- Construct a new bimap.
{-# INLINABLE new #-}
new :: STM (Bimap a b c)
new = Bimap <$> Map.new <*> Map.new

-- |
-- Construct a new bimap in IO.
-- 
-- This is useful for creating it on a top-level using 'unsafePerformIO', 
-- because using 'atomically' inside 'unsafePerformIO' isn't possible.
{-# INLINABLE newIO #-}
newIO :: IO (Bimap a b c)
newIO = Bimap <$> Map.newIO <*> Map.newIO

-- |
-- Check on being empty.
{-# INLINABLE null #-}
null :: Bimap a b c -> STM Bool
null = Map.null . m1

-- |
-- Get the number of elements.
{-# INLINE size #-}
size :: Bimap a b c -> STM Int
size = Map.size . m1

-- |
-- Look up a right value by a left value.
{-# INLINABLE lookup1 #-}
lookup1 :: (Association a b) => a -> Bimap a b c -> STM (Maybe (b, c))
lookup1 k = Map.lookup k . m1

-- |
-- Look up a left value by a right value.
{-# INLINABLE lookup2 #-}
lookup2 :: (Association a b) => b -> Bimap a b c -> STM (Maybe (a, c))
lookup2 k = Map.lookup k . m2

-- |
-- Insert an association by a left value.
{-# INLINABLE insert1 #-}
insert1 :: (Association a b) => b -> c -> a -> Bimap a b c -> STM ()
insert1 b c a (Bimap m1 m2) = 
  do
    Map.insert (b, c) a m1
    Map.insert (a, c) b m2

-- |
-- Insert an association by a right value.
{-# INLINABLE insert2 #-}
insert2 :: (Association a b) => a -> c -> b -> Bimap a b c -> STM ()
insert2 b c a (Bimap m1 m2) = (inline insert1) b c a (Bimap m2 m1)

-- |
-- Delete an association by a left value.
{-# INLINABLE delete1 #-}
delete1 :: (Association a b) => a -> Bimap a b c -> STM ()
delete1 k (Bimap m1 m2) =
  Map.focus lookupAndDeleteStrategy k m1 >>= 
    mapM_ (\(k', c) -> Map.delete k' m2)
  where
    lookupAndDeleteStrategy r =
      return (r, Focus.Remove)

-- |
-- Delete an association by a right value.
{-# INLINABLE delete2 #-}
delete2 :: (Association a b) => b -> Bimap a b c -> STM ()
delete2 k (Bimap m1 m2) = (inline delete1) k (Bimap m2 m1)

-- |
-- Delete all the associations.
{-# INLINE deleteAll #-}
deleteAll :: Bimap a b c -> STM ()
deleteAll (Bimap m1 m2) =
  do
    Map.deleteAll m1
    Map.deleteAll m2

-- |
-- Focus on a right value by a left value with a strategy.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINABLE focus1 #-}
focus1 :: (Association a b) => Focus.StrategyM STM (b, c) r -> a -> Bimap a b c -> STM r
focus1 s a (Bimap m1 m2) =
  do 
    (r, d, mb) <- Map.focus s' a m1
    case d of
      Focus.Keep -> 
        return ()
      Focus.Remove -> 
        forM_ mb $ \(b, c) -> Map.delete b m2
      Focus.Replace (b', c') ->
        do
          forM_ mb $ \(b, c) -> Map.delete b m2
          Map.insert (a, c') b' m2
    return r
  where
    s' = \k -> s k >>= \(r, d) -> return ((r, d, k), d)

-- |
-- Focus on a left value by a right value with a strategy.
-- 
-- This function allows to perform composite operations in a single access
-- to a map item.
-- E.g., you can look up an item and delete it at the same time,
-- or update it and return the new value.
{-# INLINABLE focus2 #-}
focus2 :: (Association a b) => Focus.StrategyM STM (a, c) r -> b -> Bimap a b c -> STM r
focus2 s b (Bimap m1 m2) = (inline focus1) s b (Bimap m2 m1)

-- |
-- Stream associations.
-- 
-- Amongst other features this function provides an interface to folding 
-- via the 'ListT.fold' function.
{-# INLINE stream #-}
stream :: Bimap a b c -> ListT STM (a, (b, c))
stream = Map.stream . m1

findInsert1 :: (Num a, Eq a, Eq b, Hashable a, Hashable b) => b  -> c  -> TVar a -> Bimap a b c -> IO a
findInsert1 b c seq m = do
  a <- atomically $ do
    a <- readTVar seq
    writeTVar seq $ a + 1
    return a
  r <- atomically $ Bimap.focus1 (\m -> case m of
                        Nothing -> return (Just a, Replace (b, c))
                        Just _ -> return (Nothing, Keep)
                    ) a m
  case r of
    Nothing -> findInsert1 b c seq m
    Just a -> return a
