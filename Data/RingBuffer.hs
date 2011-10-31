{-# LANGUAGE NoImplicitPrelude, RecordWildCards #-}
-- | A fixed-size ring buffer with amortized O(1) add and O(n) fold.
--
-- The buffer is implemented as two queues, one for flushing old
-- values and one for adding new ones.  When the out queue is empty
-- it's filled from the in queue.
module Data.RingBuffer (
  RingBuffer,
  new,
  add,
  add_,
  foldl,
  ) where

import qualified Data.List as L
import Prelude hiding (foldl)

data RingBuffer a = RingBuffer { outQueue :: [a]
                               , inQueue :: [a]
                               } deriving (Show)

-- | Create a new ring buffer initialized with size copies of one
-- vaue.
new :: Int -> a -> RingBuffer a
new size | size <= 0 = error "size must be positive"
         | otherwise = RingBuffer [] . replicate size

-- | Add a new value to the ring buffer, returning a flushed value and
-- the new buffer.  Since the buffer is fixed size, exactly one value
-- is flushed for every add.
add :: RingBuffer a -> a -> (a, RingBuffer a)
add RingBuffer{..} x | null outQueue = add (RingBuffer (reverse inQueue) []) x
                     | otherwise = let (y:yt) = outQueue
                                   in (y, RingBuffer yt (x:inQueue))

-- | Add a new value to the ring buffer, throwing away the flushed
-- value.
add_ :: RingBuffer a -> a -> RingBuffer a
add_ buff = snd . add buff

-- | Perform a fold over the values in the ring buffer.  The fold is
-- NOT necessarily in the order the values were added to the buffer.
foldl :: (a -> b -> a) -> a -> RingBuffer b -> a
foldl f z RingBuffer{..} = let z' = L.foldl' f z inQueue
                           in z' `seq` L.foldl' f z' outQueue
