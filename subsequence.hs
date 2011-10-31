{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.RingBuffer (RingBuffer)
import qualified Data.RingBuffer as R
import System.Console.CmdArgs

data SubSequence = SubSequence deriving (Show, Data, Typeable)

subsequence :: SubSequence
subsequence = SubSequence
  &= summary "SubSequence 0.1 (c) 2011 Itai Zukerman"
  &= help "Find the longest increasing subsequence in data passed on stdin."

-- | Do a space-efficient search for a maximal element.
--
-- Dynamic programming is an easy approach to getting a longest
-- increasing subsequnce, but is O(n^2).  Keeping a buffer of the last
-- k items makes this O(n*k) at the expense of maybe missing the
-- subsequence.
--
-- Note: Is this a kind of fold?
search :: Ord b => Int -> (a -> RingBuffer b -> b) -> [a] -> b -> b
search depth f xs z = loop xs (R.new depth z) z
  where
    loop [] _ m = m
    loop (x:xt) !buff !m =
      let y = f x buff
      in y `seq` loop xt (buff `R.add_` y) (max y m)

type S = ByteString

-- | An increasing sequence of strings with fast access to the size of
-- the sequence.
data Seq = Seq { count :: !Int
               , path :: [S]
               } deriving (Eq, Ord, Show)

zero :: Seq
zero = Seq 0 []

largest :: S -> Seq -> Seq -> Seq
largest s !old@(Seq c _) !new@(Seq c1 p1) | (c < c1) && (s > head p1) = new
                                          | otherwise = old

-- | Scan over a buffer of largest increasing sequences seen so far
-- and try to extend them with a new string.
maximize :: S -> RingBuffer Seq -> Seq
maximize s buff = let Seq q1 s1 = R.foldl (largest s) zero buff
                  in Seq (q1+1) (s:s1)

lines10 :: ByteString -> [ByteString]
lines10 = B.split 10

main :: IO ()
main = do
  _ <- cmdArgs subsequence
  emails <- lines10 <$> B.getContents
  let top = search 1000  maximize emails zero
  forM_ (reverse $ path top) B.putStrLn
