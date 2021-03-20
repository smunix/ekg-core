{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Perform 100,000 atomic sample additions using 100 concurrent
-- writers.
module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Data.Int
import Data.Kind
import qualified Gauge as G
import qualified System.Metrics.Counter as C
import qualified System.Metrics.Distribution as D

class New a where
  type Add a :: Type
  new :: IO a
  add :: a -> Add a

add' :: New a => a -> Add a
add' = add
{-# INLINEABLE add' #-}

instance New D.Distribution where
  type Add D.Distribution = Double -> IO ()
  new = D.new
  add = D.add

instance New C.Counter where
  type Add C.Counter = Int64 -> IO ()
  new = C.new
  add = C.add

main :: IO ()
main = do
  maxCap <- getNumCapabilities
  G.defaultMain
    [ G.bgroup "distribution" [bmark @D.Distribution maxCap (`add` 1.0)],
      G.bgroup "counter" [bmark @C.Counter maxCap (`add` 1)]
    ]
  where
    iters = 100000

    work :: forall a. (New a) => a -> (a -> IO ()) -> Int -> MVar () -> IO ()
    work _ _ 0 !lock = putMVar lock ()
    work !v act !i lock = act v >> work v act (i - 1) lock

    bmark :: forall a. (NFData a, New a) => Int -> (a -> IO ()) -> G.Benchmark
    bmark !cap act =
      G.env
        ((,) <$> new @a <*> replicateM cap newEmptyMVar)
        \ ~(distrib, locks) -> G.bench "G.env" $ G.nfIO do
          mapM_ (forkIO . work distrib act iters) locks
          mapM_ takeMVar locks
