{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}

module Main where

import           Args

main:: IO ()
main = runWithArgs $ \args@Args{..} -> do
  putStrLn $ show args
