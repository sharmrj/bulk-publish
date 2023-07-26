{-# LANGUAGE TupleSections #-}

module Utils
  ( concurrentlyLimited,
    writeLog,
  )
where

import Control.Concurrent.Async
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Data.Time.Clock

concurrentlyLimited :: Integer -> [IO a] -> IO [a]
concurrentlyLimited n tasks = conc n (zip [0 ..] tasks) [] []
  where
    conc :: Integer -> [(Integer, IO a)] -> [Async (Integer, a)] -> [(Integer, a)] -> IO [a]
    -- If there are no todos or ongoing tasks, give back the results (sorted to preserve order)
    conc _ [] [] results = return . map snd $ sortBy (comparing fst) results
    -- If there are no resources left, wait for a resource to become available
    conc 0 todo ongoing results = do
      (task, result) <- waitAny ongoing
      conc 1 todo (delete task ongoing) (result : results)
    -- if there are no tasks left
    conc _ [] ongoing results = conc 0 [] ongoing results
    -- Add a new task to the pool (should a resource be available)
    conc num ((i, x) : xs) ongoing results = withAsync ((i,) <$> x) (\task -> conc (num - 1) xs (task : ongoing) results)

writeLog :: String -> IO ()
writeLog m = do
  time <- getCurrentTime
  appendFile "bulk-publish-log.txt" (show time <> " " <> m <> "\n")
