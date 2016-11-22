{-|
Module      : ComCon
Description : Running actions Concurrently 
Copyright   : Clara Waldmann, 2016

A library for running actions in parallel on multiple processor cores.
-}

module ComCon where

import Control.Concurrent.Async (async, wait)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, forM, forM_, when)

-- | run action on the given number of cores concurrently
-- (blocks: all cores have to finish before next actions are taken)
pforever p action = forever $ do
   as <- forM [1..p] $ \_ -> async action
   forM_ as wait

-- | run the action on the given number of cores forever (not blocking)
pforever' :: Int -> IO a -> IO ()
pforever' p action = do
    pfor p $ repeat action
    
-- | run the actions on the given number of cores
-- If a core finished an action it picks the next (if there is any left) thus all cores will be busy as long as there are actions left.
pfor :: Int -> [IO a] -> IO ()
pfor p actions = do
    queue <- STM.atomically $ STM.newTVar actions
    pids <- forM [1..p] $ \_ -> async $ do
        let go = do
                mact <- STM.atomically $ do
                            actions <- STM.readTVar queue
                            case actions of
                                 [] -> return Nothing
                                 (a:ctions) -> do
                                     STM.writeTVar queue ctions
                                     return $ Just a
                case mact of
                     Nothing -> return ()
                     Just act -> do act; go
        go
    forM_ pids wait
    
-- | apply function before running 'pfor'
--
-- > pforM_ p xs f = pfor p (f <$> xs)
pforM_ p xs f = pfor p (f <$> xs)
