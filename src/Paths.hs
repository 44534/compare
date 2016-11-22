{-# Language OverloadedStrings, BangPatterns, ScopedTypeVariables #-}
{-|
Module      : Paths
Description : generates all possible tool chains and translates a LTL formula 
Copyright   : Clara Waldmann, 2016

Generates all tool chains from given transformers.
Translates a LTL formula using these tool chains.
-}
module Paths(
    -- * Types
    WayGraph(..), Chain, Bunch(..), Bunchs, State,
    -- * Properties
    is_done, can_continue,
    -- * Helper
    buildGraph, initstate, testequiv,
    -- * Translations
    step, state_step,state_steps, allpaths, allpathswithmap
    ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.PatriciaTree
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import Data.Maybe
import Control.Monad.Trans.Except
import Control.Monad
import qualified Control.Exception as CE

import Control.Monad.IO.Class
import System.Exit
import System.Process.Text

import System.IO (hFlush, stdout, stderr, hPutStrLn, hClose)
import System.IO.Temp

import Language.LTL
import Transformer

-- | type for the graph between automata types
type WayGraph = Gr V Transformer

-- | builds the graph from the given transformers
buildGraph :: [Transformer] -> (WayGraph, M.Map V Node)
buildGraph trs =
    let nls = S.toList $ S.fromList $ concat $ [[source tr,target tr] | tr <- trs]
        ns = zip [0..] nls
        nm = M.fromList $ zip nls [0..]
        es = [(nm M.! source tr, nm M.! target tr, tr ) | tr <- trs]
    in (mkGraph ns es, nm)
    

type Chain = [Transformer]
-- | results stored in each node of the graph
data Bunch = Bunch 
    { object :: D        -- ^ the object (formula, automaton)
    , toolchain :: Chain -- ^ the tool chain the object was compouted by
    , complete :: Bool   -- ^ have all successors of this node been considered ?
    , steps_remaining :: Maybe Int -- ^ remaining number of steps
    }
type Bunchs = [Bunch]
-- | While translating a formula in each node the results are stored
type State = M.Map Node Bunchs

-- | the maximal length of the tool chain has not already been reached
can_continue :: Bunch -> Bool
can_continue b = case steps_remaining b of
                      Nothing -> True
                      Just s -> s > 0
                      
-- | computes the initial state where at the given node the set of objects to start with are stored
initstate :: WayGraph -- ^ graph
    -> Maybe Int      -- ^ maximal lenght of tool chains
    -> Node           -- ^ initial node
    -> S.Set D        -- ^ initial objects
    -> State          -- ^ initial state
initstate gr ml start set = M.fromList $ 
    [(start, [Bunch d [] False ml | d <- S.toList set])]
    
-- | Computes the direct successors of one node in the graph
-- if a transformer fails (timeout, failure or failure of check) no new result will be stored
step :: (D -> ExceptT Failure IO ()) -- ^ action for checks on the results (e.g. a call of 'testequiv') if this check fails, the tool chain is not successfull
    -> WayGraph -> Maybe Int -> State -> Node -> IO State
step checkact gr to st n = do
    res <- sequence $ do
            (q, tr) <- lsuc gr n
            bu@(Bunch d path False ms) <- filter (\b -> not (complete b) && (can_continue b)) $ M.findWithDefault [] n st
            guard $ path == [] || tr /= last path
            return $ do
                hPutStrLn stderr $ show $ path ++ [tr]
                md' <- runExceptT $ do
                        a <- apply tr d to
                        checkact a
                        return a
                case md' of
                     Left err -> do
                         hPutStrLn stderr $ show err
                         return Nothing
                     Right d' -> 
                        return $ Just (q, [Bunch d' (path ++ [tr]) False (ms >>= \s -> Just $ s-1) ])
    let nm = M.fromListWith (++) $ catMaybes res
    return $ M.unionWith (++) (M.adjust ((\b -> b{complete = True}) <$>) n st) nm
    
-- | computes the next state by applying 'step' to each node
state_step :: (D -> ExceptT Failure IO ()) -> WayGraph -> Maybe Int -> State  -> IO State
state_step checkact gr to st = foldM (step checkact gr to) st $ M.keys st
    
-- | A state is done if all stored bunches are completely processed
is_done :: State -> Bool
is_done st = all (complete) $ concat $ M.elems st
    
-- | applies 'state_step' until the state 'is_done'
state_steps checkact gr to st =
    if is_done st then return st
                  else do
                      nst <- state_step checkact gr to st
                      state_steps checkact gr to nst
    
-- | translates a formula along the tool chains resulting from the given transformers
-- (optionally calls 'testequiv' with the output of ltl2tgba --generic --deterministic -S on the formula)
allpaths :: Bool    -- ^ apply equivalence check using autfilt 
    -> [Transformer]-- ^ the transformers
    -> V            -- ^ VForm
    -> D            -- ^ the formula 
    -> Maybe Int    -- ^ timeout
    -> Maybe Int    -- ^ max. length of tool chains
    -> IO State     -- ^ the state after completely processing all tool chains, in each node all (intermediate) results are stored and can be evaulated
allpaths check trs vini d@(DForm fin) to sl = do
    let (gr, nm) = buildGraph trs
        sini = initstate gr sl (nm M.! vini) $ S.singleton d
        ns = topsort gr
    if check
       then withSystemTempFile "spot.hoaf" $
                \fp h -> do
                    (ExitSuccess ,out, _) <- readProcessWithExitCode 
                        "ltl2tgba" 
                        [ "--generic", "--deterministic", "-S" 
                        , "-F", "/dev/stdin"]
                        $ toText fin
                    -- autfilt does not work on automata having 'properties: terminal' 
                    let woterminal = T.unlines $ 
                            T.unwords <$> 
                                filter (/= "terminal") <$> 
                                    T.words <$> T.lines out
                    T.hPutStrLn h woterminal
                    hClose h
                    state_steps (\a -> testequiv fp a ) gr to sini
       else state_steps (\a -> return ()) gr to sini

-- | as 'allpaths' but returns the map from node to automaton type as well
allpathswithmap :: Bool -> [Transformer] -> V -> D -> Maybe Int -> Maybe Int -> IO (State, M.Map V Node)
allpathswithmap check trs vini d to sl = do
    let (gr, nm) = buildGraph trs
    s <- allpaths check trs vini d to sl
    return (s, nm)
    
-- | call autfilt --equivalent-to= filepath with the object (automaton)
-- if autfilt fails (exit code 1) this function will fail
-- (special case: autfilt exits with exit code 2 (too many acceptance sets) then this function will not fail)
testequiv :: FilePath -> D -> ExceptT Failure IO ()
testequiv _ (DSpinForm {}) = return ()
testequiv fp d = do
    let !tinp = case d of
        --    DForm f -> toText f
        --    DSpinForm f -> toText f
            DNBA a ->  toText a
            DDPA a -> toText a
            DDRA a -> toText a 
            foo -> error $ show foo
    eoe <- liftIO $ CE.try $ readProcessWithExitCode 
                        "autfilt" 
                        [ "--equivalent-to=" ++ fp
                        , "-F", "/dev/stdin"]
                        tinp
    case eoe of
        Right (ExitSuccess, _ , _) -> return ()
        Right (ExitFailure 1, _, _) -> throwE $ Error $ tinp <> "not equiv to " <> T.pack fp
        -- autfilt error: this implementation cannot support such a large number of acceptance sets
        -- is handled as successfull
        Right (ExitFailure 2, _, err) -> return () 
                    -- throwE $ Failure err
        Right (ec,_,_) -> throwE $ Error $ T.pack $ show ec  
        Left (e :: CE.SomeException) -> throwE $ Error $ T.pack $ show e
