{-# Language OverloadedStrings, LambdaCase #-}
{-|
Module      : Compare
Description : Comparing different translations
Copyright   : Clara Waldmann, 2016

Generating results in CSV format.
-}

module Compare where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Data.Function
import qualified Data.Text as T
import Control.Monad (forM_)
import System.IO (stderr, hPutStrLn)

import OmegaAutomata.Automata as A
import Paths
import Transformer
import Language.LTL


-- | Replaces \',\' by \'-\'
-- (to avoid unwanted separation in CSV format).
unkomma = map (\ case
    ',' -> '-'
    c -> c)

-- | Generates a result line in CSV format:
-- formula, tool chain, nr. states, size of acceptance condition
values :: Acceptance acc => Formula -> A.Automat q a l acc -> [Transformer] -> [T.Text]
values f a ch = 
    [toText f, (T.pack $ unkomma $ show ch), ( T.pack $ show $ S.size $ states a), (T.pack $ show $ A.size $ accept a)]
        
-- | Generates result CSV for one formula.
--
-- Runs the given transformers and generates result lines for resulting parity automata
allpathsvalues :: Bool -- ^ Apply equivalence check using autfilt
    -> [Transformer] 
    -> Maybe Int       -- ^ timeout in seconds
    -> Maybe Int       -- ^ maximal length of tool chain
    -> Formula 
    -> IO [[T.Text]]
allpathsvalues check trs mto msl f = do
    (s, nm) <- allpathswithmap check trs VForm ( DForm f ) mto msl
    let res = maybe [] id $ do
            x <- M.lookup VDPA nm
            M.lookup x s
    hPutStrLn stderr $ replicate 50 '*'
    return $ do
        Bunch (DDPA a) ch _ _ <- res
        return $ values f a ch
        
-- | Same as 'allpathsvalues'. Additionally it is computed if the minimal number of states is at least the given number.
allpathsvalues_upto :: Int -> Bool -> [Transformer] -> Maybe Int -> Maybe Int -> Formula 
    -> IO (Bool,[[T.Text]])
allpathsvalues_upto end check trs mto msl f = do
    (s, nm) <- allpathswithmap check trs VForm ( DForm f ) mto msl
    let res = maybe [] id $ do
            x <- M.lookup VDPA nm
            M.lookup x s
        mins = minimum $ (\b -> let DDPA a = object b
                                in S.size $ states $ a ) <$> res
    hPutStrLn stderr $ replicate 50 '*'
    return ( mins >= end , do
        Bunch (DDPA a) ch _ _ <- res
        return $ values f a ch
         )
         
-- | Same as 'allpathsvalues' but 
-- generates result lines for parity automata and Rabin automata.
rabinreducevalues :: Bool -> [Transformer] -> Maybe Int -> Maybe Int -> Formula 
    -> IO [[T.Text]]
rabinreducevalues check trs mto msl f = do
    (s, nm) <- allpathswithmap check trs VForm ( DForm f ) mto msl
    let [dras, dpas] =
            (\t -> maybe [] id $ do
                    x <- M.lookup t nm
                    M.lookup x s
            ) <$> [VDRA,VDPA]  
    hPutStrLn stderr $ replicate 50 '*'
    return $ (do
                Bunch (DDRA a) ch _ _ <- dras
                return $ values f a ch
             ) ++ 
             (do
                Bunch (DDPA a) ch _ _ <- dpas
                return $ values f a ch
             )

-- | Same as 'allpathsvalues'. Additionally it is computed if all paths succeeded.
allpathssuccess :: Bool -> [Transformer] -> Maybe Int -> Maybe Int -> Formula 
    -> IO (Bool, [[T.Text]])
allpathssuccess check trs mto msl f = do
    let (gr, nm) = buildGraph trs
    s <- allpaths check trs VForm ( DForm f ) mto msl
    let dpanode = nm M.! VDPA
        mres = M.lookup dpanode s
    case mres of
         Nothing -> return (False,[])
         Just res -> do
            let succpaths = toolchain <$> res
                allpaths = pathsbetween trs VForm VDPA
                allsucc = null (allpaths L.\\ succpaths )
--             hPutStrLn stderr $ show $ paths trs VForm
--             hPutStrLn stderr $ show succpaths
--             hPutStrLn stderr $ show allpaths
--             hPutStrLn stderr $ show allsucc
            return (allsucc, do
                Bunch (DDPA a) ch _ _ <- res
                return $ [toText f, (T.pack $ unkomma $ show ch), ( T.pack $ show $ S.size $ states a), (T.pack $ show $ A.size $ accept a)] )
    
              
-- | Compares paths using transformers that have a property (trs_in) to other transformers.
-- 
-- Compares the resulting automata on the minimal number of states computed by tool chains using transformers from trs_in (min_in) and by tool chains not using transformers from trs_in (min_notin).
--
-- Checks if one of the tool chains using one of the transformers from trs_in computes a smaller automaton than every tool chain not using transformers from trs_in (min_in < min_notin).
--
-- Shows the minimal number of states (min_in, min_notin) and one of the tool chains achieving it (t_min_in, t_min_notin) for each of the two groups of transformers.
--
-- Computes the quotient of the minimal number of states (min_in / min_notin).
allpathseval :: Bool 
    -> [Transformer] 
    -> Maybe Int 
    -> Maybe Int 
    -> (Transformer -> Bool) -- ^ property 
    -> Formula 
    -> IO ( Bool, [T.Text]) -- ^ (min_in < min_notin, [min_in / min_notin, formula, min_in, t_min_in, min_notin, t_min_notin] )
          
allpathseval check trs mto msl pin f = do
    (s, nm) <- allpathswithmap check trs VForm ( DForm f ) mto msl
    let res = s M.! (nm M.! VDPA)
    hPutStrLn stderr $ replicate 50 '*'
    forM_ res (\(Bunch (DDPA a) ch _ _) -> do
                 hPutStrLn stderr $ (show ch) ++ ( show $ S.size $ states a)
              )
    let (trs_in, trs_notin) = L.partition (\b -> any pin $ toolchain b) res 
        best xs = case xs of
                       [] -> (Nothing ,"")
                       xs -> let Bunch (DDPA d) p _ _ =
                                    L.minimumBy ( compare `on` \ (Bunch (DDPA a) _ _ _) -> S.size $ states a ) xs
                             in ( Just $ S.size $ states d
                                , T.unwords $ map (T.pack . show) p
                                )
        (( mmin_in, t_min_in), (mmin_notin, t_min_notin)) = (best trs_in, best trs_notin)
        (b,tquot, min_in, min_notin) = 
            case ((( mmin_in, t_min_in), (mmin_notin, t_min_notin))) of
                 ((Just minin,_) , (Just minnotin,_)) ->
                    ( minin < minnotin,
                      T.pack $ show $ ( fromIntegral minin) / ( fromIntegral minnotin ), T.pack $ show  minin, T.pack $ show  minnotin )
                 ((Just minin,_) , (Nothing,_)) -> 
                    (True, "0" , T.pack $ show minin, "" )
                 ((Nothing,_) , (Just minnotin,_)) -> 
                    (False, "inf" , "" , T.pack $ show minnotin )
                 ((Nothing,_) , (Nothing,_)) -> 
                    (False, "-" , "", "" )
    return $ (b,[ tquot, toText f, min_in, t_min_in, min_notin, t_min_notin ])
