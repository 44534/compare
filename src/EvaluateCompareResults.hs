{-# Language LambdaCase #-}
{-|
Module      : EvaluateCompareResults
Description : evaluation of results 
Copyright   : Clara Waldmann, 2016

Evaluation of results.
-}

module EvaluateCompareResults where

import Text.CSV
import System.Environment ( getArgs )

import qualified Data.Text as T

import Data.CSV.Table hiding (toFile)
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo
-- import Graphics.Rendering.Chart.State 

-- import Graphics.EasyPlot
import Graphics.Gnuplot.Simple


import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Control.Monad (guard, forM_, forM)
import Debug.Trace (trace)

import qualified Language.LTL as LTL


type ToolChain = String

data RunRes = RunRes 
    { formula :: LTL.Formula
    , tool :: ToolChain
    , nrstates :: Int
    , nracc :: Int
    }
  deriving (Show)

data Results = Results
    { formulaMap :: M.Map LTL.Formula (M.Map ToolChain RunRes)
    , toolMap :: M.Map ToolChain (M.Map LTL.Formula RunRes)
    }
    deriving Show

st = True
on = if st then nrstates else nracc
    
main = do
    args <- getArgs
    case args of
         (("--plot":(fileName:(theOne: wos)))) -> do
             putStrLn $ show wos
             csv <- fromFile fileName
             -- putStrLn $ show csv
             let res = toResults csv
             putStrLn $ show $ tools  res
             histogramplot fileName 1.3 res theOne (tools res L.\\ (theOne:wos)) on
 --            putStrLn $ show b
         ("--scatter":t1:t2:files) -> do
             --csv <- fromFile fileName
             -- putStrLn $ show csv
             --let res = toResults csv
             scatterplot files t1 t2
--          [fileName] -> do
--              csv <- fromFile fileName
--              -- putStrLn $ show csv
--              -- putStrLn $ show $ getCols csv
--              let res = toResult csv
--                  maxs = map
--                     (\tc -> (tc, maximum $ M.keys (res M.! tc)) )
--                     (M.keys res)
--              -- putStrLn $ show res
--                  minall = minimum $ snd<$> maxs
--                  valsatmin = valsatn res minall 
--              --putStrLn $ show res
--             -- putStrLn $ show maxs
--              --putStrLn $ show minall
--              --putStrLn $ show valsatmin
--              putStrLn $ filter (\c -> not $ c == '"') $ show $ endtable res
--      

shortnames file =
    if L.isInfixOf "unif" file
       then "uniform"
       else  if L.isInfixOf "morefg" file 
                then "morefg"
                else if L.isInfixOf "fg" file
                        then "fg"
                        else file

plots t1 t2 (file, pt) = do 
    csv <- fromFile file
    let res = toResults csv
        fs = M.filter (\m -> (M.member t1 m) && (M.member t2 m)) $ formulaMap res
        vals = map (\(_,m) -> (on $ m M.! t1 , on $ m M.! t2)) $ M.toList fs
    return (PlotStyle Points $ CustomStyle [LineTitle $ shortnames file, PointType pt],
        vals)

scatterplot :: [String] -> ToolChain -> ToolChain -> IO ()
scatterplot fins t1 t2 = do
    plts <- mapM (plots t1 t2) $ zip fins ([2,6,1]++[0..])
    let  maxval = maximum $ map (\(_, vals) -> max (maximum $ fst <$> vals) (maximum $ snd <$> vals) ) plts
    plotListsStyle
        [ EPS "scatterplot.eps" 
        , Custom "logscale" []
        , XLabel t1
        , YLabel t2
        , Custom "key" ["outside", "bottom"]
        --, Custom "tics" ["font", "\", 30\""]
        --, Custom "key font" ["\", 30\""]
        ]
        ((PlotStyle Lines $ CustomStyle [LineTitle ""],
        zip [0..maxval] [0..maxval]) : plts )
        

row2RunRes :: Row -> RunRes
row2RunRes (R vs) = 
    let Right f = LTL.parse $ T.pack $ vs !! 0
    in RunRes f (filter (\c -> not $ c == '"')(vs !! 1)) (read (vs !! 2) :: Int) (read (vs !! 3) :: Int) 
             
toRunRes :: Table -> [RunRes]
toRunRes t = row2RunRes <$> body t

toResults :: Table -> Results
toResults t = 
    let rrs = toRunRes t
        fm = M.fromListWith M.union [ (formula rr, M.fromList [(tool rr, rr)]) | rr <- rrs ]
        tm = M.fromListWith M.union [ (tool rr, M.fromList [(formula rr, rr)]) | rr <- rrs ]
    in Results fm tm
    

tools :: Results -> [ToolChain]
tools r = M.keys $ toolMap r
     
quotients :: Results -> String -> ToolChain -> (RunRes -> Int) -> [Double]
quotients r einer derandere on = 
    let
        quots = map (\(f, tm) -> case (M.lookup einer tm, M.lookup derandere tm ) of
                            (Just einer, Just anderer) -> Just $ ((fromIntegral (on anderer) / fromIntegral (on einer)) :: Double)
                            _ -> Nothing
                    )
            $ M.toList $ formulaMap r
    in catMaybes quots
    
histogram :: Double -> Results -> String -> ToolChain -> (RunRes -> Int) -> [(Double, Int)]
histogram q r einer derandere on =
    let quots = quotients r einer derandere on 
        pointmap = M.fromListWith (+) $ map (\v -> ( q ^^ (round $ logBase q v),1)) quots
    in M.toList pointmap
    
histogramplot :: String -> Double -> Results -> ToolChain -> [ToolChain] -> (RunRes -> Int) -> IO ()
histogramplot fn q r einer andere on =
    let alle = tools r
        vals = map (\(i,derandere) -> (i, histogram q r einer derandere on)) $ zip [0..] andere 
        maxval = maximum $ (\(_,xs) -> maximum $ snd <$> xs ) <$> vals
        normvss = (\(i,xs) -> (i, (\(v,t) -> ( v , ((fromIntegral t :: Double) / (fromIntegral maxval)) + fromIntegral i))  <$>  xs)) <$> vals
        vss = map (\(i,m) -> (alle !! i , m))
                $ normvss
    in plotpdf 
        [ EPS "compplot.eps" 
        , Custom "logscale" ["x"]
        --, Custom "tics" ["font", "\", 30\""]
        --, Custom "key font" ["\", 30\""]
        ] vss
    
plotpdf opts values =
    plotListsStyle opts 
        (map (\(n,vs) -> (PlotStyle LinesPoints (CustomStyle [LineTitle n {-, LineWidth 5, PointSize 7-}] ), vs)) values)
