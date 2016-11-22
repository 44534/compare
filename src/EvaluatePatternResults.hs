{-# Language LambdaCase, OverloadedStrings #-}

module EvaluatePatternResults where

import Text.CSV
import System.Environment ( getArgs )

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.CSV.Table hiding (toFile)
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo
-- import Graphics.Rendering.Chart.State 

-- import Graphics.EasyPlot
import Graphics.Gnuplot.Simple


import qualified Data.Map as M
import Data.Maybe
import qualified Data.List as L
import Control.Monad (guard, forM_, forM)
--import Debug.Trace (trace)

import Text.LaTeX
import Text.LaTeX.Base.Class

type ToolChain = String
type Result = M.Map ToolChain (M.Map Int (Int,Int))


main = do
    args <- getArgs
    case args of
         (("--plot":(fileName:wos))) -> do
             putStrLn $ show wos
             csv <- fromFile fileName
             -- putStrLn $ show csv
             let res = toResult csv
             -- putStrLn $ show res
             --b <- scatterplot res theOne wos
             --putStrLn $ show b
             plot res wos
         (("--latex":(fileName:wos))) -> do 
             csv <- fromFile fileName
             T.putStrLn $ latextable ( toResult csv) wos
         fileName:wos -> do
             csv <- fromFile fileName
             -- putStrLn $ show csv
             -- putStrLn $ show $ getCols csv
             let res = toResult csv
                 reswowos = M.filterWithKey (\tc _ -> not( tc `elem` wos)) res 
                 maxs = map
                    (\tc -> (tc, maximum $ M.keys (res M.! tc)) )
                    (M.keys reswowos)
             -- putStrLn $ show res
                 minall = minimum $ snd<$> maxs
                 valsatmin = valsatn reswowos minall 
             --putStrLn $ show res
            -- putStrLn $ show maxs
             --putStrLn $ show minall
             --putStrLn $ show valsatmin
             putStrLn $ filter (\c -> not $ c == '"') $ show $ endtable reswowos
 
 
listsTabular ::
               [LaTeX] -- ^ (Non-empty) List of column titles
              -> [[LaTeX]] -- ^ Matrix of data
              -> LaTeX -- ^ Data organized in a tabular environment
listsTabular ts m =
  let spec = (replicate (length $ head m) RightColumn)
  in  tabular Nothing spec $ mconcat
        [ comm0 "toprule"
        , foldl1 (&) ts
        , lnbk
        , comm0 "midrule"
        , mconcat $ fmap (
            \vs -> mconcat [ foldl1 (&) vs
                          , lnbk
                            ] ) m
        , comm0 "bottomrule"
          ]

             
latextable :: Result -> [ToolChain] -> T.Text
latextable res wos =
    let res' = M.filterWithKey (\tc _ -> not $ tc `elem` wos) res
        maxn = maximum ( (fst . M.findMax) <$> M.elems res')
        mins = M.fromList ( (\n -> (n, minimum $ catMaybes (M.lookup n <$> M.elems res'))) <$> [1..maxn] )
    in render $ 
        listsTabular
            ((raw "toolchain" :: LaTeX) : ( (raw . T.pack . show) <$> [1..maxn]) )
            (( 
                (\(tc,m) -> --small <$> 
                    ( raw $ T.pack $ rm_tospinsyntax tc) : 
                        ( (\n -> maybe (raw "")  
                            (\v -> (if v == mins M.! n then textbf . raw else raw) $ T.pack $ init $ tail $ show v) $ M.lookup n m) 
                        <$> [1..maxn] 
                        )
                ) 
                <$> M.toList res' ))
             
parserow :: Row -> (String, (Int, (Int, Int)))
parserow (R vs) = (init $ tail $ filter (\c -> not $ c == '"') (vs !! 1)
                  , (read (vs !! 0) :: Int
                    , ( read (vs !! 2) :: Int 
                      , read (vs !! 3) :: Int 
                      )
                    )
                  )
                   
toResult :: Table -> Result
toResult t@(T _ _ body) = M.fromListWith M.union
    $ do
        (tc,v) <- parserow <$> body
        return (tc, M.fromList [v])
        
valsatn :: Result -> Int -> M.Map ToolChain (Int,Int)
valsatn res n = M.fromList $
    do
        (tc, m) <- M.toList res
        guard $ n <= (maximum $ M.keys m)
        return $ (tc, m M.! n)
        
endtable :: Result -> Table
endtable res = 
    let maxs = map
                    (\tc -> (tc, maximum $ M.keys (res M.! tc)) )
                    (M.keys res)
        minall = minimum $ snd<$> maxs
        valsatmin = (\(tc,(nrs,nra)) -> [tc, show nrs, show nra]) <$> ( M.toList $ valsatn res minall )
        rows = do
            vals@(tc:vs) <- valsatmin
            (tc', maxn) <- maxs
            guard $ tc == tc'
            return $ vals ++ [show maxn]
    in T 0 [C "tool", C ("nr states at n=" ++ show minall), C ("nr acc at n=" ++ show minall), C "max n"] (R <$> rows)

toVals :: Result -> ToolChain -> [Int]
toVals res tc = fst <$> M.elems (res M.! tc)
    
-- toLogVals :: Result -> ToolChain -> [LogValue]
-- toLogVals res tc = LogValue . fromIntegral <$> toVals res tc

rm_tospinsyntax :: String -> String
rm_tospinsyntax tc = 
    maybe tc id (L.stripPrefix "to spin syntax-" tc)
        
plot :: Result -> [ToolChain] -> IO ()
plot res wos =
    let vals = map (\(tc,m) -> (rm_tospinsyntax tc, M.toList m)) $  M.toList $ M.filterWithKey (\tc _ -> not $ tc `elem` wos) res
    in plotpdf 
        [ EPS "plot.eps"
        , Custom "logscale" ["y"]
        , Custom "key" ["outside", "bottom"]
        ]
        vals
    
scatterplot :: Result -> ToolChain -> [ToolChain] -> IO ()
scatterplot res theOne wos = 
    let tchs = M.keys res
        theOneVals = toVals res theOne
        allVals = do
            tc <- tchs
            guard $ (tc /= theOne) && (not $ tc `elem` wos)
            let tcVals = toVals res tc
            return $ (tc, zip theOneVals tcVals)
    in do
        putStrLn $ show allVals
        plotpdf
            [ EPS "plot.eps"
            , Custom "logscale" []
            ]   
            allVals
 
plotpdf opts values =
    plotListsStyle opts
        (map (\((n,vs), pt)-> (PlotStyle LinesPoints (CustomStyle [LineTitle n, PointType pt, LineType 0 {-, LineWidth 5, PointSize 7-}] ), vs)) $ zip values ([14,2,34,10,8,1,50,56,20,40,17] ++ [15..]))
 
{-plotpdf fileName title values = plot' [Debug] (PDF fileName) $
    map (\(n,vs) -> 
            Data2D [Title n, Style Linespoints, Color Black] [] vs
            --, Data2D [Title n, Style Lines] [] vs
             ) values
   -} 
-- plotpdf :: FilePath -> String -> [(ToolChain,[(LogValue,LogValue)])] -> IO ()
-- plotpdf fileName title vss = toFile (def & fo_format .~ PDF ) fileName $ 
--     do
--              layout_title .= title
-- --              layout_plots .= ( map
-- --                 ( \(n,vs) ->
-- --                    do
-- --                       s <- takeShape
-- --                       return $ toPlot $
-- --                         plot_points_values .~ vs
-- --                             $ plot_points_title .~ n
-- --                             $ plot_points_style . point_shape .~ s
-- --                             $ (def :: PlotPoints LogValue LogValue)
-- --                     
-- --                      )
-- --                         
-- --                             vss ) ++
-- --                             (map
-- --                     (\(n, vs) ->
-- --                        toPlot $
-- --                         plot_lines_values .~ [vs]
-- --                             $ plot_lines_title .~ n
-- --                             $ plot_lines_style . line_color .~ opaque black
-- --                             $ (def :: PlotLines LogValue LogValue) )
-- --                             vss )
--              forM_ vss $ \(n,vs) -> do
--                    plot (points n vs)
--                    plot (line n [vs] )
--                    
