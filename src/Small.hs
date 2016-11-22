{-# Language OverloadedStrings #-}
{-|
Module      : Small
Description : Compare transformers on enumerated formulae
Copyright   : Clara Waldmann, 2016

Compare transformers on enumerated formulae.
-}
module Small where 
 
import Control.Monad (when, forM)
import Language.LTL
import Data.Text.IO as T
import Data.Text as T
import System.IO (stderr, stdout, hFlush)

import Compare
import Transformer
import ComCon
import Options

-- | compare transformers on enumerated formulae.
-- enumerate formulae according to the options until all tool chains computed a parity automaton with at least 10 states.
mainaufz args trs = do
    let fgen = case fclass $ modus args of
                "fg" -> forms_fg_nf
                "unif" -> forms_uniform_nf
        fs = case sizearg $ modus args of
         Nothing -> [1..] >>= \s -> fgen s s
         Just s -> fgen s s
        act = if iaronly $ modus args 
                 then \f -> do
                    T.hPutStrLn stderr $ toText f
                    (b,line) <- allpathseval (check args) trs (timeout args) (steps args) (\t -> name t `elem` ["IAR*", "SIAR*", "IAR", "SIAR"]) f
            --         T.putStrLn $ T.intercalate "," line
            --         hFlush stdout
                    when b $ do 
                        T.putStrLn $ T.intercalate "," line
                        hFlush stdout 
            
                 else \f -> do
                    T.hPutStrLn stderr $ toText f
                    --vals <- allpathsvalues (check args) trs (timeout args) (steps args) f
                    (b,vals) <- allpathsvalues_upto 10 (check args) trs (timeout args) (steps args) f
                    forM vals (T.hPutStrLn stdout . T.intercalate ",")
                    hFlush stdout
                    when b $ error "end"
    pforM_ (cores args) fs act
                    
