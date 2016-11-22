{-# Language OverloadedStrings #-}
{-|
Module      : Pathtest
Description : Compare random  and enumerated formulae 
Copyright   : Clara Waldmann, 2016

Compares transformers on a given formula or randomly generated formualae.
-}
module Pathtest where

import Transformer
import Compare
import ComCon
import Options

import Language.LTL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (forM)
import System.IO (hFlush, stderr, stdout)

-- | compare transformers on randomly generated formulae
-- according to the options
mainwürfel :: ArgOpts -> [Transformer] -> IO ()
mainwürfel args trs = do
    let values = case only args of
                    Just "rabinreduce" -> rabinreducevalues
                    _ -> allpathsvalues
        compute act = case number $ modus args of
                    Nothing -> pforever' (cores args) act
                    Just n -> pfor (cores args) $ replicate n act
                        
        forms = case formclass $ modus args of
                    "unif" -> forms_uniform_nf
                    "fg" -> forms_fg_nf
                    "morefg" -> forms_morefg_nf
    compute $ do
            s <- select [(sizemin $ modus args)..(sizemax $ modus args)]
            f <- forms (nraps $ modus args) s
            T.hPutStrLn stderr $ toText f
            vals <- values (check args) trs (timeout args) (steps args) f
            forM vals (T.hPutStrLn stdout . T.intercalate ",")
            hFlush stdout
                
-- | compare transformers on a given formula
mainformula args trs = do
    let values = case only args of
                    Just "rabinreduce" -> rabinreducevalues
                    _ -> allpathsvalues
        Right f = parse $ T.pack $ formula $ modus args
    T.hPutStrLn stderr $ toText f
    vals <- values (check args) trs (timeout args) (steps args) f
    forM vals (T.hPutStrLn stdout . T.intercalate ",")
    hFlush stdout
