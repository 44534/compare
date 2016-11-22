{-# Language OverloadedStrings #-}
{-|
Module      : Patterns
Description : Compares transformers on patterns 
Copyright   : Clara Waldmann, 2016

Compares transformers on a pattern.
-}
module Patterns where

import Transformer
import Compare
import Language.LTL
import ComCon
import Options

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (forM, when)
import System.IO ( hFlush, stderr, stdout, hPutStrLn)
import Control.Exception

instance Exception Int
   
-- |  Compare transformers on a pattern according to the options.
-- for each path translate the formulae up to the smallest parameter which could not be translated
mainpattern args trs = do
    let pat = case pattern $ modus args of
                    "e" -> e
                    "u" -> u
                    "u2" -> u2
                    "r" -> r
                    "c1" -> c1
                    "c2" -> c2
                    "q" -> q
                    "s" -> s
                    "f" -> f
    pforM_ (cores args) (pathsbetween trs VForm VDPA) 
            $ \ path -> do
                Left n <- (try $ forM [1..] $ \n -> do
                        hPutStrLn stderr $ replicate 10 '*' ++ show n
                        let f = pat n
                        (b,vals) <- allpathssuccess (check args) path (timeout args) (steps args) f
                        forM vals (\ (f:vls) -> T.hPutStrLn stdout $ T.intercalate "," (T.pack (show n):vls))
                        hFlush stdout
                        when (not b) $ throw n) :: IO (Either Int [()])
                hPutStrLn stderr $ displayException n
                return n

