{-|
Module      : Options
Description : Arguments Parser 
Copyright   : Clara Waldmann, 2016

A parser for arguments of the executable ltlmaze.
-}

module Options where

import Options.Applicative
import Options.Applicative.Extra
import Transformer

data Modus = Pattern { pattern :: String -- ^ one of e, f, u, u2, c1, c2, q , r, s
                     }  
                -- ^ compare tranformers on formulae of a pattern
           | Enumerate { sizearg :: Maybe Int -- ^ Nothing: begin with size 1
                       , fclass :: String -- ^ one of fg, unif
                       , iaronly :: Bool} 
                -- ^ compare transformers on formulae of a given size
           | Random { formclass :: String -- ^ one of unif, fg, morefg
                    , nraps :: Int
                    , sizemin :: Int
                    , sizemax :: Int
                    , number :: Maybe Int -- ^ Nothing: generate formulae forever
                    }
                -- ^ compare transformers on randomly generated formulae
           | Form { formula :: String}
                -- ^ compare transformers on a given formula
    deriving Show
    

data ArgOpts = ArgOpts
    { modus :: Modus
    , cores :: Int
    , timeout :: Maybe Int
    , check :: Bool
    , only :: Maybe String -- ^ restrict paths: one of rabin, notrabin, rabinreduce, Nothing: all paths
    , steps :: Maybe Int    -- ^ maximal length of a tool chain (if there are cycles in the graph), Nothing: no bound
    }
   deriving Show
   
argOpts :: Parser ArgOpts
argOpts = ArgOpts 
    <$>   ((Pattern <$> 
                strOption
                (  long "pattern"
                <> metavar "(e | f | u | u2 | c1 | c2 | q | r | s)"
                <> help "Compare on formulae of the given pattern."
                )
            )
             <|> (Enumerate <$>
                    option (Just <$> auto)
                        ( long "enumerate"
                        <> metavar "Int"
                        <> help "enumerate formulae of the given size"
                        )
                <*> strOption 
                        ( long "class"
                        <> metavar "(unif | fg)"
                        <> help "the class of formulae to be generated"
                        )
                <*> switch (long "iar")
                    
                )
            <|> ( Enumerate <$>
                    flag' Nothing 
                        (  long "enumerate-all"
                        <> help "enumerate formulae beginning with size 1"
                        )
                <*> strOption
                        ( long "class"
                        <> metavar "(unif | fg)"
                        <> help "the class of formulae to be generated"
                        )
                <*> switch (long "iar")
                )
            <|> (Random <$>
                    strOption
                    (  long "random"
                    <> metavar "(unif | fg | morefg)"
                    <> help "the class of formulae to be generated"
                    )
                    
                <*> option auto
                    (  long "nraps"
                    <> value 4
                    <> metavar "Int"
                    <> showDefault
                    <> help "maximal number of atomic propositions in the formulae"
                    )
                <*> option auto
                    ( long "smin"
                    <> value 10
                    <> metavar "Int"
                    <> showDefault
                    <> help "minimal size of the formulae"
                    )
                <*> option auto
                    ( long "smax"
                    <> value 20
                    <> metavar "Int"
                    <> showDefault
                    <> help "maximal size of the formulae"
                    )
               <*> optional 
                        (option auto
                            ( long "nr"
                            <> metavar "Int"
                            <> help "number of formulae to be compared on"
                            )
                        )
                )
            <|> ( Form <$>
                    strOption
                    (  long "formula"
                    <> metavar "'spot's format'"
                    <> help "compare on a given formula (quoted in spot's format)"
                    )
                )
           )
        <*>  option auto
              ( long "cores"
              <> value 1
              <> metavar "Int"
              <> help "give number of processor cores used to compute in parallel"
              )
        <*> optional (option  auto 
              (long "timeout"
              <> metavar "sec"
              <> help "timeout for each transformer in seconds"
              ))
        <*> switch 
              (  long "check"
              <> help "turn on equivalence check using autfilt"
              )
        <*> optional ( strOption 
              (  long "only"
              <> metavar "(rabin | notrabin | rabinreduce)"
              <> help "restrict transformers used to paths using DRAs as intermediate automata (rabin), or to paths not using DRAs (notrabin) or add reductions of a Rabin acceptance condition (rabinreduce) (implies --only rabin)"
              ))
        <*> option (Just <$> auto)
              ( long "steps"
              <> value Nothing
              <> metavar "Int"
              <> showDefault
              <> help "give maximal length of tool chains (if there are cycles in the graph)"
              )
