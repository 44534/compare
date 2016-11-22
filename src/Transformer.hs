{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances, LambdaCase, OverloadedStrings #-}
{-|
Module      : Transformer
Description : Transformers
Copyright   : Clara Waldmann, 2016

Call of a transformer and translations bewteen different automata types from the literature as transformers.
-}

module Transformer (
    -- * Constructors
    V(..), D(..), Failure(..), 
    -- * Transformer
    Transformer(..), apply,
    -- * Functions
    paths, pathsbetween,
    -- * Tools From the Literature
    -- ** LTL -> NBA
    spin, ltl2ba, ltl3ba, goal_ltl2nba, ltl2tgba_nba,
    ltl2nbas,
    -- ** Others
    ltl2tgba_dpa, piterman, schewe, ltl2dstar, rabinizer,
    -- ** IAR
    iar, siar, iaropt, siaropt,
    transformers,
    -- ** Reductions
    setsplit, setred, setcombine, setsat, setirred, 
    topsplit, topred, topcombine, topsat, topirred,
    reduceall, rabinreducers,
    -- ** Special
    eps, ltltospin
    ) where

import OmegaAutomata.Automata
import qualified OmegaAutomata.Hoa as H
import Language.LTL as LTL

import Data.Attoparsec.Text
import qualified Data.Text as T

import System.Process.Text
import System.Process (proc,CreateProcess(..))
import System.Environment
import System.Timeout
import System.Exit
import System.IO.Temp
import Control.Monad (guard)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))


class ToHOA a where
    toHOA   :: a -> H.HOA 

instance ToHOA a => ToText a where
    toText = T.pack . H.toHoa . toHOA

class FromText a where
    fromText :: T.Text -> a
    
class FromHOA a where
    fromHOA :: H.HOA -> a

instance FromHOA a => FromText a where
    fromText a = case parseOnly H.parseHoa a of
                      Right a -> fromHOA a
                      Left err -> error $ "automaton parser fail" ++ show err
    
instance (Show q, Show l, Ord q) => ToHOA (DPA q (Maybe H.LabelExpr) l) where
    toHOA = H.dpaToHoa

instance (Show q, Show l, Ord q) => ToHOA (DRA q (Maybe H.LabelExpr) l) where
    toHOA = H.draToHoa

    
instance FromHOA (DRA State (Maybe H.LabelExpr) (Maybe H.LabelExpr)) where
    fromHOA = H.hoaToDRA
 
instance FromHOA (NBA State (Maybe H.LabelExpr) (Maybe H.LabelExpr)) where
    fromHOA = H.hoaToNBA
 
    
instance (Show q, Show l, Ord q)
   => ToHOA (NBA q (Maybe H.LabelExpr) l) where
       toHOA = H.nbaToHoa
    
instance FromHOA (Either [H.AccName] (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))) where
    fromHOA = H.hoaToDPA
    
instance FromText (Formula) where
    fromText f = case LTL.parse f of
                     Right f -> f
                     _ -> error "formula parse fail"
instance FromText (SpinFormula) where
    fromText f = case LTL.parse f of
                     Right f -> SpinFormula f
                     _ -> error "formula parse fail"
    
data Transformer =
    Transformer {
          source :: V
        , target :: V
        , name :: String
        , exe :: FilePath
        , args :: [String]
        }

instance Show Transformer where
    show = name
instance Eq Transformer where
    t1 == t2 = name t1 == name t2
    

data Failure = TO           -- ^ timeout 
             | PE T.Text    -- ^ parse error
             | Error T.Text -- ^ other error
instance Show Failure where
    show f = case f of
                  TO -> "timeout"
                  PE t -> "parse error" ++ show t
                  Error t -> show t

-- | apply a transformer to an object (formula or automaton) optionally with a timeout
--
-- if a timeout is given and the transformer takes too long, exits not successfully or the output could not be parsed this function will fail.
--
-- Important: The transformer has to compute an automaton of its target type or else the parser will fail.
apply :: Transformer 
    -> D 
    -> Maybe Int    -- ^ timeout 
    -> ExceptT Failure IO D
-- special case: transformer from spot's to spins' format
apply  (t@Transformer{source = VForm, target = VSpinForm}) (DForm f) mto = do
    return $ DSpinForm $ SpinFormula f
-- general case
apply t inp mto = do
    let tinp = case (source t, inp) of
            (VForm, DForm f) -> toText f
            (VSpinForm, DSpinForm f) -> toText f
                               
            (VNBA, DNBA a) ->  toText a
            (VDPA, DDPA a) -> toText a
            (VDRA, DDRA a) -> toText a 
            (VDRAcompact, DDRA a) -> toText a
            (VDRAscc, DDRA a) -> toText a  
            (VDRAsingleton, DDRA a) -> toText a 
            foo -> error $ show foo
    mout <- case mto of
                Nothing -> do
                    (liftIO $ do
                         env <- getEnvironment
                         withSystemTempDirectory "foo" $
                            \tmpdir ->
                            readCreateProcessWithExitCode
                            (proc (exe t) (args t)){env= Just $ env ++ [("TMPDIR",tmpdir)]}
                            ( tinp))
                    >>= \ case
                            (ExitSuccess ,out, _) -> return out
                            (ec, out, err) -> throwE $ Error $
                                (T.pack $ show ec) <> err
                Just to -> do
                    (liftIO $ do
                         env <- getEnvironment
                         withSystemTempDirectory "foo" $
                           \tmpdir -> do
                            timeout (to*10^6) $ 
                                readCreateProcessWithExitCode
                                (proc (exe t) (args t)){env= Just $ env ++ [("TMPDIR",tmpdir)]}
                                  ( tinp)
                     )
                    >>= \ case
                            Just (ExitSuccess ,out, _) -> return out
                            Just (ec, out, err) -> throwE $ Error $
                                (T.pack $ show ec) <> err
                            Nothing -> throwE TO
    case parseOnly H.parseHoa mout of
                      Right a -> case target t of
                        VNBA -> return $ DNBA $! fromHOA a
                        VDPA -> case (fromHOA a) :: (Either [H.AccName] (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))) of
                                        Right ha -> return $ DDPA ha
                                        Left accn -> throwE $ PE $ T.pack $ show accn
                        VDRA -> return $ DDRA $! fromHOA a
                        VDRAcompact -> return $ DDRA $! fromHOA a
                        VDRAscc -> return $ DDRA $! fromHOA a
                        VDRAsingleton -> return $ DDRA $! fromHOA a
                      Left err -> throwE $ PE $ T.pack err
    
-- | transforms a formula in spot's syntax into spin's syntax
ltltospin :: Transformer
ltltospin = Transformer {
      source = VForm
    , target = VSpinForm
    , name = "to spin syntax"
    , exe = undefined
    , args = []
    }
                                    
-- LTL -> NBA

spin :: Transformer -- SpinFormula (NBA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
spin = Transformer {
      source = VSpinForm
    , target = VNBA
    , name = "spin"
    , exe = "callspin"
    , args = []
    }
    
ltl2ba :: Transformer -- SpinFormula (NBA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
ltl2ba = Transformer {
      source = VSpinForm
    , target = VNBA
    , name = "ltl2ba"
    , exe = "callltl2ba"
    , args = []
    }

ltl3ba :: Transformer -- SpinFormula (NBA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
ltl3ba = Transformer {
      source = VSpinForm
    , target = VNBA
    , name = "ltl3ba"
    , exe = "ltl3ba"
    , args = ["-F", "-", "-H"]
    }

goal_ltl2nba :: Transformer -- Formula (NBA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
goal_ltl2nba = Transformer {
      source = VSpinForm
    , target = VNBA
    , name = "GOAL ltl2nba"
    , exe = "goal_ltl2nba"
    , args = []
    }
    

ltl2tgba_nba :: Transformer -- Formula (NBA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
ltl2tgba_nba = Transformer {
      source = VForm
    , target = VNBA
    , name = "ltl2tgba NBA"
    , exe = "ltl2tgba"
    , args = ["-B", "-F", "/dev/stdin"]
    }
 
ltl2nbas = [spin, ltl2ba, ltl3ba, goal_ltl2nba, ltl2tgba_nba]
-- LTL -> DPA
 
ltl2tgba_dpa :: Transformer -- Formula (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
ltl2tgba_dpa = Transformer {
      source = VForm
    , target = VDPA
    , name = "ltl2tgba DPA"
    , exe = "ltl2tgba"
    , args = ["--generic", "--deterministic", "-S" , "-F", "/dev/stdin"]
    }
 
-- NBA -> DPA
 
piterman :: Transformer -- (NBA q (Maybe H.LabelExpr) l ) (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
piterman = Transformer {
      source = VNBA
    , target = VDPA
    , name = "GOAL piterman"
    , exe = "goal_nba2dpa"
    , args = []
    }
    
schewe :: Transformer -- (NBA q (Maybe H.LabelExpr) l ) (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
schewe = Transformer {
      source = VNBA
    , target = VDPA
    , name = "GOAL schewe"
    , exe = "goal_nba2dpa"
    , args = ["-ht"]
    }
    
-- NBA -> DRA
    
ltl2dstar :: Transformer --(NBA q (Maybe H.LabelExpr) l ) (DRA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
ltl2dstar = Transformer {
      source = VNBA
    , target = VDRA
    , name = "ltl2dstar"
    , exe = "ltl2dstar"
    , args = ["--input=nba", "--output-format=hoa", "-", "-"]
    }
    
-- LTL -> DRA

rabinizer :: Transformer -- Formula (DRA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
rabinizer = Transformer {
      source = VForm
    , target = VDRA
    , name = "Rabinizer"
    , exe = "rabinizer"
    , args = ["-silent", "-out=std", "-format=hoa", "-auto=sr", "-in=file", "/dev/stdin"]
    }

-- DRA -> DPA

iaropt :: Transformer -- (DRA q (Maybe H.LabelExpr) l ) (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
iaropt = Transformer {
      source = VDRA
    , target = VDPA
    , name = "IAR*"
    , exe = "IAR-exe"
    , args = ["--opt"]
    }
    
    
iar :: Transformer -- (DRA q (Maybe H.LabelExpr) l ) (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
iar = Transformer {
      source = VDRA
    , target = VDPA
    , name = "IAR"
    , exe = "IAR-exe"
    , args = []
    }
    
    
siaropt :: Transformer -- (DRA q (Maybe H.LabelExpr) l ) (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
siaropt = Transformer {
      source = VDRA
    , target = VDPA
    , name = "SIAR*"
    , exe = "IAR-exe"
    , args = ["--opt", "--streett"]
    }
    
    
siar :: Transformer -- (DRA q (Maybe H.LabelExpr) l ) (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr))
siar = Transformer {
      source = VDRA
    , target = VDPA
    , name = "SIAR"
    , exe = "IAR-exe"
    , args = ["--streett"]
    }
-- reduce Rabinpairs

-- rabinreduce = Transformer{
--       source = VDRA
--     , target = VRedDRA
--     , name = "reducerabinpairs"
--     , exe = "reducerabinpairs"
--     , args = ["--hoa"]
--     }
    
-- iar_red = iaropt {source = VRedDRA, name = "IAR red"}
    
setred = Transformer {
      source = VDRA
    , target = VDRA
    , name = "set_reduce"
    , exe = "reducerabinpairs"
    , args = ["--do", "reduce"]
    }
    
    
setsplit = Transformer {
      source = VDRA
    , target = VDRAsingleton
    , name = "set_split"
    , exe = "reducerabinpairs"
    , args = ["--do", "split"]
    }
    

setcombine = Transformer {
      source = VDRA
    , target = VDRA
    , name = "set_combine"
    , exe = "reducerabinpairs"
    , args = ["--do", "combine"]
    }
    

setsat = Transformer {
      source = VDRA
    , target = VDRA
    , name = "set_sat"
    , exe = "reducerabinpairs"
    , args = ["--do", "sat"]
    }
    

setirred = Transformer {
      source = VDRA
    , target = VDRA
    , name = "set_irred"
    , exe = "reducerabinpairs"
    , args = ["--do", "irred"]
    }
    
setreds = [setred, setsplit, setcombine, setsat, setirred]

topred = Transformer {
      source = VDRAcompact
    , target = VDRAscc
    , name = "top_red"
    , exe = "reducerabinpairs"
    , args = ["--do", "reduce", "--top"]
    }
    
topsat = Transformer {
      source = VDRAcompact
    , target = VDRAcompact
    , name = "top_sat"
    , exe = "reducerabinpairs"
    , args = ["--do", "sat", "--top"]
    }

topirred = Transformer {
      source = VDRAsingleton
    , target = VDRAsingleton
    , name = "top_irred"
    , exe = "reducerabinpairs"
    , args = ["--do", "irred", "--top"]
    }

topcombine = Transformer {
      source = VDRAscc
    , target = VDRA
    , name = "top_combine"
    , exe = "reducerabinpairs"
    , args = ["--do", "combine", "--top"]
    }

topsplit = Transformer {
      source = VDRA
    , target = VDRAcompact
    , name = "top_split"
    , exe = "reducerabinpairs"
    , args = ["--do", "split", "--top"]
    }
    
topreds = [topred, topsat, topirred, topcombine, topsplit]

reduceall = Transformer {
      source = VDRA
    , target = VDRA
    , name = "reduce_all"
    , exe = "reducerabinpairs"
    , args = ["--do", "all"]
    }

-- | either only 'reduceall' or all reductions with the corresponding epsilon transformers

-- rabinreducers = setreds ++ topreds ++ [reduceall] ++
--     [ eps VDRAcompact VDRA
--     , eps VDRAscc VDRA
--     , eps VDRAsingleton VDRA
--     , eps VDRAscc VDRAcompact
--     , eps VDRAsingleton VDRAcompact
--     ]

rabinreducers = [reduceall]
    
-- | \epsilon transformer bewteen the given nodes
eps :: V -> V -> Transformer
eps s t = Transformer {
      source = s
    , target = t
    , name = "epsilon"
    , exe = "epsilon"
    , args = []
    }
    
-- | all transformers (except 'rabinreducers')
transformers :: [Transformer]
transformers = 
    [ ltltospin
    , spin
    , ltl2ba
    , ltl3ba
    , goal_ltl2nba
    , ltl2tgba_nba
    , ltl2tgba_dpa
    , piterman
    , schewe
    , ltl2dstar
    , rabinizer
    , iaropt, iar
    , siaropt, siar
    ]
    
-- | compute all tool chains using the given transformers starting at the given node
paths :: [Transformer] -> V -> [[Transformer]]
paths trs start = [] : do
    tr <- trs
    guard $ start == source tr
    es <- paths trs $ target tr
    return $ tr : es
    
-- | compute all tool chains using the given transformers between the given nodes
pathsbetween :: [Transformer] -> V -> V -> [[Transformer]]
pathsbetween trs start end = filter
    (\p -> end == (target $ head $ reverse p)) 
    $ tail $ paths trs start
    
-- | Node types
data V = VForm -- ^ LTL formula (spot's format)
       | VSpinForm -- ^ LTL formula (spin's format)
       | VNBA | VDPA | VDRA
       | VDRAcompact -- ^ compact Rabin automaton 
       | VDRAscc    -- ^ strongly compact Rabin automaton 
       | VDRAsingleton -- ^ Rabin automaton where each final set is singleton
    deriving (Eq, Ord, Show)
    
-- | data types stored in nodes
data D = DForm Formula | DSpinForm SpinFormula
       | DNBA (NBA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
       | DDPA (DPA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
       | DDRA (DRA State (Maybe H.LabelExpr) (Maybe H.LabelExpr) )
    deriving (Eq, Show)

