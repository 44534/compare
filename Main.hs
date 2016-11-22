import Pathtest (mainwürfel, mainformula)
import Patterns (mainpattern)
import Small (mainaufz)
import Options
import Transformer

import System.IO ( stderr, stdout, hPutStrLn, hSetBuffering, BufferMode(..))

import Options.Applicative

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    args <- execParser $
        info (helper <*> argOpts) 
            (fullDesc <> progDesc "ltlmaze" <> header "Compare LTL to omega-automata translations")
    -- hPutStrLn stderr $ show args
    let trs = case only args of 
                Just "rabin" -> (ltltospin : ltl2nbas) ++
                        [ ltl2dstar, rabinizer
                        , iar, iaropt
                        , siar, siaropt
                        , ltl2tgba_dpa
                        ]
                Just "rabinreduce" -> (ltltospin : ltl2nbas) ++
                        [  ltl2dstar, rabinizer
                        , iar, iaropt
                        -- , siar, siaropt
                        , ltl2tgba_dpa
                        ] ++ rabinreducers
                Just "notrabin" -> (ltltospin : ltl2nbas) ++ [piterman, schewe, ltl2tgba_dpa]
                Nothing -> transformers
    case modus args of
         Pattern {} -> mainpattern args trs
         Enumerate {} -> mainaufz args trs
         Random {} -> mainwürfel args trs
         Form {} -> mainformula args trs
    return ()
