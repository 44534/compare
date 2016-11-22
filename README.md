compare
=======
[![Build Status](https://travis-ci.org/44534/compare.svg?branch=master)](https://travis-ci.org/44534/compare)

A library to compare LTL to omega-automata translators.
Currently evaluation is restricted to LTL to parity automata translations.

Features
--------
* Find all paths from LTL to parity automata from the given transformers between different automata types
* Output the results directly after processing one formula completely
* Process several formulae in parallel
* Give a timeout and an equivalence check
* Restrict the paths to paths using deterministic Rabin automata as intermediate results
* Compare tool chains on
    * patterns
    * enumerated formulae of a given size
    * randomly generated formulae of a given size
    * a given formula

Programm
--------
The package contains the executable `ltlmaze` to compare different LTL to parity automata translations.
The following transformers have to be installed separately and available in `$PATH` with the given names:

* [spin](http://spinroot.com/spin/whatispin.html) - spin
* [ltl2ba](http://www.lsv.ens-cachan.fr/~gastin/ltl2ba/) - ltl2ba
* [ltl3ba](http://ltl3ba.sourceforge.net/) - ltl3ba
* [GOAL](http://goal.im.ntu.edu.tw/wiki/doku.php#download)(`gc`) - goal
* [spot](https://spot.lrde.epita.fr/)
    * autfilt
    * ltl2tgba
* [ltl2dstar](http://www.ltl2dstar.de/) - ltl2dstar
* [Rabinizer 3](https://www7.in.tum.de/~kretinsk/rabinizer3.html) - rabinizer
* [IAR](https://github.com/44534/IAR) - IAR-exe
* [reducerabinpairs](https://github.com/44534/reducerabinpairs) - reducerabinpairs

Additionally for some tools there are scripts in the `scripts` folder which have to be available with the following names:

* `callspin.sh` - callspin
* `callltl2ba.sh` - callltl2ba
* `goal_ltl2nba.sh` - goal_ltl2nba
* `goal_nbadpa.sh` - goal_nba2dpa
* `epsilon.sh` - epsilon
* `rabinizer.sh` - rabinizer (change `$RAB` :path to rabinizer jar file)

To run several instances of `GOAL` concurrently the following changes have to be made to the sources:

* in `gc` change
    * line 8: `` DIR=`dirname $0` `` to `DIR=absolute path to the GOAL directory`
    * line 14: add `exec` before the call to `java`
    * lines 16 - 22: add line `-Djava.io.tmpdir=${TMPDIR:-/tmp} \`
* in `boot_cmd.properties` change line 7
`org.java.plugin.standard.ShadingPathResolver.shadowFolder = ${applicationRoot}/.jpf-shadow` to ```org.java.plugin.standard.ShadingPathResolver.shadowFolder = ${java.io.tmpdir}/.jpf-shadow```

The supported options are:
```
> ltlmaze -h
Compare LTL to omega-automata translations

Usage: ltlmaze (--pattern (e | f | u | u2 | c1 | c2 | q | r | s) |
               --enumerate Int --class (unif | fg) [--iar] | --enumerate-all
               --class (unif | fg) [--iar] | --random (unif | fg | morefg)
               [--nraps Int] [--smin Int] [--smax Int] [--nr Int] |
               --formula 'spot's format') [--cores Int] [--timeout sec]
               [--check] [--only (rabin | notrabin | rabinreduce)] [--steps Int]
  ltlmaze

Available options:
  -h,--help                Show this help text
  --pattern (e | f | u | u2 | c1 | c2 | q | r | s)
                           Compare on formulae of the given pattern.
  --enumerate Int          enumerate formulae of the given size
  --class (unif | fg)      the class of formulae to be generated
  --enumerate-all          enumerate formulae beginning with size 1
  --class (unif | fg)      the class of formulae to be generated
  --random (unif | fg | morefg)
                           the class of formulae to be generated
  --nraps Int              maximal number of atomic propositions in the
                           formulae (default: 4)
  --smin Int               minimal size of the formulae (default: 10)
  --smax Int               maximal size of the formulae (default: 20)
  --nr Int                 number of formulae to be compared on
  --formula 'spot's format'
                           compare on a given formula (quoted in spot's format)
  --cores Int              give number of processor cores used to compute in
                           parallel
  --timeout sec            timeout for each transformer in seconds
  --check                  turn on equivalence check using autfilt
  --only (rabin | notrabin | rabinreduce)
                           restrict transformers used to paths using DRAs as
                           intermediate automata (rabin), or to paths not using
                           DRAs (notrabin) or add reductions of a Rabin
                           acceptance condition (rabinreduce) (implies --only
                           rabin)
  --steps Int              give maximal length of tool chains (if there are
                           cycles in the graph) (default: Nothing)

```

Example call:
```
> ltlmaze --formula 'F(G a)' --timeout 10

(F (G a))
[to spin syntax]
[ltl2tgba NBA]
...
[ltl2tgba NBA,ltl2dstar,IAR*]
**************************************************
(F (G a)),[ltl2tgba DPA],3,3

...
(F (G a)),[to spin syntax-GOAL ltl2nba-ltl2dstar-SIAR],4,3
(F (G a)),[Rabinizer-SIAR],5,5

```

Install
-------
Dependencies:

* omega-automata
* ltl-syntax


```
stack install
```
