{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

{-# LANGUAGE OverloadedStrings #-}

module SequentCalc.Latex where

import SequentCalc
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath

newtype SequenceRtl = SequenceRtl [Formula]
newtype SequenceLtr = SequenceLtr [Formula]

instance Texy Formula where
  texy (Var x) = fromLaTeX $ fromString x
  texy (Not f) = negOf $ texy f
  texy (Or l r) = parens $ texy l `vee` texy r

instance Texy SequenceRtl where
  texy (SequenceRtl []) = emptyset
  texy (SequenceRtl [f]) = texy f
  texy (SequenceRtl (f:fs)) = texy (SequenceRtl fs) `comma` texy f

instance Texy SequenceLtr where
  texy (SequenceLtr []) = emptyset
  texy (SequenceLtr [f]) = texy f
  texy (SequenceLtr (f:fs)) = texy f `comma` texy (SequenceLtr fs)

instance Texy Sequent where
  texy (Sequent l r) = texy (SequenceRtl l) `vdash` texy (SequenceLtr r)

instance Texy Proof where
  texy (Identity s) = prfTree (textrm "I") [] s
  texy (OrLeft l r s) = prfTree (ordVee <> textrm (fromLaTeX "L")) [l, r] s
  texy (OrRight1 p s) = prfTree ((ordVee <> textrm (fromLaTeX "R")) !: (fromLaTeX "1")) [p] s
  texy (OrRight2 p s) = prfTree ((ordVee <> textrm (fromLaTeX "R")) !: (fromLaTeX "2")) [p] s
  texy (NotLeft p s) = prfTree (neg <> textrm (fromLaTeX "L")) [p] s
  texy (NotRight p s) = prfTree (neg <> textrm (fromLaTeX "R")) [p] s
  texy (WeakenLeft p s) = prfTree (textrm (fromLaTeX "WL")) [p] s
  texy (WeakenRight p s) = prfTree (textrm (fromLaTeX "WR")) [p] s
  texy (ContractLeft p s) = prfTree (textrm (fromLaTeX "CL")) [p] s
  texy (ContractRight p s) = prfTree (textrm (fromLaTeX "CR")) [p] s
  texy (PermuteLeft p s) = prfTree (textrm (fromLaTeX "PL")) [p] s
  texy (PermuteRight p s) = prfTree (textrm (fromLaTeX "PR")) [p] s

negOf :: LaTeXC l => l -> l
negOf l = neg <> l

neg :: LaTeXC l => l
neg = comm0 "neg"

parens :: LaTeXC l => l -> l
parens l = raw "(" <> l <> raw ")"

emptyset :: LaTeXC l => l
emptyset = comm0 "emptyset"

vdash :: LaTeXC l => l -> l -> l
vdash = between $ comm0 "vdash"

comma :: LaTeXC l => l -> l -> l
comma = between $ raw ","

ordVee :: LaTeXC l => l
ordVee = mathord $ comm0 "vee"

mathord :: LaTeXC l => l -> l
mathord = comm1 "mathord"

prfTree :: LaTeXC l => l -> [Proof] -> Sequent -> l
prfTree ruleName above below = liftL
  (\ruleName -> TeXComm "prftree" $
    OptArg (raw "l") :
    FixArg ruleName :
    map (\proof -> FixArg $ texy proof) above ++
    [FixArg $ texy below])
  ruleName
