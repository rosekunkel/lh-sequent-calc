{-# LANGUAGE OverloadedStrings #-}

module SequentCalc.Latex where

import Prelude.Reflected
import SequentCalc

import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath (math, text, vee, (!:))

newtype SequenceRtl = SequenceRtl [Formula]
newtype SequenceLtr = SequenceLtr [Formula]
newtype RawProof = RawProof Proof

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
  texy proof = raisebox
    (CustomMeasure ("-" <> comm0 "height"))
    Nothing Nothing
    (texy (RawProof proof))

instance Texy RawProof where
  texy (RawProof (Identity s)) = prfTree (fromLaTeX "I") [] s
  texy (RawProof (Cut l r s)) = prfTree (fromLaTeX "Cut") [l, r] s
  texy (RawProof (OrLeft l r s)) = prfTree (math (ordVee <> text (fromLaTeX "L"))) [l, r] s
  texy (RawProof (OrRight1 p s)) = prfTree (math ((ordVee <> text (fromLaTeX "R")) !: (fromLaTeX "1"))) [p] s
  texy (RawProof (OrRight2 p s)) = prfTree (math ((ordVee <> text (fromLaTeX "R")) !: (fromLaTeX "2"))) [p] s
  texy (RawProof (NotLeft p s)) = prfTree (math (neg <> text (fromLaTeX "L"))) [p] s
  texy (RawProof (NotRight p s)) = prfTree (math (neg <> text (fromLaTeX "R"))) [p] s
  texy (RawProof (WeakenLeft p s)) = prfTree (fromLaTeX "WL") [p] s
  texy (RawProof (WeakenRight p s)) = prfTree (fromLaTeX "WR") [p] s
  texy (RawProof (ContractLeft p s)) = prfTree (fromLaTeX "CL") [p] s
  texy (RawProof (ContractRight p s)) = prfTree (fromLaTeX "CR") [p] s
  texy (RawProof (PermuteLeft p s)) = prfTree (fromLaTeX "PL") [p] s
  texy (RawProof (PermuteRight p s)) = prfTree (fromLaTeX "PR") [p] s

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
    FixArg (parens ruleName) :
    map (\proof -> FixArg $ texy (RawProof proof)) above ++
    [FixArg $ texy below])
  ruleName
