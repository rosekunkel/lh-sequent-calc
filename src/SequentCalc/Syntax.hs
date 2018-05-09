module SequentCalc.Syntax where

import Prelude.Reflected

data Formula = Var String
             | Not Formula
             | Or Formula Formula
  deriving (Eq)

data Sequent = Sequent [Formula] [Formula]
  deriving (Eq)

{-@ reflect formulaAnd @-}
p `formulaAnd` q = (Not ((Not p) `Or` (Not q)))

{-@ reflect formulaImplies @-}
formulaImplies :: Formula -> Formula -> Formula
p `formulaImplies` q = (Not p) `Or` q
