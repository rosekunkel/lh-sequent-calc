module SequentCalc.Syntax where

import Prelude.Reflected

data Formula = Var String
             | Not Formula
             | Or Formula Formula
  deriving (Eq)

data Sequent = Sequent [Formula] [Formula]
  deriving (Eq)
