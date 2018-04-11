{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

module SequentCalc.Syntax where

data Formula = Var String
             | Not Formula
             | Or Formula Formula
  deriving (Eq)

data Sequent = Sequent [Formula] [Formula]
  deriving (Eq)
