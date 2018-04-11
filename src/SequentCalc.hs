{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

module SequentCalc
  ( module SequentCalc
  , module SequentCalc.Syntax
  , module SequentCalc.Util
  ) where

import SequentCalc.Syntax
import SequentCalc.Util

data Proof = Identity Sequent
           | WeakenLeft Proof Sequent
           | PermuteLeft Proof Sequent
  deriving (Eq)

{-@
  data Proof where
    Identity :: {s:Sequent | len (left s) == 1 &&
                             left s == right s}
      -> Proof
  | WeakenLeft :: p:Proof
      -> {s:Sequent | len (left s) > 0 &&
                      tail (left s) == left (conclusion p) &&
                      right s == right (conclusion p)}
      -> Proof
  | PermuteLeft :: p:Proof
      -> {s:Sequent | right s == right (conclusion p) &&
                      len (left s) >= 2 &&
                      len (left s) == len (left (conclusion p)) &&
                      isTransposition (left s) (left (conclusion p))}
      -> Proof
@-}

{-@ measure conclusion @-}
conclusion :: Proof -> Sequent
conclusion (Identity s) = s
conclusion (WeakenLeft _ s) = s
conclusion (PermuteLeft _ s) = s
