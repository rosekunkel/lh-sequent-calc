{-# LANGUAGE NoImplicitPrelude #-}

import ReflectedPrelude
import SequentCalc

test :: Proof
test = PermuteLeft
       (WeakenLeft
         (Identity (Sequent [Var "x"] [Var "x"]))
         (Sequent [Var "y", Var "x"] [Var "x"]))
       (Sequent [Var "x", Var "y"] [Var "x"])
