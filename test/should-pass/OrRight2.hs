{-# LANGUAGE NoImplicitPrelude #-}

import ReflectedPrelude
import SequentCalc

test :: Proof
test = OrRight2
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Sequent [Var "x"] [Or (Var "y") (Var "x")])
