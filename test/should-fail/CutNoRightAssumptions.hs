{-# LANGUAGE NoImplicitPrelude #-}

import ReflectedPrelude
import SequentCalc

test :: Proof
test = Cut
         (Identity
           (Sequent [Var "x"] [Var "x"]))
         (NotRight
           (Identity
             (Sequent [Var "y"] [Var "y"]))
           (Sequent [] [Not (Var "y"), Var "y"]))
         (Sequent [Var "x"] [Var "x", Not (Var "y"), Var "y"])
