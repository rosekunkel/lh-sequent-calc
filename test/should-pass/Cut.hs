import Prelude.Reflected
import SequentCalc

test :: Proof
test = Cut
         (PermuteRight
           (WeakenRight
             (Identity
               (Sequent [Var "x"] [Var "x"]))
             (Sequent [Var "x"] [Var "y", Var "x"]))
           (Sequent [Var "x"] [Var "x", Var "y"]))      
         (PermuteLeft
           (WeakenLeft
             (Identity
               (Sequent [Var "z"] [Var "z"]))
             (Sequent [Var "y", Var "z"] [Var "z"]))
           (Sequent [Var "z", Var "y"] [Var "z"]))
         (Sequent [Var "z", Var "x"] [Var "x", Var "z"])
