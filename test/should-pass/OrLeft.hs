import Prelude.Reflected
import SequentCalc

test :: Proof
test = OrLeft
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Identity (Sequent [Var "y"] [Var "y"]))
       (Sequent [Or (Var "x") (Var "y")] [Var "x", Var "y"])
