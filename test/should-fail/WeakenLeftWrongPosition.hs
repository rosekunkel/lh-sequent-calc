import Prelude.Reflected
import SequentCalc

test :: Proof
test = WeakenLeft
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Sequent [Var "x", Var "y"] [Var "x"])
