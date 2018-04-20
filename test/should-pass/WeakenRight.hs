import Prelude.Reflected
import SequentCalc

test :: Proof
test = WeakenRight
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Sequent [Var "x"] [Var "y", Var "x"])
