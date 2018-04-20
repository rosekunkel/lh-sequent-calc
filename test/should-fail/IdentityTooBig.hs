import Prelude.Reflected
import SequentCalc

test :: Proof
test = Identity (Sequent [Var "x", Var "x"] [Var "x", Var "x"])
