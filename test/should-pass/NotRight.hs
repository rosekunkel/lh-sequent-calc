import Prelude.Reflected
import SequentCalc

test :: Proof
test = NotRight
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Sequent [] [Not (Var "x"), Var "x"])
