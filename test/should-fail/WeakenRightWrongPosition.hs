import SequentCalc

test :: Proof
test = WeakenLeft
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Sequent [Var "x"] [Var "x", Var "y"])
