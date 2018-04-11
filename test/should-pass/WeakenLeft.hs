import SequentCalc

test :: Proof
test = WeakenLeft
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Sequent [Var "y", Var "x"] [Var "x"])
