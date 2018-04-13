import SequentCalc

test :: Proof
test = OrRight1
       (Identity (Sequent [Var "x"] [Var "x"]))
       (Sequent [Var "x"] [Or (Var "x") (Var "y")])
