import SequentCalc

test :: Proof
test = ContractLeft
       (WeakenLeft
         (Identity (Sequent [Var "x"] [Var "x"]))
         (Sequent [Var "x", Var "y"] [Var "x"]))
       (Sequent [Var "x"] [Var "x"])
