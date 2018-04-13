import SequentCalc

test :: Proof
test = ContractRight
       (WeakenRight
         (Identity (Sequent [Var "x"] [Var "x"]))
         (Sequent [Var "x"] [Var "y", Var "x"]))
       (Sequent [Var "x"] [Var "x"])
