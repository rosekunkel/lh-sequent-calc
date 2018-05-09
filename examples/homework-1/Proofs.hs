module Proofs where

import Prelude.Reflected
import SequentCalc

qImpliesPOrNotP :: Proof
qImpliesPOrNotP =
  (impliesRight
    (WeakenLeft
      (ContractRight
        (OrRight2
          (NotRight
            (OrRight1
              (Identity
                (Sequent [Var "p"] [Var "p"]))
              (Sequent [Var "p"] [Var "p" `Or` (Not (Var "p"))]))
            (Sequent [] [Not (Var "p"), Var "p" `Or` (Not (Var "p"))]))
          (Sequent [] [Var "p" `Or` (Not (Var "p")), Var "p" `Or` (Not (Var "p"))]))
        (Sequent [] [Var "p" `Or` (Not (Var "p"))]))
      (Sequent [Var "q"] [Var "p" `Or` (Not (Var "p"))])))

qImpliesPOrQ :: Proof
qImpliesPOrQ =
  (impliesRight
    (OrRight2
      (Identity
        (Sequent [Var "q"] [Var "q"]))
      (Sequent [Var "q"] [(Var "p") `Or` (Var "q")])))

notPImpliesPImpliesQ :: Proof
notPImpliesPImpliesQ =
  (impliesRight
    (impliesRight
      (WeakenRight
        (PermuteLeft
          (NotLeft
            (Identity
              (Sequent [Var "p"] [Var "p"]))
            (Sequent [Not (Var "p"), Var "p"] []))
          (Sequent [Var "p", Not (Var "p")] []))
        (Sequent [Var "p", Not (Var "p")] [Var "q"]))))
