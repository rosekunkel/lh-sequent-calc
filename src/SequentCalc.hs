module SequentCalc
  ( module SequentCalc
  , module SequentCalc.Syntax
  , module SequentCalc.Util
  ) where

import Prelude.Reflected

import SequentCalc.Syntax
import SequentCalc.Util

-- Currently infix annotations are not imported, so we need to
-- duplicate them here. See
-- https://github.com/ucsd-progsys/liquidhaskell/issues/1123
{-@ infixr 5 ++ @-}

data Proof = Identity Sequent
           | Cut Proof Proof Sequent
           | OrLeft Proof Proof Sequent
           | OrRight1 Proof Sequent
           | OrRight2 Proof Sequent
           | NotLeft Proof Sequent
           | NotRight Proof Sequent
           | WeakenLeft Proof Sequent
           | WeakenRight Proof Sequent
           | ContractLeft Proof Sequent
           | ContractRight Proof Sequent
           | PermuteLeft Proof Sequent
           | PermuteRight Proof Sequent
  deriving (Eq)

{-@
  data Proof where
    Identity :: {s:Sequent | len (left s) == 1 &&
                             left s == right s}
      -> Proof
  | Cut :: pl:Proof -> pr:Proof
      -> {s:Sequent | len (right (conclusion pl)) > 0 &&
                      len (left (conclusion pr)) > 0 &&
                      last (right (conclusion pl)) == last (left (conclusion pr)) &&
                      left s == init (left (conclusion pr)) ++ left (conclusion pl)  &&
                      right s == init (right (conclusion pl)) ++ right (conclusion pr)}
      -> Proof
  | OrLeft :: p0:Proof -> p1:Proof
      -> {s:Sequent | len (left (conclusion p0)) > 0 &&
                      len (left (conclusion p1)) > 0 &&
                      OrHeads (left s) (left (conclusion p0)) (left (conclusion p1)) &&
                      right s == (right (conclusion p0)) ++ (right (conclusion p1))}
      -> Proof
  | OrRight1 :: p:Proof
      -> {s:Sequent | len (right s) > 0 &&
                      len (right (conclusion p)) > 0 &&
                      OrHead1 (right s) (right (conclusion p)) &&
                      left s == left (conclusion p)}
      -> Proof
  | OrRight2 :: p:Proof
      -> {s:Sequent | len (right s) > 0 &&
                      len (right (conclusion p)) > 0 &&
                      OrHead2 (right s) (right (conclusion p)) &&
                      left s == left (conclusion p)}
      -> Proof
  | NotLeft :: p:Proof
      -> {s:Sequent | len (left s) > 0 &&
                      len (right (conclusion p)) > 0 &&
                      head (left s) == (Not (head (right (conclusion p)))) &&
                      (tail (left s)) == (left (conclusion p)) &&
                      right s == tail (right (conclusion p))}
      -> Proof
  | NotRight :: p:Proof
      -> {s:Sequent | len (right s) > 0 &&
                      len (left (conclusion p)) > 0 &&
                      head (right s) == (Not (head (left (conclusion p)))) &&
                      (tail (right s)) == (right (conclusion p)) &&
                      left s == tail (left (conclusion p))}
      -> Proof
  | WeakenLeft :: p:Proof
      -> {s:Sequent | len (left s) > 0 &&
                      tail (left s) == left (conclusion p) &&
                      right s == right (conclusion p)}
      -> Proof
  | WeakenRight :: p:Proof
      -> {s:Sequent | len (right s) > 0 &&
                      tail (right s) == right (conclusion p) &&
                      left s == left (conclusion p)}
      -> Proof
  | ContractLeft :: p:Proof
      -> {s:Sequent | right s == right (conclusion p) &&
                      len (left s) > 0 &&
                      len (left (conclusion p)) >= 2 &&
                      RepeatHeads (left s) (left (conclusion p))}
      -> Proof
  | ContractRight :: p:Proof
      -> {s:Sequent | left s == left (conclusion p) &&
                      len (right s) > 0 &&
                      len (right (conclusion p)) >= 2 &&
                      RepeatHeads (right s) (right (conclusion p))}
      -> Proof
  | PermuteLeft :: p:Proof
      -> {s:Sequent | right s == right (conclusion p) &&
                      len (left s) >= 2 &&
                      len (left s) == len (left (conclusion p)) &&
                      isTransposition (left s) (left (conclusion p))}
      -> Proof
  | PermuteRight :: p:Proof
      -> {s:Sequent | left s == left (conclusion p) &&
                      len (right s) >= 2 &&
                      len (right s) == len (right (conclusion p)) &&
                      isTransposition (right s) (right (conclusion p))}
      -> Proof
@-}

{-@ measure conclusion @-}
conclusion :: Proof -> Sequent
conclusion (Identity s) = s
conclusion (Cut _ _ s) = s
conclusion (OrLeft _ _ s) = s
conclusion (OrRight1 _ s) = s
conclusion (OrRight2 _ s) = s
conclusion (NotLeft _ s) = s
conclusion (NotRight _ s) = s
conclusion (WeakenLeft _ s) = s
conclusion (WeakenRight _ s) = s
conclusion (ContractLeft _ s) = s
conclusion (ContractRight _ s) = s
conclusion (PermuteLeft _ s) = s
conclusion (PermuteRight _ s) = s

{-@ reflect andLeft1 @-}
{-@ andLeft1 :: {p:Proof | len (left (conclusion p)) >= 1}
             -> Formula
             -> Proof @-}
andLeft1 :: Proof -> Formula -> Proof
andLeft1 proof q =
  case conclusion proof of
    (Sequent (p:l) r) ->
      (NotLeft
        (OrRight1
          (NotRight
            proof
            (Sequent l ((Not p):r)))
          (Sequent l (((Not p) `Or` (Not q)):r)))
        (Sequent ((Not ((Not p) `Or` (Not q))):l) r))

{-@ reflect andLeft2 @-}
{-@ andLeft2 :: {p:Proof | len (left (conclusion p)) >= 1}
             -> Formula
             -> Proof @-}
andLeft2 :: Proof -> Formula -> Proof
andLeft2 proof p =
  case conclusion proof of
    (Sequent (q:l) r) ->
      (NotLeft
        (OrRight2
          (NotRight
            proof
            (Sequent l ((Not q):r)))
          (Sequent l (((Not p) `Or` (Not q)):r)))
        (Sequent ((Not ((Not p) `Or` (Not q))):l) r))

{-@ reflect andRight @-}
{-@ andRight :: {p1:Proof | len (right (conclusion p1)) >= 1}
             -> {p2:Proof | len (right (conclusion p2)) >= 1}
             -> Proof @-}
andRight proof1 proof2 =
  case (conclusion proof1, conclusion proof2) of
    (Sequent l1 (p:r1), Sequent l2 (q:r2)) ->
      (NotRight
        (OrLeft
          (NotLeft
            proof1
            (Sequent ((Not p):l1) r1))
          (NotLeft
            proof2
            (Sequent ((Not q):l2) r2))
          (Sequent (((Not p) `Or` (Not q)):l2 ++ l1) (r1 ++ r2)))
        (Sequent (l2 ++ l1) ((Not ((Not p) `Or` (Not q))):r1 ++ r2)))

{-@ reflect impliesLeft @-}
{-@ impliesLeft :: {p1:Proof | len (right (conclusion p1)) >= 1}
                -> {p2:Proof | len (left (conclusion p2)) >= 1}
                -> Proof @-}
impliesLeft :: Proof -> Proof -> Proof
impliesLeft proof1 proof2 =
  case (conclusion proof1, conclusion proof2) of
    (Sequent l1 (p:r1), Sequent (q:l2) r2) ->
      (OrLeft
        (NotLeft
          proof1
          (Sequent ((Not p):l1) r1))
        proof2
        (Sequent (((Not p) `Or` q):l2 ++ l1) (r1 ++ r2)))

{-@ reflect impliesRight @-}
{-@ impliesRight :: {p:Proof | len (right (conclusion p)) >= 1 &&
                               len (left (conclusion p)) >= 1}
                 -> Proof @-}
impliesRight :: Proof -> Proof
impliesRight proof =
  case conclusion proof of
    (Sequent (p:l) (q:r)) ->
      (ContractRight
        (OrRight1
         (NotRight
           (OrRight2
             proof
             (Sequent (p:l) (((Not p) `Or` q):r)))
           (Sequent l ((Not p):((Not p) `Or` q):r)))
          (Sequent l (((Not p) `Or` q):((Not p) `Or` q):r)))
        (Sequent l (((Not p) `Or` q):r)))
