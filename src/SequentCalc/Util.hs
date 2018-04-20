module SequentCalc.Util where

import Prelude.Reflected
import SequentCalc.Syntax

-- Currently infix annotations are not imported, so we need to
-- duplicate them here. See
-- https://github.com/ucsd-progsys/liquidhaskell/issues/1123
{-@ infixr 5 ++ @-}

{-@ measure left @-}
left :: Sequent -> [Formula]
left (Sequent xs _) = xs

{-@ measure right @-}
right :: Sequent -> [Formula]
right (Sequent _ xs) = xs

{-@ measure isOr @-}
isOr :: Formula -> Bool
isOr (Or _ _) = True
isOr _ = False

{-@ measure leftConjunct @-}
{-@ leftConjunct :: {f:Formula | isOr f} -> Formula @-}
leftConjunct :: Formula -> Formula
leftConjunct (Or l _) = l

{-@ measure rightConjunct @-}
{-@ rightConjunct :: {f:Formula | isOr f} -> Formula @-}
rightConjunct :: Formula -> Formula
rightConjunct (Or _ r) = r

{-@ predicate RepeatHeads XS YS = head XS == head YS && head YS == head (tail YS) @-}
{-@ predicate OrHeads XS YS ZS = XS == cons (Or (head YS) (head ZS)) (tail ZS ++ tail YS) @-}
{-@ predicate OrHead1 XS YS = leftConjunct (head XS) == head YS && tail XS == tail YS @-}
{-@ predicate OrHead2 XS YS = rightConjunct (head XS) == head YS && tail XS == tail YS @-}

{-@ reflect sequenceEqual @-}
sequenceEqual :: [Formula] -> [Formula] -> Bool
sequenceEqual [] [] = True
sequenceEqual (x:xs) (y:ys) = x == y && sequenceEqual xs ys
sequenceEqual _ _ = False

{-@ reflect isTransposition @-}
{-@ isTransposition :: xs:{[Formula] | len xs >= 2} -> {ys:[Formula] | len ys = len xs} -> Bool @-}
isTransposition [x1, x2] [y1, y2] = x1 == y2 && x2 == y1
isTransposition (x1:x2:xs) (y1:y2:ys) =
  isTransposition [x1, x2] [y1, y2] && sequenceEqual xs ys ||
  x1 == y1 && isTransposition (x2:xs) (y2:ys)
