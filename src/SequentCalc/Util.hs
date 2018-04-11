{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

module SequentCalc.Util where

import SequentCalc.Syntax

{-@ measure left @-}
left :: Sequent -> [Formula]
left (Sequent xs _) = xs

{-@ measure right @-}
right :: Sequent -> [Formula]
right (Sequent _ xs) = xs

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
