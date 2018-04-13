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

{-@ reflect repeatHeads @-}
{-@ repeatHeads :: xs:{[Formula] | len xs > 0} -> {ys:[Formula] | len ys >= 2} -> Bool @-}
repeatHeads :: [Formula] -> [Formula] -> Bool
repeatHeads (x:_) (y:z:_) = x == y && y == z

{-@ reflect sequenceEqual @-}
sequenceEqual :: [Formula] -> [Formula] -> Bool
sequenceEqual [] [] = True
sequenceEqual (x:xs) (y:ys) = x == y && sequenceEqual xs ys
sequenceEqual _ _ = False

{-@ reflect sequenceAppend @-}
sequenceAppend :: [Formula] -> [Formula] -> [Formula]
sequenceAppend (x:xs) ys = x:(sequenceAppend xs ys)
sequenceAppend [] xs = xs

{-@ reflect orHeads @-}
{-@ orHeads :: xs:{[Formula] | len xs > 0} -> {ys:[Formula] | len ys > 0} -> {zs:[Formula] | len zs > 0} -> Bool @-}
orHeads :: [Formula] -> [Formula] -> [Formula] -> Bool
orHeads (x:xs) (y:ys) (z:zs) =
   x == (Or y z) &&
   xs == sequenceAppend zs ys

{-@ reflect orHead1 @-}
{-@ orHead1 :: xs:{[Formula] | len xs > 0} -> {ys:[Formula] | len ys > 0} -> Bool @-}
orHead1 :: [Formula] -> [Formula] -> Bool
orHead1 ((Or x _):xs) (y:ys) = x == y && xs == ys
orHead1 _ _ = False

{-@ reflect orHead2 @-}
{-@ orHead2 :: xs:{[Formula] | len xs > 0} -> {ys:[Formula] | len ys > 0} -> Bool @-}
orHead2 :: [Formula] -> [Formula] -> Bool
orHead2 ((Or _ x):xs) (y:ys) = x == y && xs == ys
orHead2 _ _ = False

{-@ reflect isTransposition @-}
{-@ isTransposition :: xs:{[Formula] | len xs >= 2} -> {ys:[Formula] | len ys = len xs} -> Bool @-}
isTransposition [x1, x2] [y1, y2] = x1 == y2 && x2 == y1
isTransposition (x1:x2:xs) (y1:y2:ys) =
  isTransposition [x1, x2] [y1, y2] && sequenceEqual xs ys ||
  x1 == y1 && isTransposition (x2:xs) (y2:ys)
