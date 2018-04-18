{-# LANGUAGE NoImplicitPrelude #-}

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

module ReflectedPrelude
  ( module ReflectedPrelude
  , module Prelude
  ) where

import Prelude hiding ((++), last, init)

infixr 5  ++
{-@ infixr 5 ++ @-}

{-@ reflect ++ @-}
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

{-@ reflect last @-}
{-@ last :: {xs:_ | len xs >= 1} -> _ @-}
last :: [a] -> a
last [x] = x
last (_:xs) = last xs

{-@ reflect init @-}
{-@ init :: {xs:_ | len xs >= 1} -> _ @-}
init :: [a] -> [a]
init [_] = []
init (x:xs) = x : init xs
