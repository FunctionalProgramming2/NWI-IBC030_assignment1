> {-# LANGUAGE UnicodeSyntax #-}
>
> module Tree
> where

Leaf trees.

> data Tree elem  =  Leaf elem | Tree elem :^: Tree elem
>   deriving (Show, Eq, Ord)
