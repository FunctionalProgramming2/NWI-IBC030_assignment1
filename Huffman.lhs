> {-# LANGUAGE UnicodeSyntax #-}
>
> module Huffman
> where
> import Unicode
> import Satellite
> import Tree
> import Data.List

author: Hendrik Werner s4549775
-------------------------------------------------------------------------------

Warm-up: constructing a frequency table.

< frequencies  ∷  (Ord char) ⇒ [char] → [With Int char]

> frequencies :: (Ord char) => [char] -> [With Int char]
> frequencies = foldr add []
>     where add c [] = [1 :- c]
>           add c (w@(a :- b):ws)
>               | c == b = ((a + 1) :- b):ws
>               | otherwise = w : add c ws

-------------------------------------------------------------------------------

Constructing a Huffman tree.

< huffman ∷ [With Int char] → Tree char

> huffman :: [With Int char] -> Tree char
> huffman w = snd $ reduce combine $ map leafify $ sort w
>     where combine (t1:t2:ts) = sort $ merge t1 t2 : ts
>           leafify (a :- b) = (a :- Leaf b)
>           merge (a1 :- b1) (a2 :- b2) = a2 + a2 :- b1 :^: b2
>           reduce f [a] = a
>           reduce f a = reduce f $ f a
>           snd (a :- b) = b

-------------------------------------------------------------------------------

Encoding ASCII text.

> data Bit = O | I
>   deriving (Show, Eq, Ord)

< encode ∷ (Eq char) ⇒ Tree char → [char] → [Bit]

< codes ∷ Tree char → [(char, [Bit])]

> codes :: Tree char -> [(char, [Bit])]
> codes t = cds [] t
>     where cds bs (Leaf c) = [(c, bs)]
>           cds bs (l :^: r) = cds (O:bs) l ++ cds (I:bs) r

-------------------------------------------------------------------------------

Decoding a Huffman binary.

< decode ∷ Tree char → [Bit] → [char]

-------------------------------------------------------------------------------

Some test data.

> hw, why ∷ String
> hw =
>   "hello world"

code = huffman (frequencies hw)
encode code hw
decode code it
decode code it == hw

> why =
>   "As software becomes more and more complex, it\n\
>   \is  more  and  more important to structure it\n\
>   \well.  Well-structured  software  is  easy to\n\
>   \write,   easy   to   debug,  and  provides  a\n\
>   \collection  of modules that can be re-used to\n\
>   \reduce future programming costs. Conventional\n\
>   \languages place a conceptual limit on the way\n\
>   \problems   can   be  modularised.  Functional\n\
>   \languages  push  those  limits  back. In this\n\
>   \paper we show that two features of functional\n\
>   \languages    in    particular,   higher-order\n\
>   \functions and lazy evaluation, can contribute\n\
>   \greatly  to  modularity.  Since modularity is\n\
>   \the key to successful programming, functional\n\
>   \languages  are  vitally important to the real\n\
>   \world."

code = huffman (frequencies why)
encode code why
decode code it
decode code it == why
