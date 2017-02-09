> {-# LANGUAGE UnicodeSyntax #-}
>
> module Huffman
> where
> import Unicode
> import Satellite
> import Tree
> import Data.List
> import Data.Maybe

author: Hendrik Werner s4549775
-------------------------------------------------------------------------------

Exercise 1
==========

Warm-up: constructing a frequency table.

< frequencies  ∷  (Ord char) ⇒ [char] → [With Int char]

> frequencies :: (Ord char) => [char] -> [With Int char]
> frequencies = foldr add []
>     where add c [] = [1 :- c]
>           add c (w@(a :- b):ws)
>               | c == b = ((a + 1) :- b):ws
>               | otherwise = w : add c ws

-------------------------------------------------------------------------------

Exercise 2.1
============

Constructing a Huffman tree.

< huffman ∷ [With Int char] → Tree char

> huffman :: [With Int char] -> Tree char
> huffman = snd . reduce combine . map leafify . sort
>     where combine (t1:t2:ts) = insert (merge t1 t2) ts
>           leafify (a :- b) = (a :- Leaf b)
>           merge (a1 :- b1) (a2 :- b2) = a2 + a2 :- b1 :^: b2
>           reduce f [a] = a
>           reduce f a = reduce f $ f a
>           snd (a :- b) = b

Exercise 2.2
============

I applied the huffman algorithm to the English letter frequencies from Wikipedia

> englishFrequencies :: [(With Int Char)]
> englishFrequencies = [
>     (8167 :- 'a')
>     ,(1492 :- 'b')
>     ,(2782 :- 'c')
>     ,(4253 :- 'd')
>     ,(12702 :- 'e')
>     ,(2228 :- 'f')
>     ,(2015 :- 'g')
>     ,(6094 :- 'h')
>     ,(6966 :- 'i')
>     ,(153 :- 'j')
>     ,(772 :- 'k')
>     ,(4025 :- 'l')
>     ,(2406 :- 'm')
>     ,(6749 :- 'n')
>     ,(7507 :- 'o')
>     ,(1929 :- 'p')
>     ,(95 :- 'q')
>     ,(5987 :- 'r')
>     ,(6327 :- 's')
>     ,(9056 :- 't')
>     ,(2758 :- 'u')
>     ,(978 :- 'v')
>     ,(2360 :- 'w')
>     ,(150 :- 'x')
>     ,(1974 :- 'y')
>     ,(74 :- 'z')]

and got the following huffman tree

((Leaf 't' :^: ((Leaf 'f' :^: Leaf 'w') :^: (Leaf 'm' :^: Leaf 'u'))) :^: (((Leaf 'c' :^: (Leaf 'v' :^: Leaf 'b')) :^: Leaf 'r') :^: (Leaf 'h' :^: Leaf 's'))) :^: ((Leaf 'e' :^: (Leaf 'n' :^: Leaf 'i')) :^: ((Leaf 'o' :^: (((((Leaf 'z' :^: Leaf 'q') :^: (Leaf 'x' :^: Leaf 'j')) :^: Leaf 'k') :^: Leaf 'p') :^: Leaf 'l')) :^: (Leaf 'a' :^: ((Leaf 'y' :^: Leaf 'g') :^: Leaf 'd'))))

and the following letter codes

[
    ('t',[O,O,O]),
    ('f',[O,O,I,O,O]),
    ('w',[O,O,I,O,I]),
    ('m',[O,O,I,I,O]),
    ('u',[O,O,I,I,I]),
    ('c',[O,I,O,O,O]),
    ('v',[O,I,O,O,I,O]),
    ('b',[O,I,O,O,I,I]),
    ('r',[O,I,O,I]),
    ('h',[O,I,I,O]),
    ('s',[O,I,I,I]),
    ('e',[I,O,O]),
    ('n',[I,O,I,O]),
    ('i',[I,O,I,I]),
    ('o',[I,I,O,O]),
    ('z',[I,I,O,I,O,O,O,O,O]),
    ('q',[I,I,O,I,O,O,O,O,I]),
    ('x',[I,I,O,I,O,O,O,I,O]),
    ('j',[I,I,O,I,O,O,O,I,I]),
    ('k',[I,I,O,I,O,O,I]),
    ('p',[I,I,O,I,O,I]),
    ('l',[I,I,O,I,I]),
    ('a',[I,I,I,O]),
    ('y',[I,I,I,I,O,O]),
    ('g',[I,I,I,I,O,I]),
    ('d',[I,I,I,I,I])
]

which I mapped to (Char, code length) pairs and mapped these to (With Int Char)
and sorted them. I got back (almost, due to equal lengths) the original ranking.

[
    3 :- 't',
    3 :- 'e',
    4 :- 'r',
    4 :- 'h',
    4 :- 's',
    4 :- 'n',
    4 :- 'i',
    4 :- 'o',
    4 :- 'a',
    5 :- 'f',
    5 :- 'w',
    5 :- 'm',
    5 :- 'u',
    5 :- 'c',
    5 :- 'l',
    5 :- 'd',
    6 :- 'v',
    6 :- 'b',
    6 :- 'p',
    6 :- 'y',
    6 :- 'g',
    7 :- 'k',
    9 :- 'z',
    9 :- 'q',
    9 :- 'x',
    9 :- 'j'
]

This confirms that the algorithms works and more common letters get shorter
codes.

-------------------------------------------------------------------------------

Exercise 3.1
============

Encoding ASCII text.

> data Bit = O | I
>   deriving (Show, Eq, Ord)

< encode ∷ (Eq char) ⇒ Tree char → [char] → [Bit]

> encode :: (Eq char) => Tree char -> [char] -> [Bit]
> encode t cs = concat $ map (bitCode (codes t)) cs
>     where bitCode ((ch, bits):cds) c
>               | ch == c = bits
>               | otherwise = bitCode cds c

< codes ∷ Tree char → [(char, [Bit])]

> codes :: Tree char -> [(char, [Bit])]
> codes t = cds [] t
>     where cds bs (Leaf c) = [(c, reverse bs)]
>           cds bs (l :^: r) = cds (O:bs) l ++ cds (I:bs) r

-------------------------------------------------------------------------------

Exercise 4
==========

Decoding a Huffman binary.

< decode ∷ Tree char → [Bit] → [char]

> decode :: Tree char -> [Bit] -> [char]
> decode t [] = []
> decode t (b:bs) = dec [b] bs
>     where dec c [] = [fromJust (fromBits code c)]
>           dec c (b:bs)
>               | isJust mc = fromJust mc : dec [b] bs
>               | otherwise = dec (c ++ [b]) bs
>               where mc = fromBits code c
>           code = codes t

> fromBits :: [(char, [Bit])] -> [Bit] -> Maybe char
> fromBits [] _ = Nothing
> fromBits ((c, bs):cs) bits
>     | bs == bits = Just c
>     | otherwise = fromBits cs bits

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
