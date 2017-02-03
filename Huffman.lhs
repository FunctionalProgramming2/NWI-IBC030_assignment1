> {-# LANGUAGE UnicodeSyntax #-}
>
> module Huffman
> where
> import Unicode
> import Satellite
> import Tree

-------------------------------------------------------------------------------

Warm-up: constructing a frequency table.

< frequencies  ∷  (Ord char) ⇒ [char] → [With Int char]

-------------------------------------------------------------------------------

Constructing a Huffman tree.

< huffman ∷ [With Int char] → Tree char

-------------------------------------------------------------------------------

Encoding ASCII text.

> data Bit = O | I
>   deriving (Show, Eq, Ord)

< encode ∷ (Eq char) ⇒ Tree char → [char] → [Bit]

< codes ∷ Tree char → [(char, [Bit])]

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
