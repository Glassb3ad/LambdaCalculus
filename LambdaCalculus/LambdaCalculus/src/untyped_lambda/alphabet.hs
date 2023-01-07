module ALPHABET where

data ALPHABET = VARIABLE | LAMBDA | PARENTHESE 

data VARIABLE = VARIABLE Char
                deriving (Eq, Show, Read) 
newtype LAMBDA = LAMBDA VARIABLE
data PARENTHESE = LEFT | RIGHT 

{- 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' 
                | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' 
                | 'o' | 'p' | 'q' | 'r' | 'p' | 'q' | 'r'
                | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
                | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' 
                | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' 
                | 'O' | 'P' | 'Q' | 'R' | 'P' | 'Q' | 'R'
                | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' -}