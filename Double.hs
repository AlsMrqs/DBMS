module Parse.Double where

import Prelude hiding   (lex)
import Data.Char        (isNumber)
import Queue

key = ['.']

type Error = [Char]

parse :: [Char] -> Either [Char] Error
parse str = let frame = scan str in
    either (\x -> if rule x then Left (concat x) else Right ("Error: [ "++str++" ] invalid input!")) 
           (\e -> Right e) 
           $ scan str

lex :: [Char] -> Either ([Char],[Char]) Error
lex s = f Queue s
    where
    f :: Queue Char -> [Char] -> Either ([Char],[Char]) Error
    f q []     = Left (list q, [])
    f q (x:xs) | elem x key = if empty q then Left ([x],xs) else Left (list q, (x:xs))
               | isNumber x = f (enq x q) xs
               | otherwise  = Right ("Error! lex: [ "++[x]++" ] ")

scan :: [Char] -> Either [String] Error
scan [] = Right ("Error: scan: empty string")
scan xs = f Queue xs
    where
    f :: Queue [Char] -> [Char] -> Either [String] Error
    f s z = either (\(a,b) -> if null a then Left (list s) else f (enq a s) b) 
                   (\e -> Right e) 
                   $ lex z 

rule :: [String] -> Bool
rule str | length str == 1 = all isNumber . head $ str
         | length str == 3 = (\(x:y:z:[]) -> all isNumber x && elem y [key] && all isNumber z) str 
         | otherwise       = False

