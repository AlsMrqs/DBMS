module DBMS.Parser where 

import Data.Char (isAlpha,isNumber,isUpper,isLower)
import Data.Bool (bool)

subsequent :: [Char] -> Either () String
subsequent [] = Left ()
subsequent (x:xs)
    | isAlpha x || isNumber x = subsequent xs
    | otherwise               = Right ("Error: [ "++[x]++" ] invalid caracter!")

constructor :: [Char] -> Either () String
constructor [] = Right ("Error: Constructor is empty!")
constructor (x:xs) 
    | isUpper x = subsequent xs 
    | otherwise = Right ("Error: [ "++[x]++" ] cannot start a constructor!")

name :: [Char] -> Either () String
name [] = Right ("Error: Name is empty!")
name (x:xs)
    | isLower x = subsequent xs
    | otherwise = Right ("Error: [ "++[x]++" ] cannot start a name!")


