module Parser where 

import Data.Char    (isAlpha,isNumber,isUpper,isLower)
import Data.Bool    (bool)
import Data.Either  (fromRight)

import Prelude hiding (lex)

type Identity = [Char]
type Error    = [Char]
type Frame    = [[Char]]

subsequent :: [Char] -> Either () [Char]
subsequent [] = Left ()
subsequent (x:xs)
    | isAlpha x || isNumber x = subsequent xs
    | otherwise               = Right ("Error: [ "++[x]++" ] invalid caracter!")

constructor :: [Char] -> Either () [Char]
constructor [] = Right ("Error: constructor is empty!")
constructor (x:xs) 
    | isUpper x = subsequent xs 
    | otherwise = Right ("Error: [ "++[x]++" ] cannot start a constructor!")

var :: Identity -> Frame -> Either Frame Error
var _ [] = Right ("Error: Empty frame!")
var "constructor" (x:xs) = _data_ x xs

----------------- Parser --------------------

_data_ :: [Char] -> Frame -> Either Frame Error
_data_     [] _ = Right ("Error: constructor not found!")
_data_ (x:xs) z 
    | isUpper x = _body_ xs z
    | otherwise = Right ("Error: constructor must start with a capital letter!")

_body_ :: [Char] -> Frame -> Either Frame Error
_body_     [] z  = Left z
_body_ (x:xs) z
    | isAlpha  x = _body_ xs z
    | isNumber x = _body_ xs z
    | otherwise  = Right ("Error: [ "++[x]++" ] invalid character!")

-------------------- LEXICAL ----------------

lex :: [Char] -> [Char]
lex [] = []
lex (x:xs) 
    | (==) x ' ' || (==) '_' x || isAlpha  x || isNumber x = x : lex xs
    | otherwise = ' ' : x : ' ' : lex xs

