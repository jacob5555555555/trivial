module Test where

import Data.Map as Map
import Data.List as List
import Data.Maybe

import Lang

data Unique t a = Unique t a Int (Map String a) deriving (Show)

charOrder :: String
charOrder = ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

hashString :: String -> Integer
hashString [] = 0
hashString (c:cs) =
    let (Just i) = List.findIndex (==c) charOrder
     in  1 + (toInteger i) + ((genericLength charOrder) * (toInteger (hashString cs)))

v :: String -> Expression
v s = Value (hashString s)

ap :: Expression -> Expression -> Expression
ap = Apply

u :: String -> Expression
u s = Unknown (hashString s)

eq :: Expression -> Expression -> Equality
eq = Equal

prog :: Program
prog = makeProg [
                    (eq (ap (v "num") (v "Z")) (v "True")),
                    (eq (ap (v "num") (ap (v "S") (u "a"))) (ap (v "num") (u "a"))),

                    (eq (ap (v "not") (v "True")) (v "False")),
                    (eq (ap (v "not") (v "False")) (v "True"))
                ]
