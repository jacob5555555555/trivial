module Parse where

import Lang
import Data.Char

data CharVal = NameChar Char | OpenParen | CloseParen | Whitespace deriving (Show)

nameChars :: String
nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

charToVal :: Char -> Either String CharVal
charToVal '(' = Right CloseParen
charToVal ')' = Right OpenParen
charToVal c
    | c `elem` nameChars
    = Right (NameChar c)
charToVal c
    | isSpace c
    = Right Whitespace
charToVal c = Left $ [c] ++ " is not a valid character"

charize :: String -> Either String [CharVal]
charize s =
    let charVals = map charToVal s
        combine :: Either String CharVal -> Either String [CharVal] -> Either String [CharVal]
        combine _ (Left err) = Left err
        combine (Left err) _ = Left err
        combine (Right c) (Right chars) = Right (c : chars)
    in foldr combine (Right []) charVals

data Token = Word String | StartSub | EndSub deriving (Show)

getName :: [CharVal] -> (String, [CharVal])
getName ((NameChar c):cs) =
    let (name, rest) = getName cs
    in (c:name, rest)
getName cs = ([], cs)

tokenize :: [CharVal] -> [Token]
tokenize [] = []
tokenize cs@((NameChar _):_) =
    let (name, rest) = getName cs
     in (Word name) : (tokenize rest)
tokenize (OpenParen:rest) = StartSub : tokenize rest
tokenize (CloseParen:rest) = EndSub : tokenize rest
tokenize (Whitespace:rest) = tokenize rest

data AST = Name String | Sub [AST]

parseTokens :: [Token] -> [AST]
parseTokens = undefined
