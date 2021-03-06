module Parse where

import Lang
import Data.Char
import Control.Applicative
import Util

data CharVal = NameChar Char | OpenParen | CloseParen | Whitespace | Hash | Invalid Char  deriving (Show)

nameChars :: String
nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "'_+-=*/'[],"

charToVal :: Char -> CharVal
charToVal ')' = CloseParen
charToVal '(' = OpenParen
charToVal '#' = Hash
charToVal c
    | c `elem` nameChars
    = NameChar c
charToVal c
    | isSpace c
    = Whitespace
charToVal c = Invalid c

charize :: String -> Either String [CharVal]
--charize s =
--    let charVals = map charToVal s
--        combine :: Either String CharVal -> Either String [CharVal] -> Either String [CharVal]
--        combine _ (Left err) = Left err
--        combine (Left err) _ = Left err
--        combine (Right c) (Right chars) = Right (c : chars)
--    in foldr combine (Right []) charVals
charize [] = return []
charize (c:cs) =
    case charToVal c of
        Hash ->  return []
        Invalid c -> fail (c : " Is not a valid character")
        other -> do next <- charize cs
                    return (charToVal c : next)

data Token = Word String | StartSub | EndSub deriving (Show)

getName :: [CharVal] -> (String, [CharVal])
getName (NameChar c : cs) =
    let (name, rest) = getName cs
    in (c:name, rest)
getName cs = ([], cs)

tokenize :: [CharVal] -> [Token]
tokenize [] = []
tokenize cs@(NameChar _ :_) =
    let (name, rest) = getName cs
    in Word name : tokenize rest
tokenize (OpenParen:rest) = StartSub : tokenize rest
tokenize (CloseParen:rest) = EndSub : tokenize rest
tokenize (Whitespace:rest) = tokenize rest
tokenize (Hash:_) = []

getSubExpr :: [Token] -> ([Token], [Token])
getSubExpr toks =
    let getSubExprHelper :: Integer -> [Token] -> ([Token], [Token])
        getSubExprHelper 0 [] = ([], [])
        getSubExprHelper 0 (EndSub:toks) = ([], toks)
        getSubExprHelper n (StartSub:toks) =
            let (inSub, rest) = getSubExprHelper (n+1) toks
            in (StartSub:inSub, rest)
        getSubExprHelper n (EndSub:toks) =
            let (inSub, rest) = getSubExprHelper (n-1) toks
            in (EndSub:inSub, rest)
        getSubExprHelper n (tok:toks) =
            let (inSub, rest) = getSubExprHelper n toks
            in (tok:inSub, rest)

    in getSubExprHelper 0 toks

data AST = Name String | Sub [AST] deriving (Show)

parseTokens :: [Token] -> Either String AST
parseTokens toks =
    let parseTokensHelper :: [Token] -> Either String [AST]
        parseTokensHelper [] = Right []
        parseTokensHelper (Word str : rest) =
            (:) <$> return (Name str) <*> parseTokensHelper rest
        parseTokensHelper (StartSub : toks) =
            let (sub, rest) = getSubExpr toks
            in (:) <$>  fmap Sub (parseTokensHelper sub) <*> parseTokensHelper rest
        parseTokensHelper (errTok:toks) = Left (show errTok ++ " caused an error")
     in case parseTokensHelper toks of
          Left err -> Left err
          Right [a] -> return a
          Right as -> return (Sub as)

parseAST :: NameMap -> AST ->  Either String (NameMap, Expression)
parseAST names (Name name@('_':rest)) =
    let (newNames, id) = findOrMake names name
    in  return (newNames, Unknown id)
parseAST names (Name name) =
    let (newNames, id) = findOrMake names name
    in  return (newNames, Value id)
parseAST names (Sub [a,b]) =
    case parseAST names a of
      Left err -> Left err
      Right (names1, ea) ->
          case parseAST names1 b of
            Left err -> Left err
            Right (names2, eb) ->
                return (names2, Apply ea eb)
parseAST names (Sub (a:b:asts)) =
    parseAST names (Sub (Sub [a,b] : asts))
parseAST names (Sub [a]) = Left "cant have Sub with only one element"
parseAST names (Sub []) = Left "cant have Sub with no elements"



data NameMap = NameMap (Association String Id) Id deriving (Show)

emptyNameMap :: NameMap
emptyNameMap = NameMap Util.empty 0

findOrMake :: NameMap -> String -> (NameMap, Id)
findOrMake names@(NameMap ass _) s
    | (Just b) <- lookupA s ass
    = (names, b)
findOrMake (NameMap ass id) s =
    let nextId = id + 1
        newAss = insert s nextId ass
    in (NameMap newAss nextId, nextId)

--data ParsedLine = Empty NameMap | Result NameMap Expression
type ParsedLine = (NameMap, Expression)

parse :: NameMap -> String -> Either String (Maybe ParsedLine)
parse names s =
    case charize s of
      Left err -> Left ("Error in charize: " ++ err)
      Right charVals ->
          let tokens = tokenize charVals
          in if null tokens then Right Nothing else
             case parseTokens tokens of
             Left err -> Left ("Error in parsing tokens: " ++ err)
             Right ast ->
                 case parseAST names ast of
                   Left err -> Left ("Error in parseAST: " ++ err)
                   Right res -> Right $ Just res
