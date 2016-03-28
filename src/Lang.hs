module Lang where

import Data.Map as Map

type Id = Integer

data Expression = Value Id | Apply Expression Expression | Unknown Id deriving (Show)

instance Eq Expression where
    (Value a)   == (Value b)   = a == b
    (Apply a b) == (Apply c d) = (a == c) && (b == d)
    (Unknown a) == (Unknown b) = a == b
    _           == _           = False

-- Unknown < Value < Apply
instance Ord Expression where
    compare (Value a) (Value b) = compare a b
    compare (Apply a1 b1) (Apply a2 b2) =
        let aVal = compare a1 a2
        in if aVal /= EQ then aVal
                         else compare b1 b2
    compare a b = compare (getRanking a) (getRanking b)

getRanking :: Expression -> Integer
getRanking (Unknown _) = 0
getRanking (Value _) = 1
getRanking (Apply _ _) = 2

data Equality = Equal Expression Expression deriving (Show)
type Substitutions = Map Id Expression
type Program = [Equality]

emptyProgram :: Program
emptyProgram = []

unifySubs :: Maybe Substitutions -> Maybe Substitutions -> Maybe Substitutions
unifySubs (Just m1) (Just m2) =
    let intersections = Map.intersectionWith (==) m1 m2
        hasConflict = False `elem` (elems intersections)
    in if hasConflict then Nothing
                      else Just $ m1 `Map.union` m2
unifySubs _ _ = Nothing

unify :: Expression -> Expression -> Maybe Substitutions
unify (Value a) (Value b) =
    if a == b then Just empty else Nothing
unify (Unknown a) ex = Just $ Map.singleton a ex
unify (Apply p1 a1) (Apply p2 a2) =
    let pRes = unify p1 p2
        aRes = unify a1 a2
     in unifySubs pRes aRes
unify _ _ = Nothing

replaceUnknowns :: Substitutions -> Expression -> Expression
replaceUnknowns subs (Apply p a) = 
    (Apply (replaceUnknowns subs p) (replaceUnknowns subs a))
replaceUnknowns subs (Unknown u)
    | Just replacement <- Map.lookup u subs
    = replacement
replaceUnknowns _ ex = ex

reduce :: Equality -> Expression -> Expression
reduce (Equal def res) ex
    | Just subs <- unify def ex
    = replaceUnknowns subs res
reduce _ ex = ex

reduceProg :: Program -> Expression -> Expression
reduceProg prog (Apply p a)
    | (reduceProg prog p) /= p || (reduceProg prog a) /= a
    = reduceProg prog $ (Apply (reduceProg prog  p) (reduceProg prog  a))
reduceProg prog ex =
    let newEx = Prelude.foldl (\ex eq -> reduce eq ex) ex prog
    in if newEx == ex then newEx else reduceProg prog newEx
--NOTE: this bit is super inefficient, but works. Need to find a more efficient way of storing deifinitions.

makeProg :: [Equality] -> Program
makeProg = id

addToProg :: Equality -> Program -> Program
addToProg eq prog = prog ++ [eq]
