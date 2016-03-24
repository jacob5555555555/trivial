module Lang where

import Data.Map as Map

type Id = Integer

data Expression = Value Id | Apply Expression Expression | Unknown Id deriving (Show)

instance Eq Expression where
    (Value a)   == (Value b)   = a == b
    (Apply a b) == (Apply c d) = (a == c) && (b == d)
    (Unknown a) == (Unknown b) = a == b
    _           == _           = False

data Equality = Equal Expression Expression
type Substitution = (Id, Expression)
type Substitutions = Map Id Expression

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

replaceUnknown :: Substitution -> Expression -> Expression
replaceUnknown u (Apply p a) =
    (Apply (replaceUnknown u p) (replaceUnknown u a))
replaceUnknown (a, res) (Unknown b)
    | a == b
    = res
replaceUnknown _ ex = ex

reduce :: Equality -> Expression -> Expression
reduce (Equal a res) b
    | a == b
    = res
reduce (Equal (Unknown a1) res) ex =
    replaceUnknown (a1, ex) res
reduce (Equal (Apply p1 a1) res) (Apply p2 a2)
    | p1 == p2
    = reduce newEquality a2 where
        newEquality = Equal a1 res
reduce eq (Apply a b) =
    (Apply (reduce eq a) (reduce eq b))
reduce _ ex = ex
