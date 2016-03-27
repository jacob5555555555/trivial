module Util where

import Data.Map as Map

data Association a b = Association (Map a b) (Map b a)

instance (Show a, Show b) => Show (Association a b) where
    show (Association map _) = show map

empty :: Association a b
empty = Association (Map.empty) (Map.empty)

singleton :: Ord a => Ord b => a -> b -> Association a b
singleton a b = Association (Map.singleton a b) (Map.singleton b a)

insert :: Ord a => Ord b => a -> b -> Association a b -> Association a b
insert a b (Association aTob bToa) = Association (Map.insert a b aTob) (Map.insert b a bToa)

lookupA :: Ord a => Ord b => a -> Association a b -> Maybe b
lookupA a (Association aTob _) = Map.lookup a aTob

lookupB :: Ord a => Ord b => b -> Association a b -> Maybe a
lookupB b (Association _ bToa) = Map.lookup b bToa

union :: Ord a => Ord b => Association a b -> Association a b -> Association a b
union (Association a1 b1) (Association a2 b2) = (Association (Map.union a1 a2) (Map.union b1 b2))
