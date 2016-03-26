module Util where

import Data.Map as Map

data Association a b = Association (Map a b) (Map b a)

instance (Show a, Show b) => Show (Association a b) where
    show (Association map _) = show map

singleton :: Ord a => Ord b => a -> b -> Association a b
singleton a b = Association (Map.singleton a b) (Map.singleton b a)

insert :: Ord a => Ord b => a -> b -> Association a b -> Association a b
insert a b (Association aTob bToa) = Association (Map.insert a b aTob) (Map.insert b a bToa)

lookupA :: Ord a => Ord b => a -> Association a b -> Maybe b
lookupA a (Association aTob _) = Map.lookup a aTob

lookupB :: Ord a => Ord b => b -> Association a b -> Maybe a
lookupB b (Association _ bToa) = Map.lookup b bToa
