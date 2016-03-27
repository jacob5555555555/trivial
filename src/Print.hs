module Print where

import Lang
import Parse
import Util

printEx :: NameMap -> Expression -> String
printEx (NameMap names _) (Unknown a) =
    let (Just str) = lookupB a names
    in str
printEx (NameMap names _) (Value a) =
    let (Just str) = lookupB a names
    in str
printEx names (Apply a b) =
    "(" ++ printEx names a ++ " " ++ printEx names b ++ ")"

