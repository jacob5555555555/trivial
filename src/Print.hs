module Print where

import Lang
import Parse
import Util


printEx :: NameMap -> Expression -> String
printEx names ex =
    let helper :: NameMap -> Expression -> Bool -> String
        helper (NameMap names _) (Unknown a) _ =
            let (Just str) = lookupB a names
            in str
        helper (NameMap names _) (Value a) _ =
            let (Just str) = lookupB a names
            in str
        --helper names (Apply (Apply a b) c) parens =
        --    pOpen parens ++ helper names a True ++ " " ++ helper names b True ++ " " ++ helper names c True ++ pClose parens
        helper names (Apply a b@(Apply _ _)) parens =
            helper names a True ++ " (" ++ helper names b True ++ ")"
        helper names (Apply a b) parens =
            --pOpen parens ++ helper names a True ++ " " ++ helper names b True ++ pClose parens
            helper names a True ++ " " ++ helper names b True
        pOpen :: Bool -> String
        pOpen True = "("
        pOpen False = ""
        pClose :: Bool -> String
        pClose True = ")"
        pClose False = ""
    in helper names ex False

