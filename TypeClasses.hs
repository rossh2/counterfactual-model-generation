module TypeClasses where

-- Unlike Show, which should return a Haskell-compatible string,  this is designed to take a lexicon item,
-- proposition or syntactic tree and return the linearized constituent/sentence.
-- It can also be used for pretty-printing any items that contain trees or propositions
class ShowLinear a where
    showLin :: a -> String
    showLinList :: [a] -> String

    showLinList [] = "[]"
    showLinList (x:xs) = "[" ++ showLin x ++ showl xs
        where showl []     = "]"
              showl (x:xs) = ", " ++ showLin x ++ showl xs

instance (ShowLinear a, ShowLinear b) => ShowLinear (a, b) where
    showLin (x, y) = "(" ++ showLin x ++ ", " ++ showLin y ++ ")"

instance (ShowLinear a) => ShowLinear (Maybe a) where
    showLin (Just a) = showLin a
    showLin Nothing = "Nothing"