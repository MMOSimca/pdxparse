{-|
Module      : StatementUtils
Description : Small helpers for picking statements out of a script
-}
module StatementUtils (
        extractStmt
    ,   matchLhsText
    ,   sameKey
    ,   matchExactText
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import QQ (pdx)

-- | Try to extract one matching statement
extractStmt :: (a -> Bool) -> [a] -> (Maybe a, [a])
extractStmt p xs = extractStmt' p xs []
    where
        extractStmt' _ [] acc = (Nothing, acc)
        extractStmt' p (x:xs) acc =
            if p x then
                (Just x, acc++xs)
            else
                extractStmt' p xs (acc++[x])

-- | Predicate for matching text on the left hand side. The game reads a key
-- however it happens to be capitalized, and script now and then writes one
-- oddly -- @originaL_tag_to_check@ in the Australian tree -- so a key is
-- matched the same way here. A field this misses is not read at all.
matchLhsText :: Text -> GenericStatement -> Bool
matchLhsText t [pdx| $lhs = %_ |] | sameKey t lhs = True
matchLhsText t [pdx| $lhs < %_ |] | sameKey t lhs = True
matchLhsText t [pdx| $lhs > %_ |] | sameKey t lhs = True
matchLhsText _ _ = False

-- | Whether two script keys are the same one, which is not a matter of how
-- either is capitalized.
sameKey :: Text -> Text -> Bool
sameKey a b = T.toLower a == T.toLower b

-- | Predicate for matching text on boths sides
matchExactText :: Text -> Text -> GenericStatement -> Bool
matchExactText l r [pdx| $lhs = $rhs |] | l == lhs && r == T.toLower rhs = True
matchExactText _ _ _ = False
