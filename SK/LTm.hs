module LTm
  ( LTm(..)
  , var, abs, app
  , fvs, mem )
where


import Prelude hiding (abs)
import Data.Set (Set, member, singleton, union, delete)


type Nom = String

data LTm
  = Var Nom
  | Abs Nom LTm
  | App LTm LTm
  deriving (Show)

var :: Nom -> LTm
var = Var

abs :: [Nom] -> LTm -> LTm
abs = flip (foldr Abs)

app :: [LTm] -> LTm
app = foldl1 App


fvs :: LTm -> Set Nom
fvs ltm = case ltm of
  Var nom     -> singleton nom
  Abs nom bod -> delete nom (fvs bod)
  App opr opd -> union (fvs opr) (fvs opd)

mem :: Ord a => a -> Set a -> Bool
mem = member

