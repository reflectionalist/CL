module CL
  ( Nom, CTm(..), var, cmb
  , fvs, mem
  , CL(..)
  , norm, isNF, red1, redn )
where


import Data.Set (Set, member, empty, singleton, union, delete)


type Nom = String

data CTm c
  = Var Nom
  | Atm Nom
  | Cmb (CTm c) (CTm c)

instance Show (CTm c) where
  show ctm = case ctm of
    Var nom           -> nom
    Atm nom           -> nom
    Cmb opr (Var nom) -> show opr ++ " " ++ nom
    Cmb opr (Atm nom) -> show opr ++ " " ++ nom
    Cmb opr opd       -> show opr ++ " " ++ "(" ++ show opd ++ ")"


var :: Nom -> CTm c
var = Var

cmb :: [CTm c] -> CTm c
cmb = foldl1 Cmb


fvs :: CTm c -> Set Nom
fvs ctm = case ctm of
  Var nom     -> singleton nom
  Atm _       -> empty
  Cmb opr opd -> union (fvs opr) (fvs opd)

mem :: Ord a => a -> Set a -> Bool
mem = member

class CL c where
  disp :: CTm c -> Either (CTm c -> CTm c) (CTm c)
  isnc :: CTm c -> Bool
  redc :: CTm c -> Maybe (CTm c)

norm :: CL c => CTm c -> CTm c
norm ctm = case ctm of
  Cmb opr opd -> case disp (norm opr) of
    Left ctn  -> norm (ctn opd)
    Right nf  -> cmb [nf, norm opd]
  _           -> ctm

isNF :: CL c => CTm c -> Bool
isNF ctm = case ctm of
  Var _   -> True
  Atm _   -> True
  _       -> isnc ctm

red1 :: CL c => CTm c -> CTm c
red1 ctm = case redc ctm of
  Just stp -> stp
  Nothing  -> case ctm of
    Cmb opr opd | isNF opr  -> cmb [opr, red1 opd]
                | otherwise -> cmb [red1 opr, opd]

redn :: CL c => Int -> CTm c -> [CTm c]
redn = red []
  where red stps n ctm
          | isNF ctm  = stps
          | n == 0    = stps
          | otherwise = let stp = red1 ctm
                        in  red (stp : stps)
                                (if n < 0 then n else n - 1)
                                stp

