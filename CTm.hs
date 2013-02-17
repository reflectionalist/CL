module CTm
  ( CTm(..)
  , var, cmb, cS, cK, cI, cR, cU, cY
  , fvs, mem, norm, redn, red1 )
where


import Prelude hiding (abs)
import Data.Set (Set, member, empty, singleton, union, delete)


type Nom = String

data CTm
  = Var Nom
  | Atm Nom
  | Cmb CTm CTm

instance Show CTm where
  show ctm = case ctm of
    Var nom           -> nom
    Atm nom           -> nom
    Cmb opr (Var nom) -> show opr ++ " " ++ nom
    Cmb opr (Atm nom) -> show opr ++ " " ++ nom
    Cmb opr opd       -> show opr ++ " " ++ "(" ++ show opd ++ ")"


var :: Nom -> CTm
var = Var

cmb :: [CTm] -> CTm
cmb = foldl1 Cmb

cS :: CTm
cS = Atm "S"

cK :: CTm
cK = Atm "K"

cI :: CTm
cI = cmb [cS, cK, cK]

cR :: CTm
cR = cmb [cS, cI, cI]

cU :: CTm
cU = cmb [ cS
         , cmb [ cS, cS, cmb [cS, cK, cK] ]
         , cmb [ cS
               , cmb [cS, cS, cmb[cS, cmb[cS, cS, cK], cK]]
               , cmb [cS, cmb [cS, cS, cK], cK]] ]

cY :: CTm
cY = cmb [cR, cU]


fvs :: CTm -> Set Nom
fvs ctm = case ctm of
  Var nom     -> singleton nom
  Atm _       -> empty
  Cmb opr opd -> union (fvs opr) (fvs opd)

mem :: Ord a => a -> Set a -> Bool
mem = member

norm :: CTm -> CTm
norm ctm = case ctm of
  Cmb opr opd -> case norm opr of
    Cmb (Cmb (Atm "S") s) t -> norm $ cmb [s, opd, cmb [t, opd]]
    Cmb (Atm "K")         s -> norm s
    nf                      -> cmb [nf, norm opd]
  _                         -> ctm

isNF :: CTm -> Bool
isNF ctm = case ctm of
  Var _                   -> True
  Atm _                   -> True
  Cmb (Cmb (Atm "S") r) s -> isNF r && isNF s
  Cmb (Atm "S")         r -> isNF r
  Cmb (Atm "K")         s -> isNF s
  _                       -> False

redn :: Int -> CTm -> [CTm]
redn = red []
  where red stps n ctm
          | isNF ctm  = stps
          | n == 0    = stps
          | otherwise = let stp = red1 ctm
                         in red (stp : stps)
                                (if n < 0 then n else n - 1)
                                stp

red1 :: CTm -> CTm
red1 ctm = case ctm of
  Cmb (Cmb (Cmb (Atm "S") r) s) t -> cmb [r, t, cmb [s, t]]
  Cmb (Cmb (Atm "K") s)         _ -> s
  Cmb opr opd | isNF opr          -> cmb [opr, red1 opd]
              | otherwise         -> cmb [red1 opr, opd]

