module TWICE
  ( var, cmb, cT, cW, cI, cC, cE
  , norm, isNF, red1, redn )
where


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

cT :: CTm
cT = Atm "T"

cW :: CTm
cW = Atm "W"

cI :: CTm
cI = Atm "I"

cC :: CTm
cC = Atm "C"

cE :: CTm
cE = Atm "E"


norm :: CTm -> CTm
norm ctm = case ctm of
  Cmb opr opd -> case norm opr of
    Cmb (Cmb (Atm "T") r) s -> norm $ cmb [r, cmb [s, opd]]
    Cmb (Cmb (Atm "W") r) s -> norm $ cmb [r, s]
    Atm "I"                 -> norm $ opd
    Cmb (Atm "C") s         -> norm $ cmb [s, opd, opd]
    Cmb (Cmb (Atm "E") r) s -> norm $ cmb [r, opd, s]
    nf                      -> cmb [nf, norm opd]
  _                         -> ctm

isNF :: CTm -> Bool
isNF ctm = case ctm of
  Atm _                   -> True
  Var _                   -> True
  Cmb (Cmb (Atm "T") r) s -> isNF r && isNF s
  Cmb (Atm "T") r         -> isNF r
  Cmb (Atm "C") r         -> isNF r
  Cmb (Cmb (Atm "E") r) s -> isNF r && isNF s
  Cmb (Atm "E") r         -> isNF r
  _                       -> False

red1 :: CTm -> CTm
red1 ctm = case ctm of
  Cmb (Cmb (Cmb (Atm "T") r) s) t -> cmb [r, t, cmb [s, t]]
  Cmb (Cmb (Cmb (Atm "K") r) s) _ -> cmb [r, s]
  Cmb (Atm "I") t                 -> t
  Cmb (Cmb (Atm "C") s) t         -> cmb [s, t, t]
  Cmb (Cmb (Cmb (Atm "E") r) s) t -> cmb [r, t, s]
  Cmb opr opd | isNF opr          -> cmb [opr, red1 opd]
              | otherwise         -> cmb [red1 opr, opd]

redn :: Int -> CTm -> [CTm]
redn = red []
  where red stps n ctm
          | isNF ctm  = stps
          | n == 0    = stps
          | otherwise = let stp = red1 ctm
                         in red (stp : stps)
                                (if n < 0 then n else n - 1)
                                stp

