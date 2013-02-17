module TWICE
  (var, cmb, cT, cW, cI, cC, cE)


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
  Cmb (Cmb (Cmb (Atm "T") r) s) t -> norm $ cmb [r, cmb [s, t]]
  Cmb (Cmb (Cmb (Atm "W") r) s) t -> norm $ cmb [r, s]
  Cmb (Atm "I") t                 -> norm $ t
  Cmb (Cmb (Atm "C") r) s         -> norm $ cmb [r, s, s]
  Cmb (Cmb (Cmb (Atm "E") r) s) t -> norm $ cmb [r, t, s] 
  Cmb opr opd                     -> norm $ cmb [norm opr, norm opd]
  _                               -> ctm

