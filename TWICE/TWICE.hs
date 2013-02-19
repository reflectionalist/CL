module TWICE
  ( var, cmb, cT, cW, cI, cC, cE
  , norm, isNF, red1, redn )
where


import CL


data TWICE


cT :: CTm TWICE
cT = Atm "T"

cW :: CTm TWICE
cW = Atm "W"

cI :: CTm TWICE
cI = Atm "I"

cC :: CTm TWICE
cC = Atm "C"

cE :: CTm TWICE
cE = Atm "E"


instance CL TWICE where
  disp ctm = case ctm of
    Cmb (Cmb (Atm "T") r) s -> Left $ \t -> cmb [r, cmb [s, t]]
    Cmb (Cmb (Atm "W") r) s -> Left $ \_ -> cmb [r, s]
    Atm "I"                 -> Left $ \t -> t
    Cmb (Atm "C") s         -> Left $ \t -> cmb [s, t, t]
    Cmb (Cmb (Atm "E") r) s -> Left $ \t -> cmb [r, t, s]
    nf                      -> Right nf

  isnc ctm = case ctm of
    Cmb (Cmb (Atm "T") r) s -> isNF r && isNF s
    Cmb (Atm "T") r         -> isNF r
    Cmb (Atm "C") r         -> isNF r
    Cmb (Cmb (Atm "E") r) s -> isNF r && isNF s
    Cmb (Atm "E") r         -> isNF r
    _                       -> False

  redc ctm = case ctm of
    Cmb (Cmb (Cmb (Atm "T") r) s) t -> Just $ cmb [r, t, cmb [s, t]]
    Cmb (Cmb (Cmb (Atm "K") r) s) _ -> Just $ cmb [r, s]
    Cmb (Atm "I") t                 -> Just $ t
    Cmb (Cmb (Atm "C") s) t         -> Just $ cmb [s, t, t] 
    Cmb (Cmb (Cmb (Atm "E") r) s) t -> Just $ cmb [r, t, s]
    _                               -> Nothing

