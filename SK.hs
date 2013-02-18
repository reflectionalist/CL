module SK
  ( CTm(..)
  , var, cmb, cS, cK, cI, cR, cU, cY
  , fvs, mem, norm, isNF, red1, redn )
where


import CL


data SK


cS :: CTm SK
cS = Atm "S"

cK :: CTm SK
cK = Atm "K"

cI :: CTm SK
cI = cmb [cS, cK, cK]

cR :: CTm SK
cR = cmb [cS, cI, cI]

cU :: CTm SK
cU = cmb [ cS
         , cmb [cS, cmb [cK, cS], cK]
         , cmb [cK, cmb [cS, cI, cI]] ]

cY :: CTm SK
cY = cmb [cR, cU]


instance CL SK where
  disp ctm = case ctm of
    Cmb (Cmb (Atm "S") r) s -> Left $ \t -> cmb [r, t, cmb [s, t]]
    Cmb (Atm "K") s         -> Left $ \t -> s 
    nf                      -> Right nf

  isnc ctm = case ctm of
    Cmb (Cmb (Atm "S") r) s -> isNF r && isNF s
    Cmb (Atm "S") r         -> isNF r
    Cmb (Atm "K") s         -> isNF s
    _                       -> False

  redc ctm = case ctm of
    Cmb (Cmb (Cmb (Atm "S") r) s) t -> Just $ cmb [r, t, cmb [s, t]]
    Cmb (Cmb (Atm "K") s) _         -> Just $ s
    _                               -> Nothing

