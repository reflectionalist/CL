module Trl
  (ltoc, ctol)
where


import Prelude hiding (abs)
import LTm as L
import CTm as C
import Data.Set (Set, member, empty, singleton, insert, delete, union)


ltoc :: LTm -> CTm
ltoc ltm = case ltm of
  L.Var nom   -> C.var nom
  L.Abs nom bod -> atoc nom (ltoc bod)
  L.App opr opd -> cmb [ltoc opr, ltoc opd]
  where atoc nom bod
          | C.mem nom (C.fvs bod) = case bod of
              C.Var nam | nam == nom -> cI
              Cmb opr (C.Var nam)
                | nam == nom && not (C.mem nom $ C.fvs opr) -> opr
              Cmb opr opd            -> cmb [cS, atoc nom opr, atoc nom opd]
          | otherwise             = cmb [cK, bod]

ctol :: CTm -> LTm
ctol ctm = case ctm of
  C.Var nom     -> L.Var nom
  C.Atm "S"     -> L.abs ["g", "f", "x"]
                       $ L.app [L.var "g", L.var "x", L.app [L.var "f", L.var "x"]]
  C.Atm "K"     -> L.abs ["x", "y"] (L.var "x")
  C.Cmb opr opd -> L.app [ctol opr, ctol opd]

