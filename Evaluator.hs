module Normalizer
  ( Form(Atom, Comb)
  , normalize )
where


type Name = String

data Form
  = Atom Name
  | Comb Form Form
  | Meta Form (Form -> Form)

instance Show Form where
  show form = case form of
    Atom name      -> name
    Comb optr opnd -> "(" ++ show optr ++ " " ++ show opnd ++ ")"
    Meta form _    -> show form

type REnv = [(Name, Form)]

searchREnv :: Name -> REnv -> Form
searchREnv name renv = case lookup name renv of
  Nothing   -> error $ "unknown combinator: " ++ name
  Just form -> form

normalize :: Form -> REnv -> Form
normalize form renv = case form of
  Atom name      -> searchREnv name renv
  Comb optr opnd -> reduce (normalize optr renv) opnd
  _              -> form

reduce :: Form -> Form -> Form
reduce (Meta _ f) opnd = f opnd

core :: REnv
core =
  [ ("I", Meta (Atom "I") $ \x -> x)
  , ("K", Meta (Atom "K") $ \x -> Meta (Comb (Atom "K") x) $ \y -> x)
  , ("S", Meta (Atom "S") $ \f -> Meta (Comb (Atom "S") f) $ \g -> Meta (Comb (Comb (Atom "S") f) g) $ \a -> normalize (Comb (Comb f a) (Comb g a)) core) ]

