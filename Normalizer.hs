module Normalizer
  ( Term(Atom, Comb)
  , normalize )
where


type Name = String

data Term
  = Atom Name
  | Comb Term Term
  | Meta Term (Term -> Term)

instance Show Term where
  show term = case term of
    Atom name      -> name
    Comb optr opnd -> "(" ++ show optr ++ " " ++ show opnd ++ ")"
    Meta term _    -> show term

type REnv = [(Name, Term)]

searchREnv :: REnv -> Name -> Term
searchREnv renv name = case lookup name renv of
  Nothing   -> error $ "unknown combinator: " ++ name
  Just term -> term

normalize :: REnv -> Term -> Term
normalize renv term = case term of
  Atom name      -> searchREnv renv name
  Comb optr opnd -> reduce (normalize renv optr) opnd
  _              -> term

reduce :: Term -> Term -> Term
reduce (Meta _ f) opnd = f opnd

core :: REnv
core =
  [ ("I", Meta (Atom "I") $ \x -> x)
  , ("K", Meta (Atom "K") $ \x -> Meta (Comb (Atom "K") x) $ \y -> x)
  , ("S", Meta (Atom "S") $ \f -> Meta (Comb (Atom "S") f) $ \g -> Meta (Comb (Comb (Atom "S") f) g) $ \a -> normalize core $ Comb (Comb f a) (Comb g a)) ]

