module Evaluator
  ( Term(Atom, Comb)
  , evaluate )
where


type Name = String

data Term
  = Atom Name
  | Comb Term Term
  | Meta Term (Term -> Term)

instance Show Term where
  show term = case term of
    Atom name      -> name
    Comb oprt oprd -> "(" ++ show oprt ++ " " ++ show oprd ++ ")"
    Meta term _    -> show term

type REnv = [(Name, Term)]

searchREnv :: Name -> REnv -> Term
searchREnv name renv = case lookup name renv of
  Nothing   -> error $ "unknown combinator: " ++ name
  Just term -> term

evaluate :: Term -> REnv -> Term
evaluate term renv = case term of
  Atom name      -> searchREnv name renv
  Comb oprt oprd -> combine (evaluate oprt renv) oprd
  _              -> term

combine :: Term -> Term -> Term
combine (Meta _ f) oprd = f oprd

core :: REnv
core =
  [ ("I", Meta (Atom "I") $ \x -> x)
  , ("K", Meta (Atom "K") $ \x -> Meta (Comb (Atom "K") x) $ \y -> x)
  , ("S", Meta (Atom "S") $ \f -> Meta (Comb (Atom "S") f) $ \g -> Meta (Comb (Comb (Atom "S") f) g) $ \a -> evaluate (Comb (Comb f a) (Comb g a)) core) ]

