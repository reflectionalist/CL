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

type CSig = [(Name, Term)]

searchREnv :: Name -> CSig -> Term
searchREnv name csig = case lookup name csig of
  Nothing   -> error $ "unknown combinator: " ++ name
  Just term -> term

evaluate :: Term -> CSig -> Term
evaluate term csig = case term of
  Atom name      -> searchREnv name csig
  Comb oprt oprd -> combine (evaluate oprt csig) oprd
  _              -> term

combine :: Term -> Term -> Term
combine (Meta _ f) oprd = f oprd

csig :: CSig
csig =
  [ ("I", Meta (Atom "I") $ \x -> x)
  , ("K", Meta (Atom "K") $ \x -> Meta (Comb (Atom "K") x) $ \y -> x)
  , ("S", Meta (Atom "S") $ \f -> Meta (Comb (Atom "S") f) $ \g -> Meta (Comb (Comb (Atom "S") f) g) $ \a -> evaluate (Comb (Comb f a) (Comb g a)) csig) ]

