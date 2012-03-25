module Evaluator
  ( Term(Atom, Pair)
  , evaluate )
where


type Name = String

data Term
  = Atom Name
  | Pair Term Term
  | Meta Term (Term -> Term)

instance Show Term where
  show term = case term of
    Atom name      -> name
    Pair oprt oprd -> "(" ++ show oprt ++ " " ++ show oprd ++ ")"
    Meta term _    -> show term

evaluate :: Term -> Term
evaluate term = case term of
  Atom name      -> searchCSig name csig
  Pair oprt oprd -> combine (evaluate oprt) oprd
  _              -> term

combine :: Term -> Term -> Term
combine (Meta _ f) oprd = f oprd

type CSig = [(Name, Term)]

searchCSig :: Name -> CSig -> Term
searchCSig name csig = case lookup name csig of
  Nothing   -> error $ "unknown combinator: " ++ name
  Just term -> term

csig :: CSig
csig =
  [ ("I", Meta (Atom "I") $ \x -> x)
  , ("K", Meta (Atom "K") $ \x -> Meta (Pair (Atom "K") x) $ \y -> x)
  , ("S", Meta (Atom "S") $ \f -> Meta (Pair (Atom "S") f) $ \g -> Meta (Pair (Pair (Atom "S") f) g) $ \a -> evaluate $ Pair (Pair f a) (Pair g a)) ]

