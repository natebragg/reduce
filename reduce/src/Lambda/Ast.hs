module Lambda.Ast (
    Name,
    Term(..),
) where

type Name = String

data Term = X Name
          | App Term Term
          | Lam Name Term
    deriving (Eq, Ord)

instance Show Term where
    show = showh False
        where showpar b s = if b then "(" ++ s ++ ")" else s
              showh _ (X n)      = n
              showh b (Lam n t)  = showpar b $ "\\" ++ n ++ "." ++ show t
              showh b (App t t') = showh True t ++ " " ++
                                   (case t' of App _ _ -> showpar True $ showh False t'
                                               otherwise -> showh b t')
