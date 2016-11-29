{-# LANGUAGE MonadComprehensions #-}

module DynSem (
    redex
) where

import Ast (Name, Term(..))
import Redex (Redex(..), (<|>))

import Data.Maybe (fromJust)

freein :: Name -> Term -> Bool
x `freein` X y = x == y
x `freein` App t_1 t_2 = x `freein` t_1 || x `freein` t_2
x `freein` Lam y t = x /= y && x `freein` t

fresh :: Maybe Name
fresh = Just "a"

subst :: Term -> Redex Term -> Term
subst t r = fromJust $ unwrap r $ Just t

infix 8 `subst`

beta0 :: Redex Term
beta0 = -- [t_1 t_2' | t_1 t_2 <- t, t_2' <- reduce t_2]
        Redex $ \t ->
        [App t_1 t_2' | App t_1 t_2 <- t, t_2' <- unwrap redex $ Just t_2]

beta1 :: Redex Term
beta1 = -- [t_1' t_2 | t_1 t_2 <- t, t_1' <- reduce t_1]
        Redex $ \t ->
        [App t_1' t_2 | App t_1 t_2 <- t, t_1' <- unwrap redex $ Just t_1]

beta2 :: Redex Term
beta2 = -- [ v[x|->t'] | (\x.v) t' <- t]
        Redex $ \t ->
        [ v `subst` x|->t' | App (Lam x v) t' <- t]

beta3 :: Redex Term
beta3 = -- [ \x.t'' | \x.t' <- t, t'' <- reduce t']
        Redex $ \t ->
        [ Lam x t'' | Lam x t' <- t, t'' <- unwrap redex $ Just t']

beta :: Redex Term
beta = beta3 <|> beta2 <|> beta1 <|> beta0

eta :: Redex Term
eta = -- [ t' | \x_1.t' x_2 <- t, x_1 = x_2, not x_1 free in t']
        Redex $ \t ->
      [ t' | Lam x_1 (App t' (X x_2)) <- t, x_1 == x_2, not $ x_1 `freein` t']

alpha :: Redex Term
alpha = -- [ \x_2.t'[x_1|->x_2] | \x_1.t' <- t, x_2 <- fresh]
        Redex $ \t ->
        [ Lam x_2 (t' `subst` x_1|->X x_2) | Lam x_1 t' <- t, x_2 <- fresh]

(|->) :: Name -> Term -> Redex Term
x_s |-> t_s =
        Redex $ \t ->
  -- [ t_s | x <- t, x = x_s ] <|>
  [ t_s | X x <- t, x == x_s ] <|>
  -- [ x | x <- t, x /= x_s ] <|>
  [ X x | X x <- t, x /= x_s ] <|>
  -- [ t_1[x_s|->t_s] t_2[x_s|->t_s] | t_1 t_2 <- t ] <|>
  [ App (t_1 `subst` x_s|->t_s) (t_2 `subst` x_s|->t_s) | App t_1 t_2 <- t ] <|>
  -- [ \x.t' | \x.t' <- t, x = x_s] <|>
  [ Lam x t' | Lam x t' <- t, x == x_s] <|>
  -- [ \x.t'[x_s|->t_s] | \x.t' <- t, x /= x_s, not x free in t_s] <|>
  [ Lam x (t' `subst` x_s|->t_s) | Lam x t' <- t, x /= x_s, not $ x `freein` t_s] <|>
  -- [ \x'.t''[x_s|->t_s] | \x.t' <- t, x /= x_s, x free in t_s, \x'.t'' <- alpha \x.t']
  [ Lam x' (t'' `subst` x_s|->t_s) | Lam x t' <- t, x /= x_s, x `freein` t_s, Lam x' t'' <- unwrap alpha $ Just $ Lam x t']

redex :: Redex Term
redex = eta <|> beta
