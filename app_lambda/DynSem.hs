{-# LANGUAGE MonadComprehensions #-}

module DynSem (
    eval
) where

import Ast (Name, Term(..))
import Redex (RedexT, runRedexT, reduce, term, (<|>))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Cont (ContT(..), runContT)

type FreshName = Int

type Lambda m = RedexT Term (MaybeT (StateT FreshName m))

potred :: (Monad m) => Term -> ContT Term (StateT FreshName m) Term
potred t = ContT $ \hook -> do
    t' <- runMaybeT $ runRedexT redex t
    maybe (return t) ((>>= flip runContT hook . potred) . hook) t'

eval :: (Monad m) => Term -> (Term -> StateT FreshName m Term) -> m Term
eval t h = evalStateT (runContT (potred t) h) 0

freein :: Name -> Term -> Bool
x `freein` X y = x == y
x `freein` App t_1 t_2 = x `freein` t_1 || x `freein` t_2
x `freein` Lam y t = x /= y && x `freein` t

fresh :: (Monad m) => Lambda m Name
fresh = do var <- get
           put (var + 1)
           return $ "a" ++ show var

subst :: (Monad m) => Term -> Lambda m Term -> Lambda m Term
subst = flip reduce

infix 8 `subst`

cong0 :: (Monad m) => Lambda m Term
cong0 = -- [t_1 t_2' | t_1 t_2 <- t, t_2' <- reduce t_2]
        [App t_1 t_2' | App t_1 t_2 <- term, t_2' <- reduce redex t_2]

cong1 :: (Monad m) => Lambda m Term
cong1 = -- [t_1' t_2 | t_1 t_2 <- t, t_1' <- reduce t_1]
        [App t_1' t_2 | App t_1 t_2 <- term, t_1' <- reduce redex t_1]

beta :: (Monad m) => Lambda m Term
beta = -- [ v[x|->t'] | (\x.v) t' <- t]
        [ t'' | App (Lam x v) t' <- term, t'' <- v `subst` x|->t']

cong2 :: (Monad m) => Lambda m Term
cong2 = -- [ \x.t'' | \x.t' <- t, t'' <- reduce t']
        [ Lam x t'' | Lam x t' <- term, t'' <- reduce redex t']

eta :: (Monad m) => Lambda m Term
eta = -- [ t' | \x_1.t' x_2 <- t, x_1 = x_2, not x_1 free in t']
      [ t' | Lam x_1 (App t' (X x_2)) <- term, x_1 == x_2, not $ x_1 `freein` t']

alpha :: (Monad m) => Lambda m Term
alpha = -- [ \x_2.t'[x_1|->x_2] | \x_1.t' <- t, x_2 <- fresh]
        [ Lam x_2 t'' | Lam x_1 t' <- term, x_2 <- fresh, t'' <- (t' `subst` x_1|->X x_2)]

(|->) :: (Monad m) => Name -> Term -> Lambda m Term
x_s |-> t_s =
  -- [ t_s | x <- t, x = x_s ] <|>
  [ t_s | X x <- term, x == x_s ] <|>
  -- [ x | x <- t, x /= x_s ] <|>
  [ X x | X x <- term, x /= x_s ] <|>
  -- [ t_1[x_s|->t_s] t_2[x_s|->t_s] | t_1 t_2 <- t ] <|>
  [ App t_1' t_2' | App t_1 t_2 <- term, t_1' <- (t_1 `subst` x_s|->t_s), t_2' <- (t_2 `subst` x_s|->t_s) ] <|>
  -- [ \x.t' | \x.t' <- t, x = x_s] <|>
  [ Lam x t' | Lam x t' <- term, x == x_s] <|>
  -- [ \x.t'[x_s|->t_s] | \x.t' <- t, x /= x_s, not x free in t_s] <|>
  [ Lam x t'' | Lam x t' <- term, x /= x_s, not $ x `freein` t_s, t'' <- (t' `subst` x_s|->t_s)] <|>
  -- [ \x'.t''[x_s|->t_s] | \x.t' <- t, x /= x_s, x free in t_s, \x'.t'' <- alpha \x.t']
  [ Lam x' t''' | Lam x t' <- term, x /= x_s, x `freein` t_s, Lam x' t'' <- reduce alpha $ Lam x t', t''' <- (t'' `subst` x_s|->t_s)]

redex :: (Monad m) => Lambda m Term
redex = eta <|> cong2 <|> beta <|> cong1 <|> cong0
