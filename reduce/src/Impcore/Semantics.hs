{-# LANGUAGE MonadComprehensions #-}

module Impcore.Semantics (
    eval
) where

import Impcore.Ast (
  Name,
  Value,
  XDef(..),
  UnitTest(..),
  Def(..),
  Exp(..),
  )

import Redex (RedexT, runRedexT, reduce, term, (<|>))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad (guard, mzero)
import Control.Monad.Cont (ContT(..), runContT)
import Data.List (nub)

import Prelude hiding (exp)

type Env a = [(Name, a)]
type Fun = ([Name], Exp)

type Impcore m a = RedexT a (MaybeT (StateT (Env Value, Env Fun, Env Value) m)) a

potred :: (Monad m) => [XDef] -> ContT [XDef] (StateT (Env Value, Env Fun, Env Value) m) [XDef]
potred t = ContT $ \hook -> do
    t' <- runMaybeT $ runRedexT imp t
    maybe (return t) ((>>= flip runContT hook . potred) . hook) t'

eval :: (Monad m) => [XDef] -> ([XDef] -> StateT (Env Value, Env Fun, Env Value) m [XDef]) -> m [XDef]
eval t h = evalStateT (runContT (potred t) h) ([], [], [])

replace :: (Eq a) => a -> b -> [(a, b)] -> Maybe [(a, b)]
replace _ _ [] = Nothing
replace x v ((x', _):xvs) | x == x' = Just $ (x, v):xvs
replace x v (xv:xvs) = (Just . (xv:)) =<< replace x v xvs

xi :: (Monad m) => Name -> Impcore m Exp
xi x = do (xi, _, _) <- get
          maybe mzero (return . Literal) $ lookup x xi

rho :: (Monad m) => Name -> Impcore m Exp
rho x = do (_, _, rho) <- get
           maybe mzero (return . Literal) $ lookup x rho

-- This is a subtle rule change from big-step; env is checked after evaluation.
set_e :: (Monad m) => Impcore m Exp
set_e = [Set x e' | Set x e <- term, e' <- reduce exp e]

set_xi :: (Monad m) => Impcore m Exp
set_xi = do Set x (Literal v) <- term
            (xi, phi, rho) <- get
            case replace x v xi of
               Nothing -> mzero
               Just xi' -> do put (xi', phi, rho)
                              return $ Literal v

set_rho :: (Monad m) => Impcore m Exp
set_rho = do Set x (Literal v) <- term
             (xi, phi, rho) <- get
             case replace x v rho of
                Nothing -> mzero
                Just rho' -> do put (xi, phi, rho')
                                return $ Literal v

set :: (Monad m) => Impcore m Exp
set = set_rho <|> set_xi <|> set_e

if_e :: (Monad m) => Impcore m Exp
if_e = [If e1' e2 e3 | If e1 e2 e3 <- term, e1' <- reduce exp e1]

if_true :: (Monad m) => Impcore m Exp
if_true = [e2 | If (Literal v) e2 _ <- term, v /= 0]

if_false :: (Monad m) => Impcore m Exp
if_false = [e3 | If (Literal v) _ e3 <- term, v == 0]

ifx :: (Monad m) => Impcore m Exp
ifx = if_true <|> if_false <|> if_e

-- Somewhat roundabout; reminiscent of Plotkin's SMC machine while semantics.
while :: (Monad m) => Impcore m Exp
while = [If e1 (Begin [e2, While e1 e2]) (Literal 0) | While e1 e2 <- term]

begin_e :: (Monad m) => Impcore m Exp
begin_e = [Begin (e':es) | Begin (e:es) <- term, e' <- reduce exp e]

begin_v :: (Monad m) => Impcore m Exp
begin_v = [Begin es | Begin (Literal _:es) <- term]

begin_1 :: (Monad m) => Impcore m Exp
begin_1 = [e | Begin [e] <- term]

begin_0 :: (Monad m) => Impcore m Exp
begin_0 = [Literal 0 | Begin [] <- term]

begin :: (Monad m) => Impcore m Exp
begin = begin_0 <|> begin_1 <|> begin_v <|> begin_e

formals_e :: (Monad m) => Impcore m [Exp]
formals_e = [e':es | e:es <- term, e' <- reduce exp e]

formals_v :: (Monad m) => Impcore m [Exp]
formals_v = [Literal v:es' | Literal v:es <- term, es' <- reduce formals es]

formals_0 :: (Monad m) => Impcore m [Exp]
formals_0 = [[] | [] <- term]

formals :: (Monad m) => Impcore m [Exp]
formals = formals_0 <|> formals_v <|> formals_e

apply_e :: (Monad m) => Impcore m Exp
apply_e = [Apply x es' | Apply x es <- term, es' <- reduce formals es]

-- This is a subtle rule change from big-step; env is checked after evaluation.
apply_v :: (Monad m) => Impcore m Exp
apply_v = do Apply x ls <-term
             let vs = [v | Literal v <- ls]
             guard $ length ls == length vs
             (xi, phi, rho) <- get
             case lookup x phi of
                Nothing -> mzero
                Just (fs, e) -> do
                    guard $ length fs == length vs
                    let rho' = zip fs vs
                    put (xi, phi, rho')
                    return e
                    --pop cached rho

apply :: (Monad m) => Impcore m Exp
apply = apply_v <|> apply_e

literal :: (Monad m) => Impcore m Exp
literal = do Literal v <- term
             mzero

var_xi :: (Monad m) => Impcore m Exp
var_xi = [v | Var x <- term, v <- xi x]

var_rho :: (Monad m) => Impcore m Exp
var_rho = [v | Var x <- term, v <- rho x]

variable :: (Monad m) => Impcore m Exp
variable = var_rho <|> var_xi

exp :: (Monad m) => Impcore m Exp
exp = set <|> ifx <|> while <|> begin <|> apply <|> literal <|> variable

val_e :: (Monad m) => Impcore m Def
val_e = [Val x e' | Val x e <- term, e' <- reduce exp e]

val_v :: (Monad m) => Impcore m Def
val_v = do Val x (Literal v) <- term
           (xi, phi, rho) <- get
           put ((x, v):xi, phi, rho)
           return Done

val :: (Monad m) => Impcore m Def
val = val_v <|> val_e

expdef_e :: (Monad m) => Impcore m Def
expdef_e = [Exp e' | Exp e <- term, e' <- reduce exp e]

expdef_v :: (Monad m) => Impcore m Def
expdef_v = do Exp (Literal v) <- term
              (xi, phi, rho) <- get
              case replace "it" v xi of
                 Nothing -> put (("it", v):xi, phi, rho)
                 Just xi' -> put (xi', phi, rho)
              return Done

expdef :: (Monad m) => Impcore m Def
expdef = expdef_v <|> expdef_e

define :: (Monad m) => Impcore m Def
define = do Define x fs e <- term
            guard $ length fs == length (nub fs)
            (xi, phi, rho) <- get
            put (xi, (x, (fs, e)):phi, rho)
            return Done

def :: (Monad m) => Impcore m Def
def = val <|> expdef <|> define

defx :: (Monad m) => Impcore m XDef
defx = [Def d' | Def d <- term, d' <- reduce def d]

checkexpect_ee :: (Monad m) => Impcore m UnitTest
checkexpect_ee = [CheckExpect e1' e2 | CheckExpect e1 e2 <- term, e1' <- reduce exp e1]

checkexpect_ve :: (Monad m) => Impcore m UnitTest
checkexpect_ve = [CheckExpect (Literal v1) e2' | CheckExpect (Literal v1) e2 <- term, e2' <- reduce exp e2]

checkexpect_vv :: (Monad m) => Impcore m UnitTest
checkexpect_vv = [Pass | CheckExpect (Literal v1) (Literal v2) <- term, v1 == v2]

checkexpect :: (Monad m) => Impcore m UnitTest
checkexpect = checkexpect_vv <|> checkexpect_ve <|> checkexpect_ee

--for now, no checkerror
checkerror :: (Monad m) => Impcore m UnitTest
checkerror = [undefined | CheckError e <- term]

unittest :: (Monad m) => Impcore m UnitTest
unittest = checkexpect <|> checkerror

unittestx :: (Monad m) => Impcore m XDef
unittestx = [Test t' | Test t <- term, t' <- reduce unittest t]

--for now, no use
use :: (Monad m) => Impcore m XDef
use = [undefined | Use filename <- term]

xdef :: (Monad m) => Impcore m XDef
xdef = use <|> unittestx <|> defx

imp_sorted :: (Monad m) => Impcore m [XDef]
imp_sorted = [if done xd' then xds else xd':xds | xd:xds <- term, xd' <- reduce xdef xd]
    where done (Def Done) = True
          done (Test Pass) = True
          done _ = False

imp :: (Monad m) => Impcore m [XDef]
imp = [xds' | xds <- term, xds' <- reduce imp_sorted $ filter (not . test) xds ++ filter test xds]
    where test (Test t) = True
          test _ = False
