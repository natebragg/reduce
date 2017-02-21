module Impcore.Interp (
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

import Data.List (nub)

import Prelude hiding (exp)

type Env a = [(Name, a)]
data Fun = User ([Name], Exp)
         | Prim (Value -> Value -> Value)
type Envs = (Env Value, Env Fun, Env Value)

eval :: [XDef] -> [XDef]
eval xds = snd $ until (null . snd) (uncurry imp) (([], basis, []), xds)
    where basis = [("-", Prim (-)), ("+", Prim (+)), ("*", Prim (*)), ("/", Prim div),
                   ("=", Prim ((fromBool .) . (==))), ("<", Prim ((fromBool .) . (<))),
                   (">", Prim ((fromBool .) . (>)))]
          fromBool False = 0
          fromBool True = 1

replace :: (Eq a) => a -> b -> [(a, b)] -> Maybe [(a, b)]
replace _ _ [] = Nothing
replace x v ((x', _):xvs) | x == x' = Just $ (x, v):xvs
replace x v (xv:xvs) = (Just . (xv:)) =<< replace x v xvs

-- This is a subtle rule change from big-step; env is checked after evaluation.
set_e :: Envs -> Exp -> (Envs, Exp)
set_e envs (Set x e) = (envs', Set x e')
    where (envs', e') = exp envs e

set_xi :: Envs -> Exp -> (Envs, Exp)
set_xi (xi, phi, rho) (Set x (Literal v)) =
            case replace x v xi of
               Nothing -> error "bad set"
               Just xi' -> ((xi', phi, rho), Literal v)

set_rho :: Envs -> Exp -> (Envs, Exp)
set_rho envs@(xi, phi, rho) e@(Set x (Literal v)) =
             case replace x v rho of
                Nothing -> set_xi envs e
                Just rho' -> ((xi, phi, rho'), Literal v)

set :: Envs -> Exp -> (Envs, Exp)
set envs s@(Set _ (Literal _)) = set_rho envs s
set envs s@(Set _ _) = set_e envs s

if_e :: Envs -> Exp -> (Envs, Exp)
if_e envs (If e1 e2 e3) = (envs', If e1' e2 e3)
    where (envs', e1') = exp envs e1

if_true :: Envs -> Exp -> (Envs, Exp)
if_true envs (If (Literal v) e2 _) | v /= 0 = (envs, e2)

if_false :: Envs -> Exp -> (Envs, Exp)
if_false envs (If (Literal v) _ e3) | v == 0 = (envs, e3)

ifx :: Envs -> Exp -> (Envs, Exp)
ifx envs i@(If (Literal v) _ _) | v /= 0 = if_true envs i
ifx envs i@(If (Literal v) _ _) | v == 0 = if_false envs i
ifx envs i@(If _ _ _) = if_e envs i

-- Somewhat roundabout; reminiscent of Plotkin's SMC machine while semantics.
while :: Envs -> Exp -> (Envs, Exp)
while envs (While e1 e2) = (envs, If e1 (Begin [e2, While e1 e2]) (Literal 0))

begin_e :: Envs -> Exp -> (Envs, Exp)
begin_e envs (Begin (e:es)) = (envs', Begin (e':es))
    where (envs', e') = exp envs e

begin_v :: Envs -> Exp -> (Envs, Exp)
begin_v envs (Begin (Literal _:es)) = (envs, Begin es)

begin_1 :: Envs -> Exp -> (Envs, Exp)
begin_1 envs (Begin [e]) = (envs, e)

begin_0 :: Envs -> Exp -> (Envs, Exp)
begin_0 envs (Begin []) = (envs, Literal 0)

begin :: Envs -> Exp -> (Envs, Exp)
begin envs b@(Begin []) = begin_0 envs b
begin envs b@(Begin [_]) = begin_1 envs b
begin envs b@(Begin (Literal _:_)) = begin_v envs b
begin envs b@(Begin _) = begin_e envs b

formals_e :: Envs -> [Exp] -> (Envs, [Exp])
formals_e envs (e:es) = (envs', e':es)
    where (envs', e') = exp envs e

formals_v :: Envs -> [Exp] -> (Envs, [Exp])
formals_v envs (Literal v:es) = (envs', Literal v:es')
    where (envs', es') = formals envs es

formals_0 :: Envs -> [Exp] -> (Envs, [Exp])
formals_0 envs [] = (envs, [])

formals :: Envs -> [Exp] -> (Envs, [Exp])
formals envs fs@[] = formals_0 envs fs
formals envs fs@(Literal _:_) = formals_v envs fs
formals envs fs@(_:_) = formals_e envs fs

apply_e :: Envs -> Exp -> (Envs, Exp)
apply_e envs (Apply x es) = (envs', Apply x es')
    where (envs', es') = formals envs es

-- This is a subtle rule change from big-step; env is checked after evaluation.
apply_v :: Envs -> Exp -> (Envs, Exp)
apply_v envs@(xi, phi, rho) (Apply x ls) =
        let vs = [v | Literal v <- ls]
        in   case lookup x phi of
                Nothing -> error "no fun"
                Just (User (fs, e)) -> do
                    if length fs == length vs
                        then ((xi, phi, zip fs vs), e) --pop cached rho
                        else error "wrong args"
                Just (Prim f) -> do
                    if length vs == 2
                        then (envs, Literal $ f (vs !! 0) (vs !! 1))
                        else error "wrong args"

apply :: Envs -> Exp -> (Envs, Exp)
apply envs a@(Apply _ ls) | length ls == length [v | Literal v <- ls] = apply_v envs a
apply envs a@(Apply _ _) = apply_e envs a

literal :: Envs -> Exp -> (Envs, Exp)
literal _ (Literal v) = error "evaluate literal"

variable :: Envs -> Exp -> (Envs, Exp)
variable envs@(xi, _, rho) (Var x) = (envs, maybe (maybe (error "no var") Literal $ lookup x xi) Literal $ lookup x rho)

exp :: Envs -> Exp -> (Envs, Exp)
exp envs e@(Set _ _) = set envs e
exp envs e@(If _ _ _) = ifx envs e
exp envs e@(While _ _) = while envs e
exp envs e@(Begin _) = begin envs e
exp envs e@(Apply _ _) = apply envs e
exp envs e@(Literal _) = literal envs e
exp envs e@(Var _) = variable envs e

val_e :: Envs -> Def -> (Envs, Def)
val_e envs (Val x e) = (envs', Val x e')
    where (envs', e') = exp envs e

val_v :: Envs -> Def -> (Envs, Def)
val_v (xi, phi, rho) (Val x (Literal v)) = (((x, v):xi, phi, rho), Done)

val :: Envs -> Def -> (Envs, Def)
val envs v@(Val _ (Literal _)) = val_v envs v
val envs v@(Val _ _) = val_e envs v

expdef_e :: Envs -> Def -> (Envs, Def)
expdef_e envs (Exp e) = (envs', Exp e')
    where (envs', e') = exp envs e

expdef_v :: Envs -> Def -> (Envs, Def)
expdef_v (xi, phi, rho) (Exp (Literal v)) =
              case replace "it" v xi of
                 Nothing -> ((("it", v):xi, phi, rho), Done)
                 Just xi' -> ((xi', phi, rho), Done)

expdef :: Envs -> Def -> (Envs, Def)
expdef envs d@(Exp (Literal _)) = expdef_v envs d
expdef envs d@(Exp _) = expdef_e envs d

define :: Envs -> Def -> (Envs, Def)
define (xi, phi, rho) (Define x fs e) | length fs == length (nub fs) = ((xi, (x, User (fs, e)):phi, rho), Done)

def :: Envs -> Def -> (Envs, Def)
def envs d@(Val _ _) = val envs d
def envs d@(Exp _) = expdef envs d
def envs d@(Define _ _ _) = define envs d

defx :: Envs -> XDef -> (Envs, XDef)
defx envs (Def d) = case def envs d of (envs', d') -> (envs', Def d')

checkexpect_ee :: Envs -> UnitTest -> (Envs, UnitTest)
checkexpect_ee envs (CheckExpect e1 e2) = (envs', CheckExpect e1' e2)
    where (envs', e1') = exp envs e1

checkexpect_ve :: Envs -> UnitTest -> (Envs, UnitTest)
checkexpect_ve envs (CheckExpect (Literal v1) e2) = (envs', CheckExpect (Literal v1) e2')
    where (envs', e2') = exp envs e2

checkexpect_vv :: Envs -> UnitTest -> (Envs, UnitTest)
checkexpect_vv envs (CheckExpect (Literal v1) (Literal v2)) | v1 == v2 = (envs, Pass)

checkexpect :: Envs -> UnitTest -> (Envs, UnitTest)
checkexpect envs u@(CheckExpect (Literal _) (Literal _)) = checkexpect_vv envs u
checkexpect envs u@(CheckExpect (Literal _) _) = checkexpect_ve envs u
checkexpect envs u@(CheckExpect _ _) = checkexpect_ee envs u

--for now, no checkerror
checkerror :: Envs -> UnitTest -> (Envs, UnitTest)
checkerror envs (CheckError e) = undefined

unittest :: Envs -> UnitTest -> (Envs, UnitTest)
unittest envs u@(CheckExpect _ _) = checkexpect envs u
unittest envs u@(CheckError _) = checkerror envs u

unittestx :: Envs -> XDef -> (Envs, XDef)
unittestx envs (Test t) = (envs', Test t')
    where (envs', t') = unittest envs t

--for now, no use
use :: Envs -> XDef -> (Envs, XDef)
use envs (Use filename) = undefined

xdef :: Envs -> XDef -> (Envs, XDef)
xdef envs x@(Use _) = use envs x
xdef envs x@(Test _) = unittestx envs x
xdef envs x@(Def _) = defx envs x

imp_sorted :: Envs -> [XDef] -> (Envs, [XDef])
imp_sorted envs (xd:xds) = (envs', if done xd' then xds else xd':xds)
    where (envs', xd') = xdef envs xd
          done (Def Done) = True
          done (Test Pass) = True
          done _ = False

imp :: Envs -> [XDef] -> (Envs, [XDef])
imp envs xds = imp_sorted envs $ filter (not . test) xds ++ filter test xds
    where test (Test t) = True
          test _ = False
