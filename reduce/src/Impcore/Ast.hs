module Impcore.Ast (
    Name,
    Value,
    XDef(..),
    UnitTest(..),
    Def(..),
    Exp(..),
) where

import Data.List (intercalate)

type Name = String

type Value = Integer

data XDef = Def Def
          | Use Name
          | Test UnitTest

data UnitTest = CheckExpect Exp Exp
              | CheckError Exp
              | Pass -- A sentinel for evaluation

data Def = Val Name Exp
         | Exp Exp
         | Define Name [Name] Exp
         | Done -- A sentinel for evaluation

data Exp = Literal Value
         | Var Name
         | Set Name Exp
         | If Exp Exp Exp
         | While Exp Exp
         | Begin [Exp]
         | Apply Name [Exp]

spaceout = intercalate " "

instance Show XDef where
    show (Def d) = show d
    show (Use x) = "(use " ++ x ++ ")"
    show (Test u) = show u
    showList xds = (unlines (map show xds) ++)

instance Show UnitTest where
    show (CheckExpect c e) = "(check-expect " ++ show c ++ " " ++ show e ++ ")"
    show (CheckError e) = "(check-error " ++ show e ++ ")"

instance Show Def where
    show (Val x e) = "(val " ++ x ++ " " ++ show e ++ ")"
    show (Exp e) = show e
    show (Define x fs e) = "(define " ++ x ++ " (" ++ spaceout fs  ++ ") " ++ show e ++ ")"

instance Show Exp where
    show (Literal n) = show n
    show (Var x) = x
    show (Set x e) = "(set " ++ x ++ " " ++ show e ++ ")"
    show (If e1 e2 e3) = "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
    show (While e1 e2) = "(while " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Begin es) = "(" ++ spaceout ("begin" : map show es) ++ ")"
    show (Apply f es) = "(" ++ spaceout (f : map show es) ++ ")"
