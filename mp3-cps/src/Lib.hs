--- ChatGPT was used for debugging purpose
--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n - 1) (\v -> k (n * v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] ke ko
    | even x    = ke x
    | otherwise = ko x
evenoddk (x:xs) ke ko
    | even x    = evenoddk xs (\v -> ke (x + v)) ko
    | otherwise = evenoddk xs ke (\v -> ko (x + v))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (OpExp _ e1 e2) = isSimple e1 && isSimple e2
isSimple (IfExp c t e) = isSimple c && isSimple t && isSimple e
isSimple _ = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k n
    | isSimple e = (AppExp (AppExp f e) k, n)
    | otherwise =
        let (v, n1) = gensym n
            cont = LamExp v (AppExp (AppExp f (VarExp v)) k)
        in cpsExp e cont n1

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n
    | isSimple e1 && isSimple e2 =
        (AppExp k (OpExp op e1 e2), n)

    | not (isSimple e1) && isSimple e2 =
        let (v, n1) = gensym n
            cont = LamExp v (AppExp k (OpExp op (VarExp v) e2))
        in cpsExp e1 cont n1

    | isSimple e1 && not (isSimple e2) =
        let (v, n1) = gensym n
            cont = LamExp v (AppExp k (OpExp op e1 (VarExp v)))
        in cpsExp e2 cont n1

    | otherwise =
        let (v1, n1) = gensym n
            (v2, n2) = gensym n1
            inner = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
            (e2', n3) = cpsExp e2 inner n2
            cont = LamExp v1 e2'
        in cpsExp e1 cont n3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp c t e) k n
    | isSimple c =
        let (t', n1) = cpsExp t k n
            (e', n2) = cpsExp e k n1
        in (IfExp c t' e', n2)

    | otherwise =
        let (v, n1) = gensym n
            (t', n2) = cpsExp t k n1
            (e', n3) = cpsExp e k n2
            cont = LamExp v (IfExp (VarExp v) t' e')
        in cpsExp c cont n3

cpsExp (LamExp x body) k n =
    (AppExp k (LamExp x body), n)

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params body) =
    let k = VarExp "k"
        (body', _) = cpsExp body k 1
    in Decl f (params ++ ["k"]) body'
