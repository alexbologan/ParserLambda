module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
-- Caută un macro în context și returnează valoarea acestuia dacă există
findMacro :: String -> Context -> Either String Lambda
findMacro name ctx = 
    case lookup name ctx of
        Just expr -> Right expr
        Nothing -> Left "Error"

-- Înlocuiește macro-urile dintr-o expresie cu valorile lor din context
replaceMacros :: Context -> Lambda -> Either String Lambda
replaceMacros ctx (Var x) = Right (Var x)
replaceMacros ctx (Abs x e) = do
    e' <- replaceMacros ctx e
    return (Abs x e')
replaceMacros ctx (App e1 e2) = do
    e1' <- replaceMacros ctx e1
    e2' <- replaceMacros ctx e2
    return (App e1' e2')
replaceMacros ctx (Macro name) = findMacro name ctx

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
    replacedExpr <- replaceMacros ctx expr
    return (simplify step replacedExpr)

-- Funcții de evaluare folosind strategii diferite
normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
