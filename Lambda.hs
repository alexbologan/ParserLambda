module Lambda where

import Data.List (nub, (\\))
import Data.Char (chr, ord)
import Data.List (sort)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars = 
  let
    vars' (Var x) = [x]
    vars' (App e1 e2) = vars' e1 ++ vars' e2
    vars' (Abs x e) = x : vars' e
  in
    nub . vars'

-- 1.2.
freeVars :: Lambda -> [String]
freeVars = 
  let
    freeVars' (Var x) = [x]
    freeVars' (App e1 e2) = freeVars' e1 ++ freeVars' e2
    freeVars' (Abs x e) = filter (/= x) $ freeVars' e
  in
    nub . freeVars'

-- 1.3.
newVar :: [String] -> String
newVar existingVars = findMissing sortedVars "a"
  where
    sortedVars = sort existingVars

    findMissing :: [String] -> String -> String
    findMissing list current
      | current `notElem` list = current
      | otherwise = findMissing list $ next current

    next :: String -> String
    next str
      | all (=='z') str = replicate (length str + 1) 'a'
      | otherwise = reverse $ increment $ reverse str
      where
        increment [] = ['a']
        increment (x:xs)
          | x == 'z' = 'a' : increment xs
          | otherwise = chr (ord x + 1) : xs

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Abs _ e) = isNormalForm e

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var v) s
  | x == v = s
  | otherwise = Var v
reduce x (App e1 e2) s = App (reduce x e1 s) (reduce x e2 s)
reduce x (Abs v l) s
    | v == x = Abs v l
    | v `elem` freeVars s =
        let z = newVar (vars l ++ vars s)
        in Abs z (reduce x (reduce v l (Var z)) s)
    | otherwise = Abs v (reduce x l s)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (Abs x e) = Abs x (normalStep e)
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
    | isNormalForm e1 = App e1 (normalStep e2)
    | otherwise = App (normalStep e1) e2
normalStep e = e

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (Var x) = Var x
applicativeStep (App (Abs x e1) e2)
    | isNormalForm e2 = reduce x e1 e2
    | otherwise = App (Abs x e1) (applicativeStep e2)
applicativeStep (App e1 e2)
    | isNormalForm e1 = App e1 (applicativeStep e2)
    | otherwise = App (applicativeStep e1) e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)


-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify f e = e :
  if e == f e
    then []
    else simplify f(f e)

normal :: Lambda ->  [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
