module Lambda where

import Expr
import Data.List

-- TODO 1.1. find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x e) = filter (/= x) (free_vars e)
free_vars (Application e1 e2) = nub ((free_vars e1) ++ (free_vars e2))


-- TODO 1.2. reduce a redex 
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable e1) x e2 
    | e1 == x = e2
    | otherwise = Variable e1
reduce (Function x' e1) x e2
    | x' == x = Function x e1
    | x' `notElem` free_vars e2 = Function x' (reduce e1 x e2)
    | otherwise = Function new_name (reduce (reduce e1 x' (Variable new_name)) x e2)
        where
            new_name = find_new_name [x' ++ show n | n <- [1..100]]
            find_new_name :: [String] -> String
            find_new_name new_name = if (head new_name) `notElem` (free_vars e2) then (head new_name)
                                        else find_new_name (tail new_name)
reduce (Application e1 e2') x e2 = Application (reduce e1 x e2) (reduce e2' x e2)


-- Normal Evaluation
-- TODO 1.3. perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN x = case x of
    (Variable x) -> Variable x
    (Function x e) -> Function x (stepN e)
    (Application (Function x e1) e2) -> reduce e1 x e2
    (Application e1 e2) -> case e1 of
                              (Variable e1') -> Application e1 (stepN e2)
                              e1' -> Application (stepN e1') e2 


-- TODO 1.4. perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN x = if stepN x == x then x else reduceN (stepN x)
      

reduceAllN :: Expr -> [Expr]
reduceAllN x = if stepN x == x then [x] else x : reduceAllN (stepN x)

-- Applicative Evaluation
-- TODO 1.5. perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA x = case x of
  (Variable x) -> Variable x
  (Function x e) -> Function x (stepA e)
  (Application (Function x e1) e2) -> case e2 of
                                        (Application e1' e2') -> Application (Function x e1) (stepA (Application e1' e2'))
                                        _ -> reduce e1 x e2
  (Application e1 e2) -> case e1 of
                            (Variable e1') -> Application e1 (stepA e2)
                            e1' -> Application (stepA e1') e2


-- TODO 1.6. perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA x = if stepA x == x then x else reduceA (stepA x)


reduceAllA :: Expr -> [Expr]
reduceAllA x = if stepA x == x then [x] else x : reduceAllA (stepA x) 
                                                

-- TODO 3.1. make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros dictionary (Variable entity) = case lookup entity dictionary of
                        Just e -> e
                        _ -> Variable entity
evalMacros dictionary (Function x entity) = Function x (evalMacros dictionary entity)
evalMacros dictionary (Application e1 e2) = Application (evalMacros dictionary e1) (evalMacros dictionary e2)
evalMacros dictionary (Macro m) = case lookup m dictionary of
                        Just e -> evalMacros dictionary e
                        _ -> Macro m


-- TODO 4.1. evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode _ [] = []
evalCode reduce_type list_codes = case head list_codes of
                                Evaluate expr -> reduce_type expr : evalCode reduce_type (tail list_codes)
                                Assign macro expr -> evalCode (auxReduce reduce_type [(macro, expr)]) (tail list_codes) 

auxReduce :: (Expr -> Expr) -> [(String, Expr)] -> Expr -> Expr
auxReduce reduce_type list_macro = \expr -> reduce_type (evalMacros list_macro expr)


