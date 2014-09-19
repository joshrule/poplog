module Poplog.Evaluator (eval) where

-- external imports
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logic
import Control.Monad.State.Lazy
import Data.Char
import Data.List (findIndices)
import Data.Maybe

-- internal imports
import Poplog.Types

-- Evaluation
eval :: Clause -> PStateM (Either PError Clause)
eval val@(Rule _ _) = get >>= (\e -> put (val:e)) >> return (Right val)
eval val@(Query q) = runExceptT ((runLogicT (resolve q [] (Just [])) (const . return) (return Nothing)) >>= myShow q)

myShow :: Term -> UEnv -> PErrorM Clause
myShow t Nothing = return (Rule (Structure "result" [t,(Atom "fail")]) (Atom "true"))
myShow t (Just xs) = do
    subs <- mapM toTerm $ reverse xs
    let body = foldl (\acc x -> (Structure "," [x,acc])) (Atom "true") subs
    return $ (Rule (Structure "result" [t,(Atom "true")]) body)

toTerm :: (Term,Term) -> PErrorM Term
toTerm (a@(Variable _ _),b) = return (Structure "sub" [a, b])
toTerm _ = throwError $ PError "broken environment"

-- Resolution
type UEnv = Maybe [(Term, Term)]

resolve :: Term -> [Term] -> UEnv -> LogicT PErrorM UEnv
resolve (Atom "true") _ env = return env
resolve (Atom "fail") _ env = return Nothing
resolve (Structure ";" [o1,o2]) goals env = interleave (resolve o1 goals env) (resolve o2 goals env)
resolve (Structure "," [o1,o2]) goals env = (resolve o1 goals env) >>- (resolve o2 goals)
-- Make a list of rules whose heads unify with t, reform as a disjunction
-- of the antecedents and attempt to resolve it.
resolve t goals env = do
    db <- lift $ lift get
    db' <- lift $ mapM (uniquify env) db -- rename variables in each rule
    liftIO $ putStrLn $ show (t,env)
    liftIO $ putStrLn ("goals = " ++ show goals)
    resolveList (possibleUnifiers t db') goals
  where
    resolveList ((t,e):rest) g = if (duplicates t g e)
                               then resolveList rest g
                               else interleave (resolve t (t:g) e) (resolveList rest g)
    resolveList [] g = mzero
     -- If past and current goals unify, backtrack to avoid infinite loops.
    duplicates term goals env =
      any isJust $ map (\g -> unify g term env) goals
    possibleUnifiers t db = zip [ (dbAntecedents db) !! x | x <- (survivingIndices t db)]
                                [ (unifyEnvs t db)   !! x | x <- (survivingIndices t db)]
    unifyEnvs t db = map (flip (unify t) env) (dbHeads db)-- extract heads?
    dbAntecedents db = map (\(Rule h b) -> b) db
    dbHeads db = map (\(Rule h b) -> h) db
    survivingIndices t db = findIndices isJust $ unifyEnvs t db

-- rename vars across terms, including underscores
uniquify :: UEnv -> Clause -> PErrorM Clause
uniquify (Just []) r = return r
uniquify env (Rule h b) =
    return $ evalState (rename (Structure "," [h,b]) (getMax+1) >>= (\(Structure "," [h,b]) -> return (Rule h b))) (getMax+2)
  where
    getMax = maximum $ map fromJust $ filter isJust $ map varVals $ fromJust env
    varVals ((Variable _ i),_) = Just i
    varVals _         = Nothing
    rename :: Term -> Int -> State Int Term
    rename r@(Real _) n1 = return r
    rename a@(Atom _) n1 = return a
    rename i@(Integer _) n1 = return i
    rename (Variable s i) n1 = do
      n2 <- get
      if s == "_"
      then do put (if n2 < 0 then (n2-1) else (n2+1)) >> return (Variable s n2)
      else return (Variable s n1)
    rename s@(Structure h b) n1 = liftM (Structure h) (mapM (\x -> rename x n1) b)
uniquify _ _ = throwError $ PError "trying to uniquify a query!"

-- Unification based on Russell & Norvig's Unifier (Figure 9.1)
unify :: Term -> Term -> UEnv -> UEnv
unify (Atom a1) (Atom a2) env = if a1 == a2 then env else Nothing
unify (Real r1) (Real r2) env = if r1 == r2 then env else Nothing
unify (Integer n1) (Integer n2) env = if n1 == n2 then env else Nothing

unify v1@(Variable _ _) x env = unifyVar v1 x env
unify x v1@(Variable _ _) env = unifyVar v1 x env
unify (Structure f1 b1) (Structure f2 b2) env =
    foldl (\acc (x1,x2) -> unify x1 x2 acc) (unify (Atom f1) (Atom f2) env) (zip b1 b2)
unify _ _ _ = Nothing

-- No Occur-Check!
unifyVar :: Term -> Term -> UEnv -> UEnv
unifyVar v@(Variable _ _) x jenv@(Just env) =
  maybe (maybe (maybe Nothing
                      (\e -> if v == x then (Just e) else (Just $ (v,x):e))
                      jenv)
               (\val -> unify v val jenv)
               (lookup x env))
        (\val -> unify val x jenv)
        (lookup v env)
unifyVar _ _ _ = Nothing
