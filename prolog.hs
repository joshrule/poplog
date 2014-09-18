module Main where

-- external imports
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logic
import Control.Monad.State.Lazy
import Data.Char
import Data.List (findIndices)
import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec hiding (State)

-- internal imports
import PrologParser
import PrologTypes

-- Errors

data PError = Default String
            | Parser ParseError
instance Show PError where show = showError
showError :: PError -> String
showError (Default s) = s
showError (Parser p) = show p
-- Monads

type PStateM = StateT [Clause] IO
type PErrorM = ExceptT PError PStateM

-- Evaluation

eval :: Clause -> PErrorM Clause
eval val@(Rule _ _) = lift get >>= (\e -> lift $ put (val:e)) >> return val
eval val@(Query q) = (observeT $ resolve q (Just [])) >>= myShow

myShow :: UEnv -> PErrorM Clause
myShow Nothing = return (Rule (Atom "fail") (Atom "true"))
myShow (Just xs) = do
    subs <- mapM toTerm $ reverse xs
    let body = foldl (\acc x -> (Structure "." [x,acc])) (Atom "[]") subs
    return $ (Rule (Atom "substitutions") body)

toTerm :: (Term,Term) -> PErrorM Term
toTerm (a@(Variable _ _),b) = return (Structure "sub" [a, b])
toTerm _ = throwError $ Default "broken environment"

-- Resolution
type UEnv = Maybe [(Term, Term)]

resolve :: Term -> UEnv -> LogicT PErrorM UEnv
resolve (Atom "true") env = return env -- what is it?
resolve (Atom "fail") env = return Nothing
resolve (Structure ";" [o1,o2]) env = interleave (resolve o1 env) (resolve o2 env)
resolve (Structure "," [o1,o2]) env = (resolve o1 env) >>- (resolve o2)
-- Make a list of rules whose heads unify with t, reform as a disjunction
-- of the antecedents and attempt to resolve it.
resolve t env = do
    db <- lift $ lift get
    resolveList (possibleUnifiers t db)
  where
    resolveList ((t,e):rest) = interleave (resolve t e) (resolveList rest)
    resolveList [] = mzero
    possibleUnifiers t db = zip [ (dbAntecedents db) !! x | x <- (survivingIndices t db)]
                                [ (unifyEnvs t db)   !! x | x <- (survivingIndices t db)]
    unifyEnvs t db = map (flip (topLevelUnifier t) env) (dbHeads db)-- extract heads?
    dbAntecedents db = map (\(Rule h b) -> b) db
    dbHeads db = map (\(Rule h b) -> h) db
    survivingIndices t db = findIndices isJust $ unifyEnvs t db

-- Unification based on Russell & Norvig's Unifier (Figure 9.1)
-- rename vars across terms, including underscores
topLevelUnifier :: Term -> Term -> UEnv -> UEnv
topLevelUnifier t1 t2 env =
    unify (evalState (rename t1 getMax) (getMax+2)) (evalState (rename t2 (getMax+1)) (getMin-1)) env
  where
    getMax = foldl (\acc x -> if x>acc then x else acc) 0 $ map fromJust $ filter isJust $ map varVals $ fromJust env
    getMin = foldl (\acc x -> if x<acc then x else acc) 0 $ map fromJust $ filter isJust $ map varVals $ fromJust env
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
unifyVar v@(Variable _ _) x j@(Just env) =
  maybe (maybe (maybe Nothing
                      (\e -> Just $ (v,x):e)
                      j)
               (\val -> unify v val (Just env))
               (lookup x env))
        (\val -> unify val x (Just env))
        (lookup v env)
unifyVar _ _ _ = Nothing

-- main

trapError :: PErrorM String -> PErrorM String
trapError action = catchError action (return . show)

extractValue :: PErrorM a -> PStateM a
extractValue result = do
   result' <- (runExceptT result)
   case result' of 
     (Right val) -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readInput :: String -> PErrorM Clause
readInput input = case parseProlog input of
  Left err -> throwError $ Parser err
  Right val -> eval val

readPrompt :: String -> PStateM String
readPrompt prompt = liftIO $ flushStr prompt >> liftIO getLine

evalString :: String -> PStateM String
evalString expr = extractValue $ trapError $ liftM  show $ readInput expr

evalAndPrint :: String -> PStateM ()
evalAndPrint expr = do
  str <- evalString expr
  (liftIO $ putStrLn str)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne filename = do
  str <- readFile filename
  let strs = lines str
  evalStateT (mapM evalAndPrint strs) []
  return ()

runRepl :: IO ()
runRepl = evalStateT (until_ (== "quit") (readPrompt ">> ") (evalAndPrint)) []

main :: IO ()
main = do 
    args <- getArgs
    case length args of
      0 -> runRepl
      1 -> runOne $ args !! 0
      otherwise -> putStrLn "Error: pl takes 0 or 1 argument."
