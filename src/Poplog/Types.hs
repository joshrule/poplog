module Poplog.Types
       ( Name(..)
       , Term(..)
       , Clause(..)
       , PError(..)
       , PErrorM(..)
       , PStateM(..)
       , isAtom
       , isStructure
       , isReal
       , isInteger
       , isVariable
       ) where

-- external imports
import Data.List (intersperse)
import Control.Monad.Except
import Control.Monad.State.Lazy

-- helper functions

showSepList :: Show x => String -> [x] -> String
showSepList sep xs = concat $ intersperse sep (map show xs)

-- Terms

type Name = String

data Term = Structure Name [Term] -- technically should be "atom", not name
          | Atom Name
          | Real Double
          | Integer Int
          | Variable Name Int
instance Show Term where show = showTerm
instance Eq Term where (==) = compareTerm

showTerm :: Term -> String
showTerm (Atom a) = a
showTerm (Real r) = show r
showTerm (Integer i) = show i
showTerm (Variable v i) = v ++ ":" ++ show i
showTerm (Structure "[]" []) = "[]"
showTerm (Structure "." xs) = "[" ++ (showListTerm xs) ++ "]"
showTerm (Structure "," [a,b]) = show a ++ ", " ++ show b
showTerm (Structure ";" [a,b]) = show a ++ "; " ++ show b
showTerm (Structure f ts) = f ++ "(" ++ showSepList ", " ts ++ ")"

showListTerm :: [Term] -> String
showListTerm [h,(Structure "[]" [])] = show h
showListTerm [h,(Structure "." tail)] = show h ++ ", " ++ showListTerm tail
showListTerm [h,t] = show h ++ " | " ++ show t

compareTerm (Atom a) (Atom b) = a == b
compareTerm (Real r) (Real s) = r == s
compareTerm (Integer i) (Integer j) = i == j
compareTerm (Variable v i) (Variable x j) = v == x && i == j
compareTerm (Structure f ts) (Structure g us) = f == g && ts == us
compareTerm _ _ = False

isAtom (Atom _) = True
isAtom _        = False

isStructure (Structure _ _) = True
isStructure _               = False

isReal (Real _) = True
isReal _        = False

isInteger (Integer _) = True
isInteger _           = False

isVariable (Variable _ _) = True
isVariable _            = False

-- Clauses

data Clause = Rule Term Term | Query Term
instance Show Clause where show = showClause

showClause (Rule t (Atom "true")) = show t ++ "."
showClause (Rule t ts) = show t ++ " :- " ++ show ts ++ "."
showClause (Query t) = show t ++ "."

-- Errors

data PError = PError String
instance Show PError where show = showError
showError :: PError -> String
showError (PError s) = s

-- Monads

type PStateM = StateT [Clause] IO
type PErrorM = ExceptT PError PStateM
