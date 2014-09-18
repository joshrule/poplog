module PrologTypes
       ( Name(..)
       , Term(..)
       , Clause(..)
       , isAtom
       , isStructure
       , isReal
       , isInteger
       , isVariable
       ) where

-- external imports
import Data.List (intersperse)

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
showTerm (Structure f ts) = f ++ "(" ++ showSepList ", " ts ++ ")"

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
                           
showClause (Rule t ts) = show t ++ " :- " ++ show ts ++ "."
showClause (Query t) = show t ++ "."
