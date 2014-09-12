module Main where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Data.List (intersperse, delete)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- Terms

{- http://fsl.cs.illinois.edu/images/9/9c/PrologStandard.pdf

Strings:
    - 'char' % as atoms
    - \[{[char],}+\] % a list of single character atoms
    - "[char]+" % a list of character codes

Lists
    - [] is the empty list
    - | can be placed once anywhere in a list to separate head from tail
    - curly brackets for definite clause grammars (to add later if needed)

Parentheses:
    - allowed anywhere, they override grouping.
    - Operators in parentheses do not operate (e.g. 2(+)2 is an error).

Conjunction: ,

Disjunction: ;

Success: true

Failure: fail

Cuts: !

If-Then-Else: (A -> B ; C), where "; C" is optional

Arithmetic:
    - X is Y, where Y is an arithmetic expression
    - op(X,Y) where X and Y may arithmetic expressions and op \in =:=, =\=, <, >,
      =<, >=

Unification:
    - =, unify
    - unify_with_occurs_check, unify with occurs check
    - \=, cannot unify
-}

type Name = String

data Term = Structure Name [Term] -- technically should be "atom", not name
          | Atom Name
          | Real Double
          | Integer Int
          | Variable Name
instance Show Term where show = showTerm

showTerm :: Term -> String
showTerm (Atom a) = a
showTerm (Real r) = show r
showTerm (Integer i) = show i
showTerm (Variable v) = v
showTerm (Structure f ts) = f ++ "(" ++ showSepList ", " ts ++ ")"

-- Clauses

data Clause = Fact Term
            | Rule Term [Term]
            | Query Term
instance Show Clause where show = showClause
                           
showClause (Fact t) = show t ++ "."
showClause (Rule t ts) = show t ++ " :- " ++ showSepList ", " ts ++ "."
showClause (Query t) = show t ++ "."

showSepList :: Show x => String -> [x] -> String
showSepList sep xs = concat $ intersperse sep (map show xs)

-- Parser Helpers

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

spaces :: Parser ()
spaces = skipMany space

spaces1 :: Parser ()
spaces1 = skipMany1 space

unsignedInt :: Parser String
unsignedInt = many1 digit

signedInt :: Parser String
signedInt = plus <|> minus
  where
    plus = char '+' *> unsignedInt
    minus = (char '-') <:> unsignedInt

integer :: Parser String
integer = signedInt <|> unsignedInt

double :: Parser String
double = integer <++> mantissa <++> exponent
  where
    mantissa = option "" $ char  '.'  <:> unsignedInt
    exponent = option "" $ oneOf "eE" <:> integer

-- TODO: is this a hack?
parseInt :: Parser Term
parseInt = liftM Integer $ rd <$> (integer <* notFollowedBy (oneOf ".eE"))
  where
    rd = read :: String -> Int

parseDouble :: Parser Term
parseDouble = liftM Real $ rd <$> double
  where rd = read :: String -> Double

parseNumber :: Parser Term
parseNumber = try parseInt <|> parseDouble

parseBareAtom :: Parser Term
parseBareAtom = liftM Atom $ atom
  where atom = lower <:> many (alphaNum <|> char '_')

-- broken: need to fix the fact that single-quote \in anyChar
parseQuotedAtom :: Parser Term
parseQuotedAtom = liftM Atom $ atom
  where
    atom = quote *> escapedString <* quote
    quote = char '\''
    escapedString = many (try (string "''" >> return '\'') <|> noneOf "'")

parseGraphicToken :: Parser Term
parseGraphicToken = liftM Atom $ atom
  where atom = try twoPlus <|> ((:[]) <$> oneOf gChars)
        twoPlus = try (char '/' <:> ((oneOf (delete '*' gChars)) <:> theRest))
                  <|> (oneOf (delete '/' gChars) <:> (oneOf gChars <:> theRest))
        theRest = many $ oneOf gChars
        gChars = "#$&*+-./:<=>?@^~\\"

parseEmptyList :: Parser Term
parseEmptyList = char '[' >> spaces >> char ']' >> (return (Atom "[]"))

parseEmptyBracket :: Parser Term
parseEmptyBracket = char '{' >> spaces >> char '}' >> (return (Atom "{}"))

parseAtom :: Parser Term
parseAtom =  try parseBareAtom
         <|> try parseQuotedAtom
         <|> try parseGraphicToken
         <|> try parseEmptyList
         <|> try parseEmptyBracket

parseVariable :: Parser Term
parseVariable = liftM Variable var
  where var = (letter <|> char '_') <:> many (alphaNum <|> char '_')

-- We need two of these, because sometimes a compound term can have variables
-- and sometimes it must be completely instantiated, as in a fact.
parseStructure :: Parser Term
parseStructure = do
  (Atom functor) <- parseAtom <* (char '(' >> spaces)
  args <- sepBy parseTerm (char ',') <* char ')'
  return $ Structure functor args

parseVarFreeStructure :: Parser Term
parseVarFreeStructure = do
  (Atom functor) <- parseAtom <* (char '(' >> spaces)
  args <- sepBy parseVarFreeTerm (char ',') <* char ')'
  return $ Structure functor args

parseTerm :: Parser Term
parseTerm = spaces *> term <* spaces
  where term =  try parseStructure
            <|> try parseAtom
            <|> try parseVariable
            <|> parseNumber

parseVarFreeTerm :: Parser Term
parseVarFreeTerm = spaces *> term <* spaces
  where term =  try parseVarFreeStructure
            <|> try parseAtom
            <|> parseNumber

--- Facts are just variable-free compound terms
parseFact = liftM ((:[]) . Fact) (spaces *> parseVarFreeStructure <* (spaces >> char '.'))

-- a single rule statement  potentially lists several rules,
-- one for each conjunctive clause between semi-colons.
parseRule = do
  spaces
  head <- parseStructure
  spaces >> string ":-" >> spaces
  bodies <- parseBodies
  spaces >> char '.'
  return $ map (Rule head) bodies

parseBodies = do
  sepBy parseBody (spaces >> char ';' >> spaces)

parseBody = do
  sepBy parseStructure (spaces >> char ',' >> spaces)

-- Queries are just compound terms, maybe with variables.
parseQuery = do
  spaces >> string "?-" >> spaces
  query <- parseStructure
  spaces >> char '.'
  return [ (Query query) ]

parseExpr =  try parseRule
         <|> try parseQuery
         <|> parseFact

parseExprs = endBy parseExpr eol

eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"
   <?> "End of line"

-- main

-- readInput :: String -> String
-- readInput input = case parse parseExprs "prolog" input of
--   Left err -> "Error " ++ show err
--   Right val -> "Found " -- ++ show val
-- 
-- main :: IO ()
-- main = do
--   args <- getArgs
--   putStrLn $ readInput $ args !! 0

main :: IO ()
main = forever $ do putStrLn "Enter a number: "
                    input <- getLine
                    parseTest parseExpr input
