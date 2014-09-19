module Poplog.Parser where

-- external imports
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Data.List (intersperse, delete)
import Text.ParserCombinators.Parsec hiding (spaces)

-- internal imports
import Poplog.Types

-- Helper Functions

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

listToTerm :: Name -> [Term] -> Term
listToTerm n []     = (Atom "fail")
listToTerm n (y:[])   = y
listToTerm n (x:xs)   = (Structure n [x,(listToTerm n xs)])

termToList :: Name -> Term -> [Term]
termToList n (Structure f (b1:b2:[])) = if n == f
                                        then b1:(termToList n b2)
                                        else []
termToList n _ = []

-- Helper Parsers

eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"
   <?> "End of line"

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
parseAtom :: Parser Term
parseAtom =  try parseBareAtom
         <|> try parseQuotedAtom
         <|> try parseGraphicToken

parseVariable :: Parser Term
parseVariable = liftM (flip Variable $ 0) var
  where var = (letter <|> char '_') <:> many (alphaNum <|> char '_')

parseHeadedList :: Parser Term
parseHeadedList = do
    char '[' >> spaces
    heads <- sepBy parseTerm (spaces >> char ',' >> spaces)
    spaces >> char '|' >> spaces
    tail <- (try parseList <|> parseVariable)
    spaces >> char ']'
    return $ makeList heads tail

parseList :: Parser Term
parseList = do
    char '[' >> spaces
    xs <- sepBy parseTerm (spaces >> char ',' >> spaces)
    spaces >> char ']'
    return $ makeList xs (Structure "[]" [])

makeList :: [Term] -> Term -> Term
makeList [] tail = tail
makeList (x:xs) tail = (Structure "." [x,(makeList xs tail)])

parseEmptyList :: Parser Term
parseEmptyList = char '[' >> spaces >> char ']' >> (return (Structure "[]" []))

parseEmptyBracket :: Parser Term
parseEmptyBracket = char '{' >> spaces >> char '}' >> (return (Structure "{}" []))

-- We need two of these, because sometimes a compound term can have variables
-- and sometimes it must be completely instantiated, as in a fact.
parseStructure :: Parser Term
parseStructure =
  do
    (Atom functor) <- parseAtom <* (char '(' >> spaces)
    args <- sepBy parseTerm (char ',') <* char ')'
    return $ Structure functor args
  <|> try parseHeadedList
  <|> try parseList
  <|> try parseEmptyList
  <|> try parseEmptyBracket

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
parseFact = liftM (\x -> Rule x (Atom "true")) (spaces *> parseStructure <* (spaces >> char '.'))

-- a single rule statement  potentially lists several rules,
-- one for each conjunctive clause between semi-colons.
parseRule = do
  spaces
  head <- parseStructure
  spaces >> string ":-" >> spaces
  bodies <- parseBodies
  spaces >> char '.'
  return $ Rule head bodies

parseBodies = liftM (listToTerm ";") $ sepBy parseBody (spaces >> char ';' >> spaces)

parseBody = liftM (listToTerm ",") $ sepBy parseStructure (spaces >> char ',' >> spaces)

-- Queries are just compound terms, maybe with variables.
parseQuery = do
  spaces >> string "?-" >> spaces
  query <- parseBodies
  spaces >> char '.'
  return (Query query)

parseExpr =  try parseRule
         <|> try parseQuery
         <|> parseFact

parseExpr2 = parseExpr <* eol

parseExprs = endBy parseExpr eol

parseProlog input = parse parseExpr "Prolog" input
