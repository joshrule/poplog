module Repl where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec (ParseError)

-- Error Type
data ReplError = ReplError String
instance Show ReplError where show = showReplError

showReplError (ReplError s) = s

-- Repl
repE :: (MonadState s m, Show e, Show b) =>
        (a -> m (Either e b)) -> a -> ExceptT ReplError m b
repE evalFn input = do
    result <- lift $ evalFn input
    case result of
        Left err -> throwError $ ReplError (show err)
        Right val -> return val

repR parseFn input = case parseFn input of
    Left err -> throwError $ ReplError (show err)
    Right val -> return val

repP :: (Show a1, Show e, Monad m) => ExceptT e m a1 -> m String
repP input = extractValue $ trapError $ liftM show input

rep :: (MonadState s m, MonadIO m, Show b, Show e) => (String -> Either ParseError a) -> (a -> m (Either e b)) -> String -> m ()
rep parseFn evalFn input = do
    str <- repP $ (return input) >>= repR parseFn >>= repE evalFn
    liftIO $ putStrLn str
  
extractValue :: (Monad m) => ExceptT e m a -> m a
extractValue result = do
   result' <- (runExceptT result)
   case result' of 
     (Right val) -> return val

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

readPrompt :: (MonadState s m , MonadIO m) => String -> m String
readPrompt prompt = liftIO $ flushStr prompt >> liftIO getLine

runFile :: (Show b, Show e) => (String -> Either ParseError a)
           -> (a -> StateT [t] IO (Either e b)) -> FilePath -> IO ()
runFile parseFn evalFn filename = do
  str <- readFile filename
  let strs = lines str
  evalStateT (mapM (rep parseFn evalFn) strs) []
  return ()

runRepl :: (MonadIO m, Show b, Show e) => (String -> Either ParseError a)
           -> (a -> StateT [t] m (Either e b)) -> m ()
runRepl parseFn evalFn = evalStateT (until_ (== "quit") (readPrompt ">> ") (rep parseFn evalFn)) []

conditionalRepl :: (Show b, Show e) => (String -> Either ParseError a)
                   -> (a -> StateT [t] IO (Either e b)) -> [Char] -> IO ()
conditionalRepl parseFn evalFn name = do
    args <- getArgs
    case length args of
      0 -> runRepl parseFn evalFn
      1 -> runFile parseFn evalFn $ args !! 0
      otherwise -> putStrLn ("Error: " ++ name ++ " only takes 0 or 1 argument.")
