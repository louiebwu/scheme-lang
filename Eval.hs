{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (
  evalFile,
  runParseTest,
  safeExec,
  -- testing
  runASTinEnv,
  basicEnv,
  fileToEvalForm,
  lineToEvalForm
) where

import Prims ( primEnv, unop )
import Parser ( readExpr, readExprFile )
import LispVal
    ( LispException( PError, UnboundVar, TypeMismatch,
                    BadSpecialForm, NotFunction),
      IFunc(IFunc),
      LispVal(..),
      Eval(unEval),
      EnvCtx )

import Data.Semigroup ((<>))
import Data.Map as Map ( fromList, insert, lookup, Map )
import qualified Data.Text as T
import Control.Monad.Reader ( MonadReader(local, ask), ReaderT(runReaderT) )
import Control.Exception ( throw, try, fromException, SomeException )
import Control.Monad.IO.Class (MonadIO)

basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList $ primEnv <> [("read" , Fun $ IFunc $ unop $ readFn)]

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn val          = throw $ TypeMismatch "read expects a string" val

parseFn :: LispVal -> Eval LispVal
parseFn (String txt)    = either (throw . PError . show) return $ readExpr txt
parseFn val             = throw $ TypeMismatch "parse expects string, instead got: " val

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show) eval $ readExpr input

evalFile :: T.Text -> IO ()
evalFile fileExpr = do
    result <- safeExec $ runASTinEnv basicEnv $ fileToEvalForm fileExpr
    case result of
        Left err  -> putStrLn $ "Error: " ++ err
        Right val -> print val

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show ) evalBody $ readExprFile input

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
    env <- ask
    case Map.lookup atom env of
        Just x  -> return x
        Nothing -> throw $ UnboundVar atom

getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd(xs)

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x:xs) = getEven(xs)

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "expected an atomic value" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
extractVar n = throw $ TypeMismatch "expected an atomic value" n

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
    env     <- ask
    argEval <- mapM eval args
    let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params argEval) <> env in local (const env' ) $ eval expr

-- Evaluation Function

eval :: LispVal -> Eval LispVal

-- Quote, Auto-quote
eval (List [Atom "quote", val]) = return val
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil        = return Nil

-- Write

eval (List [Atom "write", rest]) = return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) = return . String . T.pack . show $ List rest

-- Atoms

eval n@(Atom _) = getVar n

-- If

eval (List [Atom "if", pred, truExpr, flsExpr]) = do
    ifRes <- eval pred
    case ifRes of
        (Bool True)     -> eval truExpr
        (Bool False)    -> eval flsExpr
        _               -> throw $ BadSpecialForm "if"

-- Let

eval (List [Atom "let", List pairs, expr]) = do
    env     <- ask
    atoms   <- mapM ensureAtom $ getEven pairs
    vals    <- mapM eval $ getOdd pairs
    let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env in local (const env') $ evalBody expr

-- Begin, Define

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest

eval (List [Atom "define", varExpr, expr]) = do
    varAtom <- ensureAtom varExpr
    evalVal <- eval expr
    env     <- ask
    let envFn = const $ Map.insert (extractVar varAtom) evalVal env in local envFn $ return varExpr

-- Lambda Functions

eval (List [Atom "lambda", List params, expr]) = do
    envLocal <- ask
    return $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda"

-- Application

eval (List ((:) x xs)) = do
    funVar  <- eval x
    xVal    <- mapM eval xs
    case funVar of
        (Fun (IFunc internalFn))                -> internalFn xVal
        (Lambda (IFunc internalfn) boundenv)    -> local (const boundenv) $ internalfn xVal
        _                                       -> throw $ NotFunction funVar

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    env     <- ask
    local (const $ Map.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
    evalVal <- eval defExpr
    env     <- ask
    let envFn = const $ Map.insert var evalVal env in local envFn $ evalBody $ List rest   
evalBody x = eval x

-- Error catching

safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) ->
      case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                -> return $ Left (show eTop)
    Right val -> return $ Right val