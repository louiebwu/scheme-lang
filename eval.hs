newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
    deriving (Monad
    , Functor
    , Applicative
    , MonadReader EnvCtx
    , MonadIO)

-- Primitive Environment

basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList $ primEnv <> [("read" , Fun $ IFunc $ unop $ readFn)]

evalFile :: T.Text -> IO () -- Program File
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr) >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show ) evalBody $ readExprFile input

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runResourceT $ runReaderT (unEval action) code

-- Evaluation Function

eval :: LispVal -> Eval LispVal