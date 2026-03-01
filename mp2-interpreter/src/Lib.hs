module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal (op x y)
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"


--- Helpers
isExn :: Val->Bool
isExn (ExnVal _) = True
isExn _ = False

firstExn :: [Val] -> Maybe Val
firstExn [] = Nothing
firstExn (x:xs) =
    case x of
        ex@(ExnVal _) -> Just ex
        _  -> firstExn xs

evalArgs :: [Exp] -> Env -> Either Val [Val]
evalArgs [] _ = Right []
evalArgs (y:ys) env = 
    case eval y env of
        ex@(ExnVal _) -> Left ex
        v -> case evalArgs ys env of
            Left ex -> Left ex
            Right vs -> Right (v:vs)

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp b) _ = BoolVal b

--- ### Variables

eval (VarExp s) env = 
    case H.lookup s env of
        Just v -> v
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (v1,v2) of
        (IntVal _, IntVal 0) | op == "/" -> ExnVal "Division by 0"
        _ ->
          case H.lookup op intOps of
            Just f -> liftIntOp f v1 v2
            Nothing -> ExnVal ("Unknown operator: " ++ op)

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (v1,v2) of
        (ex@(ExnVal _), _) -> ex
        (_, ex@(ExnVal _)) -> ex
        _ ->
          case H.lookup op boolOps of
            Just f -> liftBoolOp f v1 v2
            Nothing -> ExnVal ("Unknown operator: " ++ op)

eval (CompOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case H.lookup op compOps of
        Just f -> liftCompOp f v1 v2
        Nothing -> ExnVal ("Unknown operator: " ++ op)

--- ### If Expressions

eval (IfExp e1 e2 e3) env =
    case eval e1 env of
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _             -> ExnVal "Condition is not a Bool" 

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    case eval e1 env of
        CloVal params body cloEnv ->
            case evalArgs args env of
                Left ex -> ex
                Right vs ->
                  let newEnv = H.union (H.fromList(zip params vs)) cloEnv
                  in eval body newEnv
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env =
    let names = map fst pairs
        exps  = map snd pairs
    in case evalArgs exps env of
         Left ex -> ex
         Right vs ->
            let newEnv = H.union (H.fromList (zip names vs)) env
            in eval body newEnv 

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env =
  case eval e env of
    ex@(ExnVal _) -> (show ex, penv, env)
    v             -> ("", penv, H.insert var v env)

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (s:ss)) penv env =
  let (o1, p1, e1) = exec s penv env
      (o2, p2, e2) = exec (SeqStmt ss) p1 e1
  in (o1 ++ o2, p2, e2)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env =
    case eval e1 env of
    BoolVal True  -> exec s1 penv env
    BoolVal False -> exec s2 penv env
    _             -> (show (ExnVal "Condition is not a Bool"), penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env =
    case H.lookup name penv of
    Nothing -> ("Procedure " ++ name ++ " undefined", penv, env)
    Just (ProcedureStmt _ params body) ->
      case evalArgs args env of
        Left ex -> (show ex, penv, env)
        Right vs ->
          let env' = H.union (H.fromList (zip params vs)) env
          in exec body penv env'
    Just _ -> ("Procedure " ++ name ++ " undefined", penv, env)
