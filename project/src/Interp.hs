-- | Interp
module Interp where

import qualified Data.HashMap.Lazy as M
import Types

gatherArgs :: Expr -> (Expr, [Expr])
gatherArgs (EAp f x) =
  let (g, args) = gatherArgs f
   in (g, args ++ [x])
gatherArgs e = (e, [])

eval :: Expr -> Env -> Core -> Out
eval (ENum i) _ _ = (OutInt i)
eval (EPack tag _) _ _ = OutPack tag []
eval (ELam params body) env _ = OutClosure params body env
eval (EAp e1 e2) env p =
  let (fn, args) = gatherArgs (EAp e1 e2)
      argVals = map (\arg -> eval arg env p) args
   in case fn of
        EVar fnName ->
          case M.lookup fnName p of
            Just (_, _, EPack tag _) ->
              OutPack tag argVals
            Just (_, params, body) ->
              let (boundArgs, extraArgs) = splitAt (length params) argVals
                  env' = foldl (\acc (param, val) -> M.insert param val acc) env (zip params boundArgs)
                  result = eval body env' p
               in if null extraArgs then result else applyOut result extraArgs env p
            Nothing -> applyOut (eval fn env p) argVals env p
        _ -> applyOut (eval fn env p) argVals env p
eval (ECase exp alts) env p =
  case eval exp env p of
    OutPack tag fields ->
      case filter (\(t, _, _) -> t == tag) alts of
        ((_, names, body) : _) ->
          let env' = foldl (\acc (name, val) -> M.insert name val acc) env (zip names fields)
           in eval body env' p
        [] -> error $ "no matching case alt for tag " ++ show tag
    _ -> error "case is not a constructor"
eval (EVar v) e prog = case M.lookup v e of
  (Just o) -> o
  Nothing -> case M.lookup v prog of
    (Just (_, [], exp)) -> eval exp e prog
    (Just (_, params, exp)) -> OutClosure params exp e
    _ -> error $ "not found " ++ v
eval (EBinOp l "+" r) env p = arithOp (+) l r env p
eval (EBinOp l "-" r) env p = arithOp (-) l r env p
eval (EBinOp l "*" r) env p = arithOp (*) l r env p
eval (EBinOp l "/" r) env p = arithOp div l r env p
eval (EBinOp l ">" r) env p = relOp (>) l r env p
eval (EBinOp l "<" r) env p = relOp (<) l r env p
eval (EBinOp l ">=" r) env p = relOp (>=) l r env p
eval (EBinOp l "<=" r) env p = relOp (<=) l r env p
eval (EBinOp l "==" r) env p = relOp (==) l r env p
eval (EBinOp l "/=" r) env p = relOp (/=) l r env p
eval (EBinOp l "&" r) env p = boolOp (&&) l r env p
eval (EBinOp l "|" r) env p = boolOp (||) l r env p
eval (ELet False [] body) env p = eval body env p
eval (ELet False bindings body) env p =
  let env' = M.union defs env
      defs = M.fromList [(name, eval exp env p) | (name, exp) <- bindings]
   in eval body env' p
eval (ELet True [] body) env p = eval body env p
eval (ELet True bindings body) env p =
  let env' = M.union defs env
      -- note: env' not env here
      defs = M.fromList [(name, eval exp env' p) | (name, exp) <- bindings]
   in eval body env' p

asInt :: Out -> Int
asInt (OutInt i) = i
asInt _ = error "expected int"

evalAsInt :: Expr -> Env -> Core -> Int
evalAsInt e env p = asInt $ eval e env p

asBool :: Out -> Bool
asBool (OutBool i) = i
asBool _ = error "expected bool"

evalAsBool :: Expr -> Env -> Core -> Bool
evalAsBool e env p = asBool $ eval e env p

applyOut :: Out -> [Out] -> Env -> Core -> Out
applyOut (OutClosure names body cloEnv) argVals env p
  | length argVals < length names =
      let (boundNames, remainingNames) = splitAt (length argVals) names
          env' = foldl (\acc (name, val) -> M.insert name val acc) (M.union env cloEnv) (zip boundNames argVals)
       in OutClosure remainingNames body env'
  | otherwise =
      let env' = foldl (\acc (name, val) -> M.insert name val acc) (M.union env cloEnv) (zip names argVals)
       in eval body env' p
applyOut (OutPack tag fields) argVals _ _ = OutPack tag (fields ++ argVals)
applyOut out _ _ _ = error $ "cannot apply non-function: " ++ showOut out

arithOp :: (Int -> Int -> Int) -> Expr -> Expr -> Env -> Core -> Out
arithOp op l r env p = OutInt $ op (evalAsInt l env p) (evalAsInt r env p)

relOp :: (Int -> Int -> Bool) -> Expr -> Expr -> Env -> Core -> Out
relOp op l r env p = OutBool $ op (evalAsInt l env p) (evalAsInt r env p)

boolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Env -> Core -> Out
boolOp op l r env p = OutBool $ op (evalAsBool l env p) (evalAsBool r env p)

-- Use this function as your top-level entry point so you don't break `app/Main.hs`

run :: Core -> String
run prog =
  case M.lookup "main" prog of
    Nothing -> error "Supercombinator main not defined."
    Just (_, [], mainBody) ->
      let result = eval mainBody (M.empty) prog
       in showOut result

showOut :: Out -> String
showOut (OutInt i) = show i
showOut (OutBool b) = show b
showOut (OutPack tag fields) = "Pack " ++ show tag ++ " [" ++ unwords (map showOut fields) ++ "]"
showOut (OutClosure _ _ _) = "<closure>"
