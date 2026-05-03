-- | The parser goes here
module Parse where

import Control.Monad
import Data.Functor.Identity
import Data.Functor.Identity (Identity)
import qualified Data.HashMap.Lazy as M
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding (Parser)
import Types
import Prelude hiding (id)

--- The Parser
--- ----------
--
-- Pretty parser type
type Parser = ParsecT String Int Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do
  string s
  spaces
  return s

int :: Parser Int
int = do
  digits <- many1 digit <?> "an integer"
  spaces
  return (read digits :: Int)

-- non-standard sig bc of eq precendence but diff associativity
ePlusMinusOp :: Parser (Expr -> Expr -> Expr, Bool)
ePlusMinusOp =
  (op "+" >> return (\l r -> EBinOp l "+" r, True))
    <|> (op "-" >> return (\l r -> EBinOp l "-" r, False))

-- non-standard sig bc of eq precendence but diff associativity
eMultDivOp :: Parser (Expr -> Expr -> Expr, Bool)
eMultDivOp =
  (op "*" >> return (\l r -> EBinOp l "*" r, True))
    -- complexity needed to avoid consuming / in /=
    <|> ( (try (string "/" <* notFollowedBy (char '=')) >> spaces)
            >> return (\l r -> EBinOp l "/" r, False)
        )

eRelOp :: Parser (Expr -> Expr -> Expr)
eRelOp = do
  opStr <- op ">=" <|> op "<=" <|> op "==" <|> op "/=" <|> op "<" <|> op ">"
  spaces
  return $ \l r -> (EBinOp l opStr r)

eAndOp :: Parser (Expr -> Expr -> Expr)
eAndOp = do
  opStr <- op "&"
  spaces
  return $ \l r -> (EBinOp l opStr r)

eOrOp :: Parser (Expr -> Expr -> Expr)
eOrOp = do
  opStr <- op "|"
  spaces
  return $ \l r -> (EBinOp l opStr r)

keywords = ["let", "letrec", "in", "case", "of"]

id :: Parser Name
id = do
  first <- oneOf ['a' .. 'z']
  rest <- many (oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "'_")
  spaces
  let name = first : rest
  if name `elem` keywords
    then fail "keyword"
    else return name

eInt :: Parser Expr
eInt = do
  i <- int
  return $ ENum i

eVar :: Parser Expr
eVar = do
  v <- try id
  return $ EVar v

eParen :: Parser Expr
eParen = do
  _ <- symbol "("
  expr <- expr
  _ <- symbol ")"
  return expr

eDefn :: Parser (Name, Expr)
eDefn = do
  v <- try id
  _ <- symbol "="
  exp <- expr
  return (v, exp)

eDefns :: Parser [(Name, Expr)]
eDefns = do
  defns <- eDefn `sepBy` (symbol ";")
  return defns

eAlt :: Parser (Int, [Name], Expr)
eAlt = do
  _ <- symbol "<"
  tag <- int
  _ <- symbol ">"
  ids <- many id
  _ <- symbol "->"
  exp <- expr
  return (tag, ids, exp)

eAlts :: Parser [(Int, [Name], Expr)]
eAlts = do
  first <- eAlt
  rest <- many (try (symbol ";" >> eAlt))
  return (first : rest)

eLet :: Parser Expr
eLet = do
  _ <- symbol "let"
  defns <- eDefns
  _ <- symbol "in"
  exp <- expr
  return $ ELet False defns exp

eLetRec :: Parser Expr
eLetRec = do
  _ <- symbol "letrec"
  defns <- eDefns
  _ <- symbol "in"
  exp <- expr
  return $ ELet True defns exp

-- needed to embed non-assoc rel op parsing within precendence hierarchy
nonAssoc :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
nonAssoc p combOp = do
  l <- p
  rest <- optionMaybe (do f <- combOp; r <- p; return (f l r))
  case rest of
    Nothing -> return l
    Just e -> return e

mixedPrec :: Parser Expr -> Parser (Expr -> Expr -> Expr, Bool) -> Parser Expr
mixedPrec nextLevel ops = do
  left <- nextLevel
  mops <- optionMaybe ops
  case mops of
    Nothing -> return left
    (Just (f, True)) -> do
      right <- mixedPrec nextLevel ops
      return (f left right)
    (Just (f, False)) -> do
      right <- nextLevel
      return (f left right)

expr :: Parser Expr
expr =
  let and = rel `chainr1` eAndOp
      rel = plusMinus `nonAssoc` eRelOp
      plusMinus = multDiv `mixedPrec` ePlusMinusOp
      multDiv = ap `mixedPrec` eMultDivOp
      ap = factor `chainl1` eAp
      factor = atom
   in and `chainr1` eOrOp

op :: String -> Parser String
op o = try (symbol o)

eAp :: Parser (Expr -> Expr -> Expr)
eAp = do
  return EAp

eConstr :: Parser Expr
eConstr = do
  c <- constr
  return $ EVar c

eCase :: Parser Expr
eCase = do
  _ <- symbol "case"
  cExp <- expr
  _ <- symbol "of"
  alts <- eAlts
  return $ ECase cExp alts

eLamb :: Parser Expr
eLamb = do
  _ <- symbol "\\"
  ids <- many id
  _ <- symbol "."
  exp <- expr
  return $ ELam ids exp

atom :: Parser Expr
atom = eInt <|> try eLetRec <|> try eLet <|> try eCase <|> try eLamb <|> eVar <|> eConstr <|> eParen

constr :: Parser Name
constr = do
  first <- oneOf ['A' .. 'Z']
  rest <- many (oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "'_")
  spaces
  return $ first : rest

ePack :: Parser Decl
ePack = do
  name <- constr
  fields <- many tyField
  count <- getState
  setState (count + 1)
  return (name, [], (EPack count (length fields)))

tyExpr :: Parser [Decl]
tyExpr = do
  packs <- ePack `sepBy` (symbol "|")
  return packs

tyField :: Parser String
tyField = try id <|> tyParam <|> tyConstExp

tyConstExp :: Parser String
tyConstExp = do
  _ <- symbol "("
  _ <- try id
  _ <- many (try id <|> tyParam)
  _ <- symbol ")"
  return "_"

tyParam :: Parser String
tyParam = do
  s <- many1 (symbol "*")
  spaces
  return $ concat s

decl :: Parser [Decl]
decl = do
  name <- try id
  params <- many (try id <|> tyParam)
  assign <- symbol "=" <|> symbol "::="
  case assign of
    "=" -> do
      body <- expr
      return [(name, params, body)]
    "::=" -> do
      packs <- tyExpr
      return packs
    _ -> error $ "unexpected assignment " ++ assign

core :: Parser Core
core = do
  decls <- decl `sepBy` (symbol ";")
  let flatDecls = concat decls
  return $ M.fromList [(n, v) | v@(n, _, _) <- flatDecls]

parseCore :: String -> Either ParseError Core
parseCore text = runParser core 1 "Core" text
