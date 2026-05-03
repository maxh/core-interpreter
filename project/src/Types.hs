-- | Here are the types for the Core interpreter.
-- You may add constructors if you wish, but do not modify any that are already here.
module Types where

import qualified Data.HashMap.Lazy as M

type Env = M.HashMap Name Out

type BinOp = String

type Name = String

type IsRec = Bool

data Out
  = OutInt Int
  | OutBool Bool
  | OutPack Int [Out]
  | OutClosure [Name] Expr Env

data Expr
  = EVar Name -- Variables
  | ENum Int -- Numbers
  | EBinOp Expr BinOp Expr
  | EPack Int Int -- Constructors
  | EAp Expr Expr -- Applications
  | ELet
      IsRec -- is the let recursive?
      [(Name, Expr)] -- Local Variable definitions
      Expr -- Body of the let
  | ECase -- Case Expressions
      Expr
      [(Int, [Name], Expr)] -- Alternatives
  | ELam [Name] Expr -- Functions (Lambdas)
  deriving (Eq, Show)

type Decl = (Name, [Name], Expr) -- The name, parameter list, and body of a supercombinator declaration

type Core = M.HashMap Name Decl -- A core program is an environment of declarations
