module Language.Angler.SymbolTable where

import           Language.Angler.ScopedTable hiding (empty)
import           Language.Angler.ASTCompact
import           Language.Angler.Error
import           Language.Angler.SrcLoc

import           Control.Lens

import           Control.Applicative         (empty)
import           Control.Monad               (unless)

import           Data.Sequence               (Seq)

type SymbolTable a = ScopedTable (Symbol a)
type SymbolTableSpan = SymbolTable SrcSpan

data Symbol a
  = SymbolFunction
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        , _sym_def      :: Maybe (Expression a)
        }
  | SymbolType
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        , _sym_cons     :: Seq (ExpressionBind a)
        , _sym_open     :: Bool
        }
  | SymbolArgument
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        -- , _sym_val      :: Expression a
        }
type SymbolSpan = Symbol SrcSpan

isSymbolFunction :: Symbol a -> Bool
isSymbolFunction sym = case sym of
        SymbolFunction {} -> True
        _                 -> False

isSymbolType :: Symbol a -> Bool
isSymbolType sym = case sym of
        SymbolType {} -> True
        _             -> False

isSymbolArgument :: Symbol a -> Bool
isSymbolArgument sym = case sym of
        SymbolArgument {} -> True
        _                 -> False

makeLenses ''Symbol

insertFunctionDecl :: Identifier a -> Expression a -> SymbolTable a -> Either Error (SymbolTable a)
insertFunctionDecl idn typ = safeInsert (view idn_str idn) (SymbolFunction idn typ Nothing)

-- insertFunctionDef :: Identifier a -> [Argument a] -> Expression a -> SymbolTable a -> Either Error (SymbolTable a)
-- insertFunctionDef idn args expr tab = do
--         let str = view idn_str idn
--
--         unless (elemInCurrentScope str tab) $ Left (error "insertFunctionDef")
--         let Just tabSym = lookup str tab
--
--         unless (isSymbolFunction tabSym) $ Left (error "insertFunctionDef 2")
--         let SymbolFunction _ typ mexpr = tabSym
--         let expr' = maybe expr (addCaseAlt args expr) mexpr
--
--     where
--         addCaseAlt :: [Argument a] -> Expression a -> Expression a -> Expression a
--         addCaseAlt as x old = case old of
--                 Lambda larg ltyp lexpr ann -> case lexpr of
--                         CaseOf arg calts ann -> CaseOf arg (buildAlt as x <| calts) ann
--                         _ -> CaseOf x
