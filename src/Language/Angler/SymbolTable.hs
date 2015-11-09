module Language.Angler.SymbolTable where

import           Language.Angler.ScopedTable
import           Language.Angler.ASTCompact
import           Language.Angler.Error
import           Language.Angler.SrcLoc

import           Control.Lens

import           Data.Sequence               (Seq)

type SymbolTable a = ScopedTable (Symbol a)
type SymbolTableSpan = SymbolTable SrcSpan

data Symbol a
  = SymbolFunction
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        , _sym_def      :: Expression a
        , _sym_annot    :: a
        }
  | SymbolOpenType
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        , _sym_cons     :: Seq (ExpressionBind a)
        , _sym_annot    :: a
        }
  | SymbolClosedType
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        , _sym_cons     :: Seq (ExpressionBind a)
        , _sym_annot    :: a
        }
  | SymbolArgument
        { _sym_idn      :: Identifier a
        , _sym_type     :: Expression a
        -- , _sym_val      :: Expression a
        , _sym_annot    :: a
        }
type SymbolSpan = Symbol SrcSpan

makeLenses ''Symbol
