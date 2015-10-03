module Language.Angler.SymbolTable
        ( SymbolTable, SymbolTableSpan

        , Symbol(..), SymbolSpan

        , Map.empty
        , Map.lookup, (Map.!)
        , insert
        , Map.delete

        , elem
        , Map.mapKeys
        , Map.filterWithKey
        ) where

import           Language.Angler.ASTCompact
import           Language.Angler.Error
import           Language.Angler.SrcLoc

import           Control.Lens               hiding (op)

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Sequence              (Seq)

import           Prelude                    hiding (elem, notElem)
import qualified Prelude                    as P (elem)

type SymbolTable a = Map String (Symbol a)
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

insert :: String -> Symbol a -> SymbolTable a -> Either Error (SymbolTable a)
insert idn sym tab = case Map.lookup idn tab of
        Just ins -> error "SymbolTable.insert: FIXME"
        Nothing  -> Right (Map.insert idn sym tab)

insertFunctionDecl :: Identifier a -> Expression a -> a
                   {--> _-} ->  SymbolTable a -> Either Error (SymbolTable a)
insertFunctionDecl idn ex ann {-op-} tab = case view (at idnStr) tab of
        Just _  -> (Left . CheckError . AlreadyInSymbolTable) idnStr
        Nothing -> Right (Map.insert idnStr symFun tab)
    where
        -- symFun :: Symbol a
        symFun = SymbolFunction idn ex (CaseOf Nothing mempty (ex^.exp_annot)) ann
        idnStr :: String
        idnStr = view idn_str idn

-- insertOpen :: L.Identifier a -> L.ExprWhere a -> Maybe (Seq (L.TypeBind a)) -> a
--            {--> _-} -> SymbolTable a -> Either Error (SymbolTable a)
-- insertOpen idn whr mcns ann {-op-} tab = case view (at idn) tab of
--         Just ins ->
--         Nothing  -> Right (Map.insert idn (Symbol SymbolOpenType (whereToLet whr) ))

elem :: String -> SymbolTable a -> Bool
elem idn = P.elem idn . Map.keys

notElem :: String -> SymbolTable a -> Bool
notElem idn = not . elem idn
