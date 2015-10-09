module Language.Angler.SymbolTable
        ( SymbolTable, SymbolTableSpan
        , Symbol(..), SymbolSpan
        , sym_idn, sym_type, sym_def, sym_cons, sym_annot

        -- basic
        -- , empty
        -- , lookup, (!) , elem
        -- , safeInsert, insert
        ) where

import           Language.Angler.ScopedTable
import           Language.Angler.ASTCompact
import           Language.Angler.Error
import           Language.Angler.SrcLoc

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

makeLenses ''Symbol

-- insert :: String -> Symbol a -> SymbolTable a -> Either Error (SymbolTable a)
-- insert idn sym tab = case Map.lookup idn tab of
--         Just ins -> error "SymbolTable.insert: FIXME"
--         Nothing  -> Right (Map.insert idn sym tab)
--
-- insertFunctionDecl :: Identifier a -> Expression a -> a
--                    {--> _-} ->  SymbolTable a -> Either Error (SymbolTable a)
-- insertFunctionDecl idn ex ann {-op-} tab = case view (at idnStr) tab of
--         Just _  -> (Left . CheckError . CErrAlreadyInSymbolTable) idnStr
--         Nothing -> Right (Map.insert idnStr symFun tab)
--     where
--         -- symFun :: Symbol a
--         symFun = SymbolFunction idn ex (CaseOf Nothing mempty (ex^.exp_annot)) ann
--         idnStr :: String
--         idnStr = view idn_str idn

-- insertOpen :: L.Identifier a -> L.ExprWhere a -> Maybe (Seq (L.TypeBind a)) -> a
--            {--> _-} -> SymbolTable a -> Either Error (SymbolTable a)
-- insertOpen idn whr mcns ann {-op-} tab = case view (at idn) tab of
--         Just ins ->
--         Nothing  -> Right (Map.insert idn (Symbol SymbolOpenType (whereToLet whr) ))

-- elem :: String -> SymbolTable a -> Bool
-- elem idn = P.elem idn . Map.keys
--
-- notElem :: String -> SymbolTable a -> Bool
-- notElem idn = not . elem idn
