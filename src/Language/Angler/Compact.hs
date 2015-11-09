module Language.Angler.Compact
        ( compactAST ) where

import qualified Language.Angler.AST         as L       -- Loose
import qualified Language.Angler.ASTCompact  as C       -- Compact
import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.SymbolTable

import           Control.Lens
import           Control.Monad.State         (State, runState)

import           Data.Default                (Default(..))

data CompactState
  = CompactState
        { _cm_table     :: SymbolTableSpan
        , _cm_warnings  :: [Located Warning]
        -- , _cm_errors    :: [Located Error]
        }

makeLenses ''CompactState

instance Default CompactState where
        def = CompactState
                { _cm_table    = undefined
                , _cm_warnings = []
                }

--------------------------------------------------------------------------------
-- Compact monad

type Compact = State CompactState

compactAST :: L.Module a -> C.Module (a, ())
compactAST = fst . flip runCompact def . compactModule

runCompact :: Compact a -> CompactState -> (a, CompactState)
runCompact = runState

----------------------------------------

compactModule :: L.Module a -> Compact (C.Module (a, ()))
compactModule (L.Module name _expts _impts bdy annot) = do
        bdy' <- compactBody bdy
        return (C.Module name bdy' (annot, undefined))

compactBody :: L.Body a -> Compact (C.Body (a, ()))
compactBody (L.Body stmts) = mapM loadTableBodyStmt stmts
                         >>= (C.Body <$>) . mapM compactBodyStmt
    where
        loadTableBodyStmt :: L.BodyStmt a -> Compact (L.BodyStmt (a, ()))
        loadTableBodyStmt stmt = case stmt of
                L.OpenType idn typ mcns ann -> undefined
        compactBodyStmt :: L.BodyStmt (a, ()) -> Compact (C.BodyStmt (a, ()))
        compactBodyStmt stmt = case stmt of
                L.OpenType idn typ mcns ann -> undefined
