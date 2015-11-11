{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Angler.Compact
        ( compactAST ) where

import qualified Language.Angler.AST         as L       -- Loose
import qualified Language.Angler.ASTCompact  as C       -- Compact
import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.SrcLoc
import           Language.Angler.SymbolTable

import           Control.Applicative         (empty)

import           Control.Lens

import           Control.Monad               (when)
import           Control.Monad.State         (State, runState)

import           Data.Default                (Default(..))
import           Data.Maybe                  (isJust, fromMaybe)

--------------------------------------------------------------------------------
-- Compact monad
type Compact = State CompactState

data CompactState
  = CompactState
        { _cm_table     :: SymbolTableSpan
        , _cm_warnings  :: [Located Warning]
        , _cm_errors    :: [Located Error]
        }

makeLenses ''CompactState

instance STWarnings CompactState where
        st_warnings = cm_warnings

instance STErrors CompactState where
        st_errors = cm_errors

instance STScopedTable CompactState SymbolSpan where
        st_table = cm_table

instance Default CompactState where
        def = CompactState
                { _cm_table    = undefined
                , _cm_warnings = []
                , _cm_errors   = []
                }

compactAST :: L.ModuleSpan -> C.Module (SrcSpan, ())
compactAST = fst . flip runCompact def . compactModule

runCompact :: Compact a -> CompactState -> (a, CompactState)
runCompact = runState

----------------------------------------

compactModule :: L.ModuleSpan -> Compact (C.Module (SrcSpan, ()))
compactModule (L.Module name _expts _impts bdy ann) = do
        bdy' <- compactBody bdy
        return (C.Module name bdy' (ann, error "compactModule"))

compactBody :: L.BodySpan -> Compact (C.Body (SrcSpan, ()))
compactBody (L.Body stmts) = mapM_ loadTableBodyStmt stmts
                          >> C.Body <$> mapM compactBodyStmt stmts
    where
        loadTableBodyStmt :: L.BodyStmtSpan -> Compact ()
        loadTableBodyStmt stmt = case stmt of
                L.OpenType idn typ mcns ann -> do
                        typ' <- compactExprWhere typ
                        cns' <- mapM compactTypeBind (fromMaybe empty mcns)

                        let sym = SymbolType idn typ' cns' True

                        merr <- insertSc (view L.idn_str idn) sym
                        when (isJust merr) $ do
                                let Just err = merr
                                addError (Loc ann err)

                L.ReopenType idn cns ann -> do
                        cns' <- mapM compactTypeBind cns
                        return ()

        compactBodyStmt :: L.BodyStmtSpan -> Compact (C.BodyStmt (SrcSpan, ()))
        compactBodyStmt stmt = case stmt of
                L.OpenType idn typ mcns ann -> undefined

compactExprWhere :: L.ExprWhereSpan -> Compact (C.Expression SrcSpan)
compactExprWhere = undefined

compactTypeBind :: L.TypeBindSpan -> Compact (C.ExpressionBind SrcSpan)
compactTypeBind = undefined
