{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Angler.Compact
        ( compactAST ) where

import qualified Language.Angler.Program     as P
import           Language.Angler.AST
import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.SrcLoc

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

compactAST :: P.ModuleSpan -> ()
compactAST = fst . flip runCompact def . compactModule

runCompact :: Compact a -> CompactState -> (a, CompactState)
runCompact = runState

----------------------------------------

compactModule :: P.ModuleSpan -> Compact ()
compactModule (P.Module name _expts _impts bdy ann) = compactBody bdy

compactBody :: P.BodySpan -> Compact ()
compactBody (P.Body stmts) = mapM_ loadTableBodyStmt stmts
                          >> mapM_ compactBodyStmt stmts
    where
        loadTableBodyStmt :: P.BodyStmtSpan -> Compact ()
        loadTableBodyStmt stmt = case stmt of
                P.OpenType idn typ mcns ann -> do
                        typ' <- compactExprWhere typ
                        cns' <- mapM compactTypeBind (fromMaybe empty mcns)

                        let sym = SymbolType idn typ' cns' True

                        merr <- insertSc (view P.idn_str idn) sym
                        when (isJust merr) $ do
                                let Just err = merr
                                addError (Loc ann err)

                P.ReopenType idn cns ann -> do
                        cns' <- mapM compactTypeBind cns
                        return ()

        compactBodyStmt :: P.BodyStmtSpan -> Compact ()
        compactBodyStmt stmt = case stmt of
                P.OpenType idn typ mcns ann -> undefined

compactExprWhere :: P.ExprWhereSpan -> Compact (Expression SrcSpan)
compactExprWhere = undefined

compactTypeBind :: P.TypeBindSpan -> Compact (ExpressionBind SrcSpan)
compactTypeBind = undefined
