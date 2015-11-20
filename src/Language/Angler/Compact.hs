{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Angler.Compact
        ( compactAST ) where

import qualified Language.Angler.Program     as P
import           Language.Angler.AST
import qualified Language.Angler.ScopedTable as ST
import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.SrcLoc

import           Control.Applicative         (empty)

import           Control.Lens

import           Control.Monad               (when)
import           Control.Monad.State         (State, runState)

import           Data.Default                (Default(..))
import           Data.Foldable               (forM_, toList)
import           Data.Function               (on)
import           Data.Maybe                  (isJust, isNothing, fromMaybe)

--------------------------------------------------------------------------------
-- Compact monad

addCError :: CheckError -> SrcSpan -> Compact ()
addCError cerr spn = addError (Loc spn (CheckError cerr))

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
                { _cm_table    = ST.fromFoldable [typeSym, arrowSym]
                , _cm_warnings = []
                , _cm_errors   = []
                }
            where
                typeSym :: (String, SymbolSpan)
                typeSym = (str, sym)
                    where
                        str = "Type"
                        sym = SymbolType str (noInfo TypeType) True
                arrowSym :: (String, SymbolSpan)
                arrowSym = (str, sym)
                    where
                        str = "_->_"
                        sym = SymbolFunction str arrTyp empty True
                        arrTyp :: ExpressionSpan
                        arrTyp = noInfo (Arrow (noInfo TypeType) (noInfo (Arrow (noInfo TypeType) (noInfo TypeType))))
                noInfo :: (SrcSpan -> a) -> a
                noInfo cns = cns SrcSpanNoInfo

compactAST :: P.ModuleSpan -> Either [Located Error] (SymbolTableSpan, [Located Warning])
compactAST = handleEither . snd . flip runCompact def . compactModule
    where
        handleEither :: CompactState -> Either [Located Error] (SymbolTableSpan, [Located Warning])
        handleEither st = let errs = view cm_errors   st
                              tab  = view cm_table    st
                              wrns = view cm_warnings st
                          in if length errs > 0 then Left errs else Right (tab, wrns)

runCompact :: Compact a -> CompactState -> (a, CompactState)
runCompact = runState

----------------------------------------

compactModule :: P.ModuleSpan -> Compact ()
compactModule = compactBody . view P.mod_body

compactBody :: P.BodySpan -> Compact ()
compactBody = undefined
{-
compactBody :: P.BodySpan -> Compact ()
compactBody = mapM_ loadTableBodyStmt . view P.bod_stmts
    where
        loadTableBodyStmt :: P.BodyStmtSpan -> Compact ()
        loadTableBodyStmt stmt = case stmt of
                P.OpenType idn typ mcns an -> do
                        typ' <- compactExprWhere typ
                        let str = view idn_str idn
                            sym = SymbolType str typ' True

                        insertAndHandleSc str sym an
                        mapM_ (compactConstructor typ') (fromMaybe empty mcns)

                P.ReopenType idn cns an -> do
                        msym <- lookupAndHandleSc (view idn_str idn) an
                        forM_ msym $ \sym -> case preview sym_open sym of
                                Just True -> do
                                        let SymbolType _ typ _ = sym
                                        mapM_ (compactConstructor typ) cns
                                _ -> addCError (CErrExpectingInsteadOf "open type" (symbolStr sym)) an

                P.OperatorDef idn fix an -> do
                        let str = "operator " ++ view idn_str idn
                            sym = SymbolOperator str fix
                        insertAndHandleSc str sym an

compactExprWhere :: P.ExprWhereSpan -> Compact ExpressionSpan
compactExprWhere = compactExpression . P.whereToExpression

compactExpression :: P.ExpressionSpan -> Compact ExpressionSpan
compactExpression expr = bracketSc $ case expr of
        P.Var str an -> lookupAndHandleSc str an >> return (Var str an)
        P.Lit lit an -> return (Lit lit an)
        P.Apply xs an -> mapM compactExpression xs >>= return . foldr1 go
            where
                go :: ExpressionSpan -> ExpressionSpan -> ExpressionSpan
                go fn ov = let spn = (srcSpanSpan `on` view exp_annot) fn ov
                           in  Apply fn ov spn

        P.Lambda arg expr an -> do
                arg'  <- compactArgument arg
                expr' <- compactExpression expr
                return (Lambda arg' expr' an)

        P.Let bdy expr an -> do
                (tab', expr') <- bracketSc $ do
                        compactBody bdy
                        expr' <- compactExpression expr
                        tab <- use cm_table
                        return (tab, expr')
                return (Let tab' expr' an)

        P.Forall typs expr an -> bracketSc $ do
                typs' <- mapM compactTypeBind typs
                expr' <- compactExpression expr
                return (Forall (toTab typs') expr' an)

        P.Exists typ expr an -> do
                typ' <- compactTypeBind typ
                expr' <- compactExpression expr
                return (Exists (toTab [typ']) expr' an)

        P.Select typ an -> do
                typ' <- compactTypeBind typ
                return (Select typ' an)

        P.ImplicitExpr imps an -> do
                error "compactExpression: ImplicitExpr"
    where
        toTab :: (Foldable f, Functor f) => f TypeBindSpan -> SymbolTableSpan
        toTab = ST.fromFoldable . fmap convert
            where
                convert :: TypeBindSpan -> (String, SymbolSpan)
                convert bind = (str, sym)
                    where
                        str = view bind_name bind
                        sym = SymbolVar str (view bind_type bind) Nothing True


compactConstructor :: ExpressionSpan -> P.TypeBindSpan -> Compact ()
compactConstructor typ (P.TypeBind idn cns an) = do
        def <- compactExprWhere cns
        let str = view idn_str idn
            sym = SymbolFunction str typ empty True
        insertAndHandleSc str sym an

compactArgument :: P.ArgumentSpan -> Compact ExpressionSpan
compactArgument arg = case arg of
        P.VarBinding str an -> return (Var str an)
        P.ApplyBinding as an -> mapM compactArgument as >>= return . foldr1 go
            where
                go :: ExpressionSpan -> ExpressionSpan -> ExpressionSpan
                go fn ov = let spn = (srcSpanSpan `on` view exp_annot) fn ov
                           in  Apply fn ov spn
        P.DontCare an -> return (DontCare an)

compactTypeBind :: P.TypeBindSpan -> Compact TypeBindSpan
compactTypeBind (P.TypeBind idn typ an) = do
        typ' <- compactExprWhere typ
        let str = view idn_str idn
            sym = SymbolVar str typ'
        insertAndHandleSc str sym (view idn_annot idn)
        return (TypeBind str typ' an)
-}
