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

import           Control.Monad               (void, when)
import           Control.Monad.State         (State, runState)

import           Data.Default                (Default(..))
import           Data.Foldable               (forM_, toList)
import           Data.Function               (on)
import           Data.Maybe                  (isJust, isNothing, fromMaybe)
import           Data.Sequence               (Seq, drop)

import           Prelude                     hiding (drop)
import           Debug.Trace

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
                        sym = SymbolType str typeType True
                arrowSym :: (String, SymbolSpan)
                arrowSym = (str, sym)
                    where
                        str = "_->_"
                        sym = SymbolConstructor str "Type" arrTyp
                        arrTyp :: ExpressionSpan
                        arrTyp = arrExpr typeType (arrExpr typeType typeType)
                typeType :: TypeSpan
                typeType = TypeType SrcSpanNoInfo
                arrExpr :: ExpressionSpan -> ExpressionSpan -> ExpressionSpan
                arrExpr f t = Arrow f t SrcSpanNoInfo

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
compactBody = mapM_ loadTableBodyStmt . view P.bod_stmts
    where
        loadTableBodyStmt :: P.BodyStmtSpan -> Compact ()
        loadTableBodyStmt stmt = case stmt of
                P.OpenType idn typ mcns an -> do
                        let str = view idn_str idn
                        typ' <- compactExprWhere typ
                        let sym = SymbolType str typ' True
                        insertAndHandleSc str sym an
                        mapM_ (compactConstructor str) (fromMaybe empty mcns)

                P.ReopenType idn cns an -> do
                        let str = view idn_str idn
                        msym <- lookupAndHandleSc str an
                        when (isJust msym) $ do
                                let Just sym = msym
                                if isSymType sym && (sym^?!sym_open)
                                        then mapM_ (compactConstructor (view sym_idn sym)) cns
                                        else addCError (CErrExpectingInsteadOf "open type" (symbolStr sym)) an

                P.ClosedType idn typ cns an -> do
                        let str = view idn_str idn
                        typ' <- compactExprWhere typ
                        let sym = SymbolType str typ' False
                        insertAndHandleSc str sym an
                        mapM_ (compactConstructor str) cns

                P.FunctionDecl idn typ an -> do
                        let str = view idn_str idn
                        typ' <- compactExprWhere typ
                        let sym = SymbolFunction str typ' empty
                        insertAndHandleSc str sym an

                P.FunctionDef arg def an -> do
                        let str = P.getHeadArgumentString arg
                        (args', def') <- bracketSc $ do
                                args' <- compactFunctionArgument arg
                                def' <- compactExprWhere def
                                return (args', def')
                        msym <- lookupAndHandleSc str an
                        when (isJust msym) $ do
                                let Just sym = msym
                                    sym' = over sym_defs (|> (args', def')) sym
                                if isSymFunction sym
                                        then replaceSc str sym'
                                        else addCError (CErrExpectingInsteadOf "function" (symbolStr sym)) an
                    where
                        compactFunctionArgument :: P.ArgumentSpan -> Compact (Seq ArgumentSpan)
                        compactFunctionArgument arg = case arg of
                                P.ApplyBinding args _ -> mapM compactArgument (drop 1 args)
                                _                     -> return empty

                P.OperatorDef {} -> return ()

compactConstructor :: String -> P.TypeBindSpan -> Compact ()
compactConstructor dat (P.TypeBind idn def an) = do
        let str = view idn_str idn
        def' <- compactExprWhere def
        let sym = SymbolConstructor str dat def'
        insertAndHandleSc str sym an

compactExprWhere :: P.ExprWhereSpan -> Compact ExpressionSpan
compactExprWhere = compactExpression . P.whereToExpression

compactExpression :: P.ExpressionSpan -> Compact ExpressionSpan
compactExpression = bracketSc . processExpression
    where
        processExpression :: P.ExpressionSpan -> Compact ExpressionSpan
        processExpression expr = case expr of
                P.Var str an -> lookupAndHandleSc str an >> return (Var str an)

                P.Lit lit an -> return (Lit lit an)

                P.Apply xs an -> case toList xs of
                        -- check for the specific case of arrow being used
                        [P.Var "_->_" an, fr, to] -> do
                                fr' <- processExpression fr
                                to' <- processExpression to
                                return (Arrow fr' to' an)
                        _ -> mapM processExpression xs >>= return . foldl1 go
                    where
                        go :: ExpressionSpan -> ExpressionSpan -> ExpressionSpan
                        go fn ov = let spn = (srcSpanSpan `on` view exp_annot) fn ov
                                   in Apply fn ov spn

                P.Lambda arg x an -> bracketSc $ do
                        arg' <- compactArgument arg
                        x' <- processExpression x
                        return (Lambda arg' x' an)

                P.Let bod x an -> bracketSc $ do
                        compactBody bod
                        x' <- processExpression x
                        scope <- topSc
                        return (Let scope x' an)

                P.Forall typs x an -> bracketSc $ do
                        mapM_ compactTypeBind typs
                        x' <- processExpression x
                        scope <- topSc
                        return (Forall scope x' an)

                P.Exists typ x an -> bracketSc $ do
                        compactTypeBind typ
                        x' <- processExpression x
                        scope <- topSc
                        return (Exists scope x' an)

                P.Select typ an -> do
                        sym <- compactTypeBind typ
                        return (Select sym an)

                P.ImplicitExpr impls an -> bracketSc $ do
                        mapM_ compactImplicits impls
                        scope <- topSc
                        return (Implicit scope an)
                    where
                        compactImplicits :: P.ImplicitBindingSpan -> Compact ()
                        compactImplicits (P.ImplicitBind idn x an) = do
                                let str = view idn_str idn
                                x' <- compactExpression x
                                let sym = SymbolVar str Nothing (Just x') False
                                insertAndHandleSc str sym an

compactTypeBind :: P.TypeBindSpan -> Compact SymbolSpan
compactTypeBind (P.TypeBind idn typ an) = do
        let str = view idn_str idn
        typ' <- compactExprWhere typ
        let sym = SymbolVar str (Just typ') Nothing True
        insertAndHandleSc str sym an
        return sym

compactArgument :: P.ArgumentSpan -> Compact ArgumentSpan
compactArgument arg' = case arg' of
        P.DontCare an -> return (DontCare an)
        P.VarBinding str an -> do
                let sym = SymbolVar str Nothing Nothing True
                msym <- lookupSc str
                when (isNothing msym) $ void (insertSc str sym)
                return (Var str an)
        P.ApplyBinding args an -> mapM compactArgument args >>= return . foldl1 go
            where
                go :: ArgumentSpan -> ArgumentSpan -> ArgumentSpan
                go fn ov = let spn = (srcSpanSpan `on` view exp_annot) fn ov
                           in Apply fn ov spn
