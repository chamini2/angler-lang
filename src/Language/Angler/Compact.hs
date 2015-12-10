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
import           Data.Foldable               (toList)
import           Data.Function               (on)
import           Data.Maybe                  (isJust, fromMaybe)
import           Data.Sequence               (Seq, drop)

import           Prelude                     hiding (drop)

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
                { _cm_table    = ST.fromFoldable [typeSym, arrowSym]
                , _cm_warnings = []
                , _cm_errors   = []
                }
            where
                typeSym :: (String, SymbolSpan)
                typeSym = (str, sym)
                    where
                        str = "Type"
                        sym = SymbolType str "Type" typeType True
                arrowSym :: (String, SymbolSpan)
                arrowSym = (str, sym)
                    where
                        str = "_->_"
                        sym = SymbolType str "Type" arrTyp False
                        arrTyp :: ExpressionSpan
                        arrTyp = arrExpr typeType (arrExpr typeType typeType)
                        arrExpr :: ExpressionSpan -> ExpressionSpan -> ExpressionSpan
                        arrExpr f t = Arrow f t SrcSpanNoInfo

compactAST :: P.ModuleSpan -> Either [Located Error] (SymbolTableSpan, [Located Warning])
compactAST = handleEither . snd . flip runCompact def . compactModule
    where
        handleEither :: CompactState -> Either [Located Error] (SymbolTableSpan, [Located Warning])
        handleEither st = let errs = view st_errors   st
                              tab  = view st_table    st
                              wrns = view st_warnings st
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
                        let sym = SymbolType str "Type" typ' True
                        insertAndHandleSc str sym an
                        mapM_ (compactConstructor str) (fromMaybe empty mcns)

                P.ReopenType idn cns an -> do
                        let str = view idn_str idn
                        msym <- lookupAndHandleSc str an
                        when (isJust msym) $ do
                                let Just sym = msym
                                if isSymType sym && (sym^?!sym_open)
                                        then mapM_ (compactConstructor (view sym_idn sym)) cns
                                        else addCErr (CErrExpectingInsteadOf "open type" (symbolStr sym)) an

                P.ClosedType idn typ cns an -> do
                        let str = view idn_str idn
                        typ' <- compactExprWhere typ
                        let sym = SymbolType str "Type" typ' False
                        insertAndHandleSc str sym an
                        mapM_ (compactConstructor str) cns

                P.FunctionDecl idn typ an -> do
                        let str = view idn_str idn
                        typ' <- compactExprWhere typ
                        let sym = SymbolFunction str typ' empty
                        insertAndHandleSc str sym an

                P.FunctionDef arg defn an -> do
                        let str = P.getHeadArgumentString arg
                        (args', defn') <- bracketSc $ do
                                args' <- compactFunctionArgument arg
                                defn' <- compactExprWhere defn
                                return (args', defn')
                        msym <- lookupAndHandleSc str an
                        when (isJust msym) $ do
                                let Just sym = msym
                                    sym' = over sym_defs (|> (args', defn')) sym
                                if isSymFunction sym
                                        then replaceSc str sym'
                                        else addCErr (CErrExpectingInsteadOf "function" (symbolStr sym)) an
                    where
                        compactFunctionArgument :: P.ArgumentSpan -> Compact (Seq ArgumentSpan)
                        compactFunctionArgument arg' = case arg' of
                                P.ApplyBinding args _ -> mapM compactArgument (drop 1 args)
                                _                     -> return empty

                P.OperatorDef {} -> return ()

compactConstructor :: String -> P.TypeBindSpan -> Compact ()
compactConstructor dat (P.TypeBind idn defn an) = do
        let str = view idn_str idn
        defn' <- compactExprWhere defn
        let sym = SymbolType str dat defn' False
        insertAndHandleSc str sym an

compactExprWhere :: P.ExprWhereSpan -> Compact ExpressionSpan
compactExprWhere = compactExpression . P.whereToExpression

compactExpression :: P.ExpressionSpan -> Compact ExpressionSpan
compactExpression = bracketSc . processExpression
    where
        processExpression :: P.ExpressionSpan -> Compact ExpressionSpan
        processExpression expr = case expr of
                P.Var "Type" an -> return (TypeType an)

                P.Var str an -> lookupAndHandleSc str an >> return (Var str an)

                P.Lit lit an -> return (Lit lit an)

                P.Apply xs _an -> case toList xs of
                        -- check for the specific case of arrow being used
                        [P.Var "_->_" van, arrfr, arrto] -> do
                                arrfr' <- processExpression arrfr
                                arrto' <- processExpression arrto
                                return (Arrow arrfr' arrto' van)
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
                        sym <- compactTypeBind typ
                        x' <- processExpression x
                        return (Exists sym x' an)

                P.Select typ an -> do
                        sym <- compactTypeBind typ
                        return (Select sym an)

                P.ImplicitExpr impls an -> bracketSc $ do
                        mapM_ compactImplicits impls
                        scope <- topSc
                        return (Implicit scope an)
                    where
                        compactImplicits :: P.ImplicitBindingSpan -> Compact ()
                        compactImplicits _ = error "implicit apply not supported"
                        -- compactImplicits (P.ImplicitBind idn x spn) = do
                                -- let str = view idn_str idn
                                -- x' <- compactExpression x
                                -- let sym = SymbolVar str Nothing (Just x') False
                                -- insertAndHandleSc str sym spn

compactTypeBind :: P.TypeBindSpan -> Compact SymbolSpan
compactTypeBind (P.TypeBind idn typ an) = do
        let str = view idn_str idn
        typ' <- compactExprWhere typ
        let sym = SymbolVar str typ' Nothing True
        insertAndHandleSc str sym an
        return sym

compactArgument :: P.ArgumentSpan -> Compact ArgumentSpan
compactArgument arg' = case arg' of
        P.DontCare an -> return (DontCare an)
        P.VarBinding str an -> do
                let sym = SymbolVar str dontCare Nothing True
                _ <- insertSc str sym
                return (Var str an)
        P.ApplyBinding args _an -> mapM compactArgument args >>= return . foldl1 go
            where
                go :: ArgumentSpan -> ArgumentSpan -> ArgumentSpan
                go fn ov = let spn = (srcSpanSpan `on` view exp_annot) fn ov
                           in Apply fn ov spn
