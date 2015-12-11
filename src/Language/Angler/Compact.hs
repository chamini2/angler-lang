{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Angler.Compact
        ( compactAST ) where

import qualified Language.Angler.Program     as P
import           Language.Angler.AST
import qualified Language.Angler.ScopedTable as ST
import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.SrcLoc

import           PrettyShow

import           Control.Applicative         (empty)

import           Control.Lens

import           Control.Monad               (void, when)
import           Control.Monad.State         (State, runState)

import           Data.Default                (Default(..))
import           Data.Foldable               (foldlM, toList)
import           Data.Function               (on)
import           Data.Maybe                  (isJust, isNothing, fromMaybe)
import           Data.Sequence               (Seq, drop)

import           Debug.Trace

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
                { _cm_table    = ST.emptyWithIndefinable ["Type", "_->_"]
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
                        (typ', typTyp) <- compactExprWhere typ
                        let sym = SymbolType str "Type" typ' True
                        insertAndHandleSc str sym an
                        mapM_ (compactConstructor str) (fromMaybe empty mcns)

                P.ReopenType idn cns an -> do
                        let str   = view idn_str idn
                            check = ("open type", \s -> isSymType s && (s^?!sym_open), symbolStr, an)
                        mSym <- lookupAndHandleSc check str
                        when (isJust mSym) $ do
                                let Just sym = mSym
                                mapM_ (compactConstructor (view sym_idn sym)) cns

                P.ClosedType idn typ cns an -> do
                        let str = view idn_str idn
                        (typ', typTyp) <- compactExprWhere typ
                        let sym = SymbolType str "Type" typ' False
                        insertAndHandleSc str sym an
                        mapM_ (compactConstructor str) cns

                P.FunctionDecl idn typ an -> do
                        let str = view idn_str idn
                        (typ', typTyp) <- compactExprWhere typ
                        let sym = SymbolFunction str typ' empty
                        insertAndHandleSc str sym an

                P.FunctionDef arg defn an -> do
                        let str   = P.getHeadArgumentString arg
                            check = ("function", isSymFunction, symbolStr, an)
                        mSym <- lookupAndHandleSc check str
                        when (isJust mSym) $ do
                                let Just sym = mSym
                                    fnTyp    = view sym_type sym
                                (args', defn') <- bracketSc $ do
                                        (args', fnTyp')  <- compactFunctionArgument str arg fnTyp
                                        (defn', defnTyp) <- compactExprWhere defn
                                        let t = trace (str ++ " (" ++ show (length args') ++ "): " ++ prettyShow fnTyp ++ " ~~> "++ prettyShow fnTyp')
                                        unify an defnTyp (t fnTyp')
                                        return (args', defn')

                                let sym' = over sym_defs (|> (args', defn')) sym
                                replaceSc str sym'
                    where
                        compactFunctionArgument :: String -> P.ArgumentSpan -> TypeSpan
                                                -> Compact (Seq ArgumentSpan, TypeSpan)
                        compactFunctionArgument fnIdn arg typ = case arg of
                                P.ApplyBinding args _ -> foldlM applyArg (empty, typ) (drop 1 args)
                                _ -> return (empty, typ)
                            where
                                applyArg :: (Seq ArgumentSpan, TypeSpan) -> P.ArgumentSpan
                                        -> Compact (Seq ArgumentSpan, TypeSpan)
                                applyArg (args, fnTyp) arg = do
                                        (arg', argTyp) <- compactArgument arg
                                        let stSpn = fromMaybe SrcSpanNoInfo (preview (_head.exp_annot) args)
                                            spn   = srcSpanSpan stSpn (view exp_annot arg')
                                        typ <- argType spn fnTyp argTyp
                                        return (args |> arg', typ)
                                argType :: SrcSpan -> TypeSpan -> TypeSpan -> Compact TypeSpan
                                argType spn fn ov = case fn of
                                        Arrow fr to an -> unify spn ov fr >> return to
                                        Forall typs ex an -> bracketScWith typs $ do
                                                typ <- argType spn ex ov
                                                scope <- topSc
                                                return (Forall scope typ an)
                                        _ -> addCErr (CErrTooManyArguments fnIdn) spn >> return fn

                P.OperatorDef {} -> return ()

compactConstructor :: String -> P.TypeBindSpan -> Compact ()
compactConstructor dat (P.TypeBind idn defn an) = do
        let str = view idn_str idn
        (defn', defnTyp) <- compactExprWhere defn
        let sym = SymbolType str dat defn' False
        insertAndHandleSc str sym an

compactExprWhere :: P.ExprWhereSpan -> Compact (ExpressionSpan, TypeSpan)
compactExprWhere = compactExpression . P.whereToExpression

compactExpression :: P.ExpressionSpan -> Compact (ExpressionSpan, TypeSpan)
compactExpression = bracketSc . processExpression
    where
        processExpression :: P.ExpressionSpan -> Compact (ExpressionSpan, TypeSpan)
        processExpression expr = case expr of
                -- check for the specific case of Type being used
                P.Var "Type" an -> return (TypeType an, typeType)

                P.Var str an -> do
                        let check = ("", const True, symbolStr, an)
                        mSym <- lookupAndHandleSc check str
                        let typ = maybe dontCare (view sym_type) mSym
                        return (Var str an, typ)

                P.Lit lit an -> return (Lit lit an, litTyp)
                    where
                        litTyp :: TypeSpan
                        litTyp = case lit of
                                LitNat    {} -> strVar "Nat"
                                LitChar   {} -> strVar "Char"
                                LitString {} -> strVar "String"
                            where
                                strVar :: String -> TypeSpan
                                strVar = flip Var SrcSpanNoInfo

                P.Apply xs _an -> case toList xs of
                        -- check for the specific case of _->_ being used
                        [P.Var "_->_" van, arrFr, arrTo] -> do
                                (arrFr', arrFrTyp) <- processExpression arrFr
                                fitify (view exp_annot arrFr') arrFrTyp typeType

                                (arrTo', arrToTyp) <- processExpression arrTo
                                fitify (view exp_annot arrTo') arrToTyp typeType

                                return (Arrow arrFr' arrTo' van, typeType)

                        _ -> do
                                let fn = xs ^?! _head
                                    as = xs ^?! _tail
                                applyFunction fn as processExpression
                                -- (fn', fnTyp) <- processExpression fn
                                -- foldlM applyFn (fn', fnTyp) as
                    where
                        applyFn :: (ExpressionSpan, TypeSpan)
                                -> P.ExpressionSpan
                                -> Compact (ExpressionSpan, TypeSpan)
                        applyFn (fn, fnTyp) ex = do
                                (ex', exTyp) <- processExpression ex
                                let spn = (srcSpanSpan `on` view exp_annot) fn ex'
                                typ <- applyType spn fnTyp exTyp
                                return (Apply fn ex' spn, typ)
                        applyType :: SrcSpan -> TypeSpan -> TypeSpan -> Compact TypeSpan
                        applyType spn fn ov = case fn of
                                Arrow fr to _ -> fitify spn ov fr >> return to
                                Forall typs ex an -> bracketScWith typs $ do
                                        typ <- applyType spn ex ov
                                        scope <- topSc
                                        return (Forall scope typ an)
                                _ -> do
                                        addCErr (CErrCannotApply (prettyShow fn) (prettyShow ov)) spn
                                        return fn
                --         _ -> mapM processExpression xs >>= return . foldl1 go
                --     where
                --         go :: ExpressionSpan -> ExpressionSpan -> ExpressionSpan
                --         go fn ov = let spn = (srcSpanSpan `on` view exp_annot) fn ov
                --                    in Apply fn ov spn

                P.Lambda arg x an -> bracketSc $ do
                        (arg', argTyp) <- compactArgument arg
                        (x', xTyp) <- processExpression x
                        let typ = Arrow argTyp xTyp an
                        return (Lambda arg' x' an, typ)

                P.Let bod x an -> bracketSc $ do
                        compactBody bod
                        (x', xTyp) <- processExpression x
                        scope <- topSc
                        return (Let scope x' an, error "type")

                P.Forall typs x an -> bracketSc $ do
                        mapM_ compactTypeBind typs
                        (x', xTyp) <- processExpression x
                        scope <- topSc
                        return (Forall scope x' an, error "type")

                P.Exists typ x an -> bracketSc $ do
                        sym <- compactTypeBind typ
                        (x', xTyp) <- processExpression x
                        return (Exists sym x' an, error "type")

                P.Select typ an -> do
                        sym <- compactTypeBind typ
                        return (Select sym an, error "type")

                P.ImplicitExpr impls an -> bracketSc $ do
                        mapM_ compactImplicits impls
                        scope <- topSc
                        return (Implicit scope an, error "type")
                    where
                        compactImplicits :: P.ImplicitBindingSpan -> Compact ()
                        compactImplicits (P.ImplicitBind idn x spn) = do
                                let str = view idn_str idn
                                (x', xTyp) <- compactExpression x
                                let sym = SymbolVar str dontCare (Just x') False sym
                                insertAndHandleSc str sym spn

compactTypeBind :: P.TypeBindSpan -> Compact SymbolSpan
compactTypeBind (P.TypeBind idn typ an) = do
        let str = view idn_str idn
        (typ', typTyp) <- compactExprWhere typ
        let sym = SymbolVar str typ' Nothing True sym
        insertAndHandleSc str sym an
        return sym

applyFunction :: forall f x . Foldable f => x -> f x -> (x -> Compact (ExpressionSpan, TypeSpan))
              -> Compact (ExpressionSpan, TypeSpan)
applyFunction fn args compactFn = compactFn fn >>= \fnInfo -> foldlM go fnInfo args
    where
        go :: (ExpressionSpan, TypeSpan) -> x -> Compact (ExpressionSpan, TypeSpan)
        go (fn, fnTyp) x = do
                (x', xTyp) <- compactFn x
                let spn = (srcSpanSpan `on` view exp_annot) fn x'
                typ <- applyType spn fnTyp xTyp
                return (Apply fn x' spn, typ)
        applyType :: SrcSpan -> TypeSpan -> TypeSpan -> Compact TypeSpan
        applyType spn fn ov = case fn of
                Arrow from to _ -> fitify spn ov from >> return to
                Forall typs ex an -> bracketScWith typs $ do
                        typ <- applyType spn ex ov
                        scope <- topSc
                        return (Forall scope typ an)
                _ -> return fn <* addCErr (CErrCannotApply (prettyShow fn) (prettyShow ov)) spn

compactArgument :: P.ArgumentSpan -> Compact (ArgumentSpan, TypeSpan)
compactArgument arg' = case arg' of
        P.DontCare an -> return (DontCare an, dontCare)
        P.VarBinding str an -> do
                let sym = SymbolVar str dontCare Nothing True sym
                mSym <- lookupSc str
                when (isNothing mSym) $ void (insertSc str sym)
                let varTyp = maybe dontCare (view sym_type) mSym
                return (Var str an, varTyp)
        P.ApplyBinding args _an -> do
                let fn = args ^?! _head
                    as = args ^?! _tail
                applyFunction fn as compactArgument
                -- (fn', fnTyp) <- compactArgument fn
                -- foldlM applyFn (fn', fnTyp) as
            where
                applyFn :: (ArgumentSpan, TypeSpan)
                        -> P.ArgumentSpan
                        -> Compact (ArgumentSpan, TypeSpan)
                applyFn (fn, fnTyp) ex = do
                        (ex', exTyp) <- compactArgument ex
                        let spn = (srcSpanSpan `on` view exp_annot) fn ex'
                        typ <- applyType spn fnTyp exTyp
                        return (Apply fn ex' spn, typ)
                applyType :: SrcSpan -> TypeSpan -> TypeSpan -> Compact TypeSpan
                applyType spn fn ov = case fn of
                        Arrow fr to _ -> fitify spn ov fr >> return to
                        Forall typs ex an -> bracketScWith typs $ do
                                typ <- applyType spn ex ov
                                scope <- topSc
                                return (Forall scope typ an)
                        _ -> do
                                addCErr (CErrCannotApply (prettyShow fn) (prettyShow ov)) spn
                                return fn
        -- P.ApplyBinding args _an -> mapM compactArgument args >>= return . foldl1 go
        --     where
        --         go :: ArgumentSpan -> ArgumentSpan -> ArgumentSpan
        --         go fn ov = let spn = (srcSpanSpan `on` view exp_annot) fn ov
        --                    in Apply fn ov spn

--------------------------------------------------------------------------------
-- typechecking algorithms

-- bilateral unification
unify :: SrcSpan -> ExpressionSpan -> ExpressionSpan -> Compact ()
unify spn exl exr = case (exl, exr) of
        (Forall typs ex an, _) -> bracketScWith typs $ unify spn ex exr
        (_, Forall typs ex an) -> bracketScWith typs $ unify spn exl ex

        (DontCare _, _) -> return ()
        (_, DontCare _) -> return ()

        (Var strl _, Var strr _) -> do
                mSyml <- lookupSc strl
                mSymr <- lookupSc strr

                when (all isJust [mSyml, mSymr]) $ do
                        let Just syml = symFind <$> mSyml
                            Just symr = symFind <$> mSymr

                        let mSymPair = symUnion syml symr
                        if isJust mSymPair
                                then do
                                        let Just (syml', symr') = mSymPair
                                        replaceSc (view sym_idn syml') syml'
                                        replaceSc (view sym_idn symr') symr'
                                else addCErr ((CErrTypeError `on` view sym_idn) syml symr) spn

        (Var strl _, exr) -> handleVarSomething strl exr
        (exl, Var strr _) -> handleVarSomething strr exl

        (Apply fnl ovl _, Apply fnr ovr _) -> unify spn fnl fnr >> unify spn ovl ovr

        (Arrow frl tol _, Arrow frr tor _) -> unify spn frl frr >> unify spn tol tor

        (TypeType _, TypeType _) -> return ()

        _ -> typeError

    where
        handleVarSomething :: String -> ExpressionSpan -> Compact ()
        handleVarSomething str ex = do
                mSym <- lookupSc str
                when (isJust mSym) $ do
                        let Just sym = symFind <$> mSym
                        -- unify spn (view sym_type sym) ex
                        case sym of
                                SymbolVar _ _ Nothing _ _ -> do
                                        let sym' = set sym_value (Just exr) sym
                                        replaceSc (view sym_idn sym) sym'
                                SymbolVar _ _ (Just varEx) _ _ -> unify spn varEx ex

                                _ -> typeError
        typeError :: Compact ()
        typeError = addCErr (CErrTypeError (prettyShow exl) (prettyShow exr)) spn

-- unilateral unification, left fits in right
fitify :: SrcSpan -> ExpressionSpan -> ExpressionSpan -> Compact ()
fitify = unify
