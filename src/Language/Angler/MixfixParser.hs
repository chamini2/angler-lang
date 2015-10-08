{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Angler.MixfixParser where

import           Language.Angler.AST
import           Language.Angler.Error       hiding (ParseError)
import           Language.Angler.Monad
import           Language.Angler.ScopedTable hiding (empty)
import qualified Language.Angler.ScopedTable as ST (empty)
import           Language.Angler.SrcLoc

import           Control.Applicative         (Alternative(..), (<|>), {-(<*>), (<*), (*>),-} liftA, many, {-some-})
import           Control.Lens                hiding (op, below, parts)
import           Control.Monad.State         (State, runState)
import           Control.Monad               (forM_)

import           Data.Default
import           Data.Foldable               (toList)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map (lookup)
import           Data.Sequence               (fromList)

import           Text.Megaparsec             (ParsecT, choice, eof, runParserT,
                                              token, try)
import           Text.Megaparsec.Pos         (SourcePos(..), setSourceName,
                                              setSourceLine, setSourceColumn)
import           Text.Megaparsec.ShowToken   (ShowToken(..))
import           Text.Megaparsec.Error       (ParseError, Message(..),
                                              errorMessages, errorPos)

import           Prelude                     hiding (lookup)

--------------------------------------------------------------------------------
-- Operator handling

type OperatorPart    = Maybe String                     -- Nothing represents hole
type OperatorParts   = [ OperatorPart ]
type PrecedenceLevel = Map (Fixity ()) [ OperatorParts ]
type PrecedenceTable = [ PrecedenceLevel ]

opStr :: OperatorParts -> String
opStr = concatMap (maybe "_" id)

strOp :: String -> OperatorParts
strOp = foldr go []
    where
        go :: Char -> OperatorParts -> OperatorParts
        go c ps = case (c,ps) of
                ('_', _           ) -> Nothing    : ps
                ( _ , Just p : ps') -> Just (c:p) : ps'
                ( _ , _           ) -> Just [c]   : ps

--------------------------------------------------------------------------------
-- State

data Operator
  = Operator
        { _op_idn       :: String
        -- , _op_repr      :: OperatorParts
        , _op_fix       :: Fixity ()
        , _op_prec      :: Maybe Int
        }

data OpPState
  = OpPState
        { _opp_table     :: ScopedTable Operator
        , _opp_errs      :: [ Located Error ]
        }

makeLenses ''Operator
makeLenses ''OpPState

instance STScopedTable OpPState Operator where
        st_table = opp_table

instance Default OpPState where
        def = OpPState
                { _opp_table = ST.empty
                , _opp_errs  = []
                }

scopedTabPrecedenceTab :: ScopedTable Operator -> PrecedenceTable
scopedTabPrecedenceTab = undefined

--------------------------------------------------------------------------------
-- Monad

type Mixfix = State OpPState

runMixfix :: Mixfix SrcSpan -> (SrcSpan, OpPState)
runMixfix = flip runState def

mixfixModule :: ModuleSpan -> Mixfix ModuleSpan
mixfixModule = mapMOf mod_body mixfixBody

mixfixBody :: BodySpan -> Mixfix BodySpan
mixfixBody = mapMOf traverse mixfixBodyStmt

mixfixBodyStmt :: BodyStmtSpan -> Mixfix BodyStmtSpan
mixfixBodyStmt stmt = case stmt of
        OpenType {} -> do
                stmt' <- mapMOf (open_cnts._Just.traverse) mixfixTypeBind stmt
                mapMOf open_type mixfixWhere stmt'
        ReopenType {} -> mapMOf (rpen_cnts.traverse) mixfixTypeBind stmt
        ClosedType {} -> do
                stmt'  <- mapMOf clsd_type mixfixWhere stmt
                mapMOf (clsd_cnts.traverse) mixfixTypeBind stmt'
        FunctionDecl {} -> mapMOf fdec_type mixfixWhere stmt
        FunctionDef {} -> do
                stmt' <- mapMOf (fdef_args.traverse) mixfixArgument stmt
                mapMOf fdef_expr mixfixWhere stmt'
        OperatorDef idn fix mprec spn -> do
                let idn' = view idn_str idn
                    fix' = set fix_annot () fix
                merr <- safeInsertSc idn' (Operator idn' fix' mprec)
                case merr of
                        Just err -> pushM opp_errs (Loc spn err)
                        _        -> return ()
                return stmt

mixfixWhere :: ExprWhereSpan -> Mixfix (ExprWhereSpan)
mixfixWhere whre = do
        whre' <- mapMOf (whre_body._Just) mixfixBody whre
        mapMOf whre_insd mixfixExpression whre'

mixfixExpression :: ExpressionSpan -> Mixfix ExpressionSpan
mixfixExpression expr = parseExpression expr >>= either leftPError return
    where
        leftPError :: ParseError -> Mixfix ExpressionSpan
        leftPError perr = do
                let msgs = errorMessages perr
                    pos  = errorPos perr
                    loc  = SrcLoc (sourceName pos) (sourceLine pos) (sourceColumn pos)
                    spn  = srcLocSpan loc loc
                forM_ msgs (pushM opp_errs . Loc spn . CheckError . CErr . show)
                return expr


mixfixTypeBind :: TypeBindSpan -> Mixfix TypeBindSpan
mixfixTypeBind = return

mixfixArgument :: ArgumentSpan -> Mixfix ArgumentSpan
mixfixArgument = return

mixfixImplicit :: ImplicitBindingSpan -> Mixfix ImplicitBindingSpan
mixfixImplicit = return

--------------------------------------------------------------------------------
-- Parser

type ExprSpan = ExpressionSpan
type OpP = ParsecT [ExprSpan] Mixfix

parseExpression :: ExprSpan -> Mixfix (Either ParseError ExprSpan)
parseExpression expr = case expr of
        Var _ _ -> return (Right expr)
        Lit _ _ -> return (Right expr)
        Apply xs _ -> mapM mixfixExpression xs >>= parse . toList
        Lambda arg x ann -> do
                arg' <- mixfixArgument arg
                x' <- mixfixExpression x
                return (Right (Lambda arg' x' ann))
        Let body x ann -> do
                body' <- mixfixBody body
                x' <- mixfixExpression x
                return (Right (Let body' x' ann))
        Forall typs x ann -> do
                typs' <- mapM mixfixTypeBind typs
                x' <- mixfixExpression x
                return (Right (Forall typs' x' ann))
        Exists typ x ann -> do
                typ' <- mixfixTypeBind typ
                x' <- mixfixExpression x
                return (Right (Exists typ' x' ann))
        Select typ ann -> do
                typ' <- mixfixTypeBind typ
                return (Right (Select typ' ann))
        ImplicitExpr imps ann -> do
                imps' <- mapM mixfixImplicit imps
                return (Right (ImplicitExpr imps' ann))
    where
        parse :: [ExprSpan] -> Mixfix (Either ParseError ExprSpan)
        parse = runOpP generateOpP (views exp_annot srcSpanFile expr)

runOpP :: OpP a -> FilePath -> [ExprSpan] -> Mixfix (Either ParseError a)
runOpP = runParserT

instance Show a => ShowToken (Expression a) where
        showToken = prettyShow

instance ShowToken a => ShowToken [a] where
        showToken = show

satisfy' :: (ExprSpan -> Either [Message] ExprSpan) -> OpP ExprSpan
satisfy' = token nextPos
    where
        nextPos :: Int -> SourcePos -> ExprSpan -> SourcePos
        nextPos _tab p x = (setName . setLine . setColumn) p
            where
                xSpan :: SrcSpan
                xSpan = view exp_annot x
                setName :: SourcePos -> SourcePos
                setName = flip setSourceName (srcSpanFile xSpan)
                setLine :: SourcePos -> SourcePos
                setLine = flip setSourceLine (srcSpanSLine xSpan)
                setColumn :: SourcePos -> SourcePos
                setColumn = flip setSourceColumn (srcSpanSCol xSpan)

satisfy :: (ExprSpan -> Bool) -> OpP ExprSpan
satisfy g = satisfy' testExpr
    where
        testExpr :: ExprSpan -> Either [Message] ExprSpan
        testExpr x = if g x
                then Right x
                else (Left . return . Unexpected . showToken) x

var :: String -> OpP ExprSpan
var str = satisfy testExpr
    where
        testExpr :: ExprSpan -> Bool
        testExpr x = case x of
                Var name _ -> name == str
                _          -> False

generateOpP :: OpP ExprSpan
generateOpP = topOpP <* eof
    where
        precTable :: OpP PrecedenceTable
        precTable = uses opp_table scopedTabPrecedenceTab

        topOpP :: OpP ExprSpan
        topOpP = precTable >>= choice . foldr go [bottomOpP] . fmap pOpP
            where
                -- We pass all the parsers below as *fallback*
                -- if we couldn't get a match on this level
                go :: ([OpP ExprSpan] -> OpP ExprSpan) -> [OpP ExprSpan] -> [OpP ExprSpan]
                go pp acts = try (pp acts) : acts

        bottomOpP :: OpP ExprSpan
        bottomOpP = do
                xs <- liftA fromList cleverTokens
                let xsSpan = srcSpanSpan (xs^?!_head.exp_annot) (xs^?!_last.exp_annot)
                return (Apply xs xsSpan)
            where
                -- This tries to get a basic token, if we couldn't get even one,
                -- it gets any token, and then continues with the basic tokens,
                -- with 'if if a then b else c' it parses 'if_then_else_ (if a) b c'
                -- instead of giving a parse error
                -- This behaviour may or may not be recommended
                cleverTokens :: OpP [ExprSpan]
                cleverTokens = cons <$> (try basicToken <|> anyToken) <*> many basicToken

                anyToken :: OpP ExprSpan
                anyToken = satisfy (const True)

                basicToken :: OpP ExprSpan
                basicToken = try obviousToken <|> closedOpOpP
                    where
                        obviousToken :: OpP ExprSpan
                        obviousToken = precTable >>= satisfy . testExpr
                            where
                                testExpr :: PrecedenceTable -> ExprSpan -> Bool
                                testExpr prec x = case x of
                                        Var name _ -> name `notElem` (parts prec)
                                        _          -> True
                                parts :: PrecedenceTable -> [String]
                                parts = toListOf (traverse.traverse.traverse.traverse._Just)

                closedOpOpP :: OpP ExprSpan
                closedOpOpP = precTable >>= choice . fmap (try . opOpP) . closedOps
                    where
                        opOpP :: OperatorParts -> OpP ExprSpan
                        opOpP op = do
                                clxs <- closedPartOpP op
                                let clsp = srcSpanSpan (clxs^?!_head.exp_annot) (clxs^?!_last.exp_annot)
                                return (Apply (Var (opStr op) clsp <| clxs) clsp)
                        closedOps :: PrecedenceTable -> [ OperatorParts ]
                        closedOps = concatMap (maybe [] id . Map.lookup (Closedfix ()))

        pOpP :: PrecedenceLevel -> [OpP ExprSpan] -> OpP ExprSpan
        pOpP _lvl below = choice below

        closedPartOpP :: Alternative f => OperatorParts -> OpP (f ExprSpan)
        closedPartOpP = foldr go (pure empty)
            where
                go :: Alternative f => OperatorPart -> OpP (f ExprSpan) -> OpP (f ExprSpan)
                go mprt act = (<|>) <$> prtOpP mprt <*> act
                prtOpP :: Alternative f => OperatorPart -> OpP (f ExprSpan)
                prtOpP mprt = case mprt of
                        Just prt -> pure empty <* var prt
                        _        -> pure <$> topOpP
