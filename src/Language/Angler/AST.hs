{-# LANGUAGE TemplateHaskell #-}
module Language.Angler.AST where

import           Language.Angler.SrcLoc

import           Control.Lens
import           Control.Monad           (unless, when)
import           Control.Monad.State     (State, execState)
import           Data.Sequence           (Seq)
import           Data.Maybe              (isJust)

import           Data.List               (intercalate)
import           Data.Foldable           (toList)

data Identifier a
  = Identifier
        { _idn_str      :: String
        , _idn_annot    :: a
        }
  deriving Show
type IdentifierSpan = Identifier SrcSpan

data Module a
  = Module
        { _mod_exports  :: Maybe (Seq (Identifier a))   -- 'Nothing' means to export everything
        , _mod_imports  :: Seq (Import a)
        , _mod_body     :: Body a
        , _mod_annot    :: a
        }
  deriving Show
type ModuleSpan = Module SrcSpan

data Import a
  = Import
        { _impr_path    :: FilePath
        , _impr_as      :: Maybe (Identifier a)
        , _impr_spcfc   :: Maybe (Seq (Identifier a))   -- 'Nothing' means to import everything
        , _impr_annot   :: a
        }
  deriving Show
type ImportSpan = Import SrcSpan

type Body a = Seq (BodyStmt a)
type BodySpan = Body SrcSpan

data BodyStmt a
  = FunctionDecl
        { _fdec_id      :: Identifier a
        , _fdec_type    :: Where a (Expression a)
        , _stm_annot    :: a
        }
  | OpenType
        { _open_id      :: Identifier a
        , _open_type    :: Where a (Expression a)
        , _open_cons    :: Maybe (Seq (TypeDecl a))
        , _stm_annot    :: a
        }
  | ReopenType
        { _rpen_id      :: Identifier a
        , _rpen_cons    :: Seq (TypeDecl a)
        , _stm_annot    :: a
        }
  | ClosedType
        { _clsd_id      :: Identifier a
        , _clsd_type    :: Where a (Expression a)
        , _clsd_cons    :: Seq (TypeDecl a)
        , _stm_annot    :: a
        }
  | FunctionDef
        { _fdef_args    :: Seq (Argument a)
        , _fdef_expr    :: Where a (Expression a)
        , _stm_annot    :: a
        }
  deriving Show
type BodyStmtSpan = BodyStmt SrcSpan

data Where a f
  = Where
        { _whre_body    :: Body a
        , _whre_insd    :: f
        , _whre_annot   :: a
        }
  deriving Show
type WhereSpan f = Where SrcSpan f

data Expression a
  = Var
        { _var_str      :: String
        , _exp_annot    :: a
        }
  | Lit
        { _val_lit      :: Literal a
        , _exp_annot    :: a
        }
  | Application
        { _app_exprs    :: Seq (Expression a)
        , _exp_annot    :: a
        }
  | Lambda
        { _lam_args     :: Seq (Argument a)
        , _exp_annot    :: a
        }
  | Forall
        { _fall_typs    :: Seq (TypeDecl a)
        , _exp_annot    :: a
        }
  | Exists
        { _exst_type    :: TypeDecl a
        , _exst_expr    :: Expression a
        , _exp_annot    :: a
        }
  | With
        { _with_type    :: TypeDecl a
        , _exp_annot    :: a
        }
  | ImplicitExpr
        { _impl_exprs   :: Implicits a
        , _exp_annot    :: a
        }
  deriving Show
type ExpressionSpan = Expression SrcSpan

data TypeDecl a
  = TypeDecl
        { _typ_id       :: Identifier a
        , _typ_type     :: Where a (Expression a)
        , _typ_annot    :: a
        }
  deriving Show
type TypeDeclSpan = TypeDecl SrcSpan

data Argument a
  = Binding
        { _bind_id      :: Identifier a
        , _arg_annot    :: a
        }
  | DontCare
        { _arg_annot    :: a }
  | ParenthesizedBinding
        { _ptrn_expr    :: Seq (Argument a)
        , _arg_annot    :: a
        }
  deriving Show
type ArgumentSpan = Argument SrcSpan

type Implicits a = Seq (ImplicitBinding a)
type ImplicitsSpan = Implicits SrcSpan

data ImplicitBinding a
  = ImplicitBind
        { _impl_id      :: Identifier a
        , _impl_expr    :: Expression a
        , _impl_annot   :: a
        }
  deriving Show
type ImplicitBindingSpan = ImplicitBinding SrcSpan

data Literal a
  = LitInt
        { _lit_int      :: Int
        , _lit_annot    :: a
        }
  | LitChar
        { _lit_char     :: Char
        , _lit_annot    :: a
        }
  | LitString
        { _lit_str      :: String
        , _lit_annot    :: a
        }
  deriving Show
type LiteralSpan = Literal SrcSpan

makeLenses ''Identifier
makeLenses ''Module
makeLenses ''Import
makeLenses ''BodyStmt
makeLenses ''Where
makeLenses ''Expression
makeLenses ''TypeDecl
makeLenses ''Argument
makeLenses ''ImplicitBinding
makeLenses ''Literal

--------------------------------------------------------------------------------

type Indentation = Int
type PrettyShowMonad = State PrettyShowState ()

data PrettyShowState
  = PrettyShowState
        { _ps_indent    :: Indentation
        , _ps_lines     :: [(Indentation, String)]
        }

makeLenses ''PrettyShowState

class PrettyShow a where
        pshow :: a -> PrettyShowMonad

prettyShow :: PrettyShow a => a -> String
prettyShow = prettyShowIndent 0 "    "

prettyShowIndent :: PrettyShow a => Indentation -> String -> a -> String
prettyShowIndent n str = showLines . _ps_lines . flip execState initST . pshow
    where
        showLines :: [(Indentation, String)] -> String
        showLines = concatMap (\(ind, s) -> tabs ind ++ s ++ "\n") . reverse
        tabs :: Indentation -> String
        tabs ind = concat (replicate ind str)
        initST :: PrettyShowState
        initST = PrettyShowState n [(n, "")]

----------------------------------------

pshows :: (PrettyShow a, Foldable f) => PrettyShowMonad -> f a -> PrettyShowMonad
pshows act xs = case toList xs of
        p:ps -> pshow p >> mapM_ (\x -> act >> pshow x) ps
        _    -> return ()

string :: String -> PrettyShowMonad
string str = ps_lines._head._2 %= (++ str)

lstring :: Located String -> PrettyShowMonad
lstring = string . view loc_insd

line :: PrettyShowMonad
line = use ps_indent >>= \n -> ps_lines %= cons (n, "")

raise :: PrettyShowMonad
raise = ps_indent += 1

lower :: PrettyShowMonad
lower = ps_indent -= 1

----------------------------------------

instance PrettyShow (Module a) where
        pshow (Module mexprts imprts bdy _) = do
                when (isJust mexprts) $ do
                        let Just exprts = mexprts
                        string "export (" >> string (showExports exprts) >> string ")"
                        line >> line

                pshows line imprts >> line >> line
                pshows (line >> line) bdy
            where
                -- showExports :: Foldable f => f (Identifier a) -> String
                showExports = intercalate ", " . toListOf (each.idn_str)

instance PrettyShow (Import a) where
        pshow (Import path mas mspec _) = do
                string "import " >> string path
                when (isJust mas) $ do
                        let Just as = mas
                        string (" as " ++ view idn_str as)
                when (isJust mspec) $ do
                        let Just spec = mspec
                        string "(" >> string (showSpecs spec) >> string ")"
            where
                -- showSpecs :: Foldable f => f (Identifier a) -> String
                showSpecs = intercalate ", " . toListOf (each.idn_str)

instance PrettyShow (BodyStmt a) where
        pshow bdst = case bdst of
                FunctionDecl idn typ _ ->
                        string (view idn_str idn) >> string " : " >> pshow typ
                -- OpenType
                -- ReopenType
                ClosedType idn typ cns _ -> do
                        string (view idn_str idn) >> string " : " >> pshow typ >> string " as "
                        raise >> line >> pshows line cns >> lower
                FunctionDef args expr _ -> do
                        pshows (string " ") args
                        string " = " >> pshow expr
                _ -> string "(re|)open"

instance PrettyShow f => PrettyShow (Where a f) where
        pshow (Where bdy a _) = do
                pshow a
                unless (null bdy) $ do
                        raise >> line
                        string "where"

                        raise >> line
                        pshows line bdy
                        lower >> lower

instance PrettyShow (Expression a) where
        pshow expr = case expr of
                Var str _           -> string "«" >> string str >> string "»"
                Lit lit _           -> pshow lit
                Application exprs _ -> pshows' " " exprs
                Lambda args _       -> string "\\ " >> pshows' " " args >> string "->"
                Forall typs _       -> string "forall (" >> pshows' ", " typs >> string ")"
                Exists typ expr' _  -> string "exists (" >> pshow typ >> string ";"
                                                       >> pshow expr' >> string ")"
                With typ _          -> string "with (" >> pshow typ >> string ")"
                ImplicitExpr imps _ -> string "{" >> pshows' " " imps >> string "}"
            where
                pshows' :: (PrettyShow a, Foldable f) => String -> f a -> PrettyShowMonad
                pshows' = pshows . string


instance PrettyShow (TypeDecl a) where
        pshow (TypeDecl idn typ _) = string (view idn_str idn) >> string " : " >> pshow typ

instance PrettyShow (Argument a) where
        pshow arg = case arg of
                Binding idn _               -> string (view idn_str idn)
                DontCare _                  -> string "_"
                ParenthesizedBinding args _ -> string "(" >> pshows (string " ") args >> string ")"

instance PrettyShow (ImplicitBinding a) where
        pshow (ImplicitBind idn expr _) = string (view idn_str idn) >> string " = " >> pshow expr

instance PrettyShow (Literal a) where
        pshow lit = case lit of
                LitInt int _    -> string (show int)
                LitChar chr _   -> string (show chr)
                LitString str _ -> string (show str)