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

type Identifier = Located String
type Qualified  = Identifier

data Module
  = Module
        { mod_exports   :: Maybe (Seq Identifier)       -- 'Nothing' means to export everything
        , mod_imports   :: Seq (Located Import)
        , mod_body      :: Body
        }
  deriving Show

data Import
  = Import
        { imp_path      :: FilePath
        , imp_as        :: String
        , imp_specific  :: Maybe (Seq Identifier)       -- 'Nothing' means to import everything
        }
  deriving Show

type Body = Seq BodyStmt

data BodyStmt
  = FunctionDecl
        { dcl_id        :: Identifier
        , dcl_type      :: Where Expression
        }
  | DataDecl
        { dcl_id        :: Identifier
        , dcl_type      :: Where Expression
        , dcl_cons      :: Seq (TypeDecl)
        }
  | FunctionDef
        { def_args      :: Seq Argument
        , def_impl      :: Implicit
        , def_expr      :: Where Expression
        }
  deriving Show

data Where a = Where Body a
  deriving Show

data Expression
  = Value               Literal
  | Application         (Seq Expression)
  | Lambda              (Seq Argument)
  | Forall              (Seq TypeDecl)
  | Exists              TypeDecl        Expression
  | With                TypeDecl
  | ImplicitExpr        Implicit
  deriving Show

data TypeDecl
  = TypeDecl
        { typ_id        :: Identifier
        , typ_type      :: Where Expression
        }
  deriving Show

data Argument
  = Binding Identifier
  | DontCare
  | PatternMatch Expression
  deriving Show

type Implicit = Seq ImplicitBinding

data ImplicitBinding = ImplicitBind Identifier Expression
  deriving Show

data Literal
  = LitId               Qualified
  | LitInt              (Located Int)
  | LitChar             (Located Char)
  | LitString           (Located String)
  deriving Show

--------------------------------------------------------------------------------


type Indentation = Int
type PrettyShowMonad = State PrettyShowState ()

data PrettyShowState
  = PrettyShowState
        { _ps_indent :: Indentation
        , _ps_lines  :: [(Indentation, String)]
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
        initST = PrettyShowState n [(n, "")]

----------------------------------------

pshows :: (PrettyShow a, Foldable f) => PrettyShowMonad -> f a -> PrettyShowMonad
pshows act xs = case toList xs of
        p:ps -> pshow p >> mapM_ (\x -> act >> pshow x) ps
        _    -> return ()

string :: String -> PrettyShowMonad
string str = ps_lines._head._2 %= (++ str)

lstring :: Located String -> PrettyShowMonad
lstring = string . unlocate

lpshow :: PrettyShow a => Located a -> PrettyShowMonad
lpshow = pshow . unlocate

line :: PrettyShowMonad
line = use ps_indent >>= \n -> ps_lines %= cons (n, "")

raise :: PrettyShowMonad
raise = ps_indent += 1

lower :: PrettyShowMonad
lower = ps_indent -= 1

----------------------------------------

instance PrettyShow Module where
        pshow (Module mexprts imprts bdy) = do
                when (isJust mexprts) $ do
                        let Just exprts = mexprts
                        string "export (" >> string (showExports exprts) >> string ")"
                        line >> line

                mapM_ (\x -> lpshow x >> line) imprts
                line
                mapM_ (\x -> pshow x >> line) bdy
            where
                showExports = intercalate ", " . fmap unlocate . toList

instance PrettyShow Import where
        pshow (Import path as mspec) = do
                string "import " >> string path
                unless (null as) (string (" as " ++ as))
                when (isJust mspec) $ do
                        let Just spec = mspec
                        string "(" >> string (showSpec spec) >> string ")"
            where
                showSpec = intercalate ", " . fmap unlocate . toList

instance PrettyShow BodyStmt where
        pshow bdst = case bdst of
                FunctionDecl idn typ  ->
                        lstring idn >> string " : " >> pshow typ
                DataDecl idn typ cns -> do
                        lstring idn >> string " : " >> pshow typ >> string " as "
                        raise >> line >> pshows line cns >> lower >> line
                FunctionDef args imp expr -> do
                        pshows (string " ") args
                        unless (null imp) $ string " {" >> mapM_ pshow imp >> string "}"
                        string " = " >> pshow expr

instance PrettyShow a => PrettyShow (Where a) where
        pshow (Where bdy a) = do
                pshow a
                unless (null bdy) $ do
                        raise >> line
                        string "where"

                        raise >> line
                        pshows line bdy

                        lower >> lower >> line

instance PrettyShow Expression where
        pshow expr = case expr of
                Value lit         -> pshow lit
                Application exprs -> pshows' " " exprs
                Lambda args       -> string "\\ " >> pshows' " " args >> string "->"
                Forall typs       -> string "forall (" >> pshows' ", " typs >> string ") ."
                Exists typ expr'  -> string "exists (" >> pshow typ >> string ";" >> pshow expr' >> string ")"
                With typ          -> string "with (" >> pshow typ >> string ")"
                ImplicitExpr imps -> string "{" >> pshows' " " imps >> string "}"
            where
                pshows' :: (PrettyShow a, Foldable f) => String -> f a -> PrettyShowMonad
                pshows' str = pshows (string str)


instance PrettyShow TypeDecl where
        pshow (TypeDecl idn typ) = lstring idn >> string " : " >> pshow typ

instance PrettyShow Argument where
        pshow arg = case arg of
                Binding idn       -> lstring idn
                DontCare          -> string "_"
                PatternMatch expr -> do
                        string "("
                        pshow expr
                        string ")"

instance PrettyShow ImplicitBinding where
        pshow (ImplicitBind idn expr) = do
                lstring idn
                string " = "
                pshow expr

instance PrettyShow Literal where
        pshow lit = case lit of
                LitId qid     -> string (unlocate qid)
                LitInt int    -> string (show (unlocate int))
                LitChar chr   -> string (show (unlocate chr))
                LitString str -> string (show (unlocate str))