module Language.Angler.AST where

import           Language.Angler.SrcLoc
import           Data.Sequence (Seq)

type Identifier = Located String
type Qualified  = Identifier

data Module
  = Module
        { mod_exports   :: Maybe (Seq Identifier)       -- 'Nothing' means to export everything
        , mod_imports   :: Seq (Located Import)
        , mod_body      :: Seq ()
        }
  deriving Show

data Import
  = Import
        { imp_path      :: FilePath
        , imp_as        :: String
        , imp_specific  :: Maybe (Seq Identifier)       -- 'Nothing' means to import everything
        }
  deriving Show

