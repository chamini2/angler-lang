module Language.Angler.Compact where

import qualified Language.Angler.AST         as L               -- Loose
import qualified Language.Angler.ASTCompact  as C               -- Compact
import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.SymbolTable

import           Control.Lens
import           Control.Monad.State         (State, runState)

data CompactState
  = CompactState
        { _cm_table     :: SymbolTableSpan
        -- , _cm_operators :: [Operator]
        , _cm_warnings  :: [Located Warning]
        -- , _cm_errors    :: [Located Error]
        }

makeLenses ''CompactState

type Compact a = State (CompactState)

runCompact :: Compact a (C.Module a) -> CompactState -> ((C.Module a), CompactState)
runCompact = runState

compactModule :: L.Module a -> Compact a (C.Module a)
compactModule (L.Module name _expts _impts body _annot) = do
        cBody' <- mapM compactBodyStmt body
        C.Module name cBody

compactBodyStmt :: L.BodyStmt a -> Compact a (C.BodyStmt a)
compactBodyStmt stmt = case stmt of
        L.OpenType idn typ mcns _ann -> do
