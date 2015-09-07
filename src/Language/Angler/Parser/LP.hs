module Language.Angler.Parser.LP
        ( LP
        , LayoutContext(..)
        , LPState(..)
        , Byte

        , throwError
        , get, gets, modify, put
        , pushLexState, peekLexState, popLexState
        , pushContext, popContext
        , getOffside
        ) where

import           Language.Angler.SrcLoc
import           Language.Angler.Error

import           Control.Monad.Identity (Identity(..))
import           Control.Monad.Except   (ExceptT(..), throwError)
import           Control.Monad.State    (StateT(..), get, gets, modify, put)
import           Data.Word              (Word8)

import           Prelude                hiding (span)

-- LexerParser Monad
type LP a = StateT LPState (ExceptT (Located Error) Identity) a

type Byte = Word8

-- from GHC's Lexer
data LayoutContext
  = NoLayout            -- top level definitions
  | Layout Int
  deriving Show

-- from GHC's Lexer
data LPState
  = LPState
        { lp_buffer     :: String
        , lp_last_char  :: Char
        , lp_loc        :: SrcLoc               -- current location (end of prev token + 1)
        , lp_bytes      :: [Byte]
        -- , lp_last_tk    :: Maybe Token
        -- , lp_last_loc   :: SrcSpan              -- location of previous token
        -- , lp_last_len   :: Int                  -- length of previous token
        , lp_lex_state  :: [Int]                -- lexer states stack
        , lp_context    :: [LayoutContext]      -- contexts stack
        , lp_srcfiles   :: [String]
        }

----------------------------------------
-- LPState's manipulation

pushLexState :: Int -> LP ()
pushLexState ls = modify $ \s -> let lss = lp_lex_state s
                                 in s { lp_lex_state = ls : lss }

peekLexState :: LP Int
peekLexState = gets (head . lp_lex_state)

popLexState :: LP Int
popLexState = do
        ls : lss <- gets lp_lex_state
        modify $ \s -> s{ lp_lex_state = lss }
        return ls

pushContext :: LayoutContext -> LP ()
pushContext lc = modify $ \s -> let lcs = lp_context s
                                in s { lp_context = lc : lcs }

popContext :: LP LayoutContext
popContext = do
        lc : lcs <- gets lp_context
        modify $ \s -> s{ lp_context = lcs }
        return lc

getOffside :: SrcSpan -> LP Ordering
getOffside span = do
        stk <- gets lp_context
        let indent = srcSpanSCol span
        return $ case stk of
                Layout indent' : _ -> compare indent indent'
                _                  -> GT

