{-# LANGUAGE RankNTypes #-}

module Language.Angler.Parser.LP
        ( LP
        , Byte
        , LayoutContext(..)

        , LPState(..), def
        , lp_buffer, lp_last_char, lp_loc, lp_bytes
        -- , lp_last_tk, lp_last_loc, lp_last_len
        , lp_lex_state, lp_context, lp_srcfiles
        , lp_warnings

        , getOffside
        ) where

import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.SrcLoc

import           Control.Lens
import           Control.Monad.Except   (Except)
import           Control.Monad.State    (StateT(..))
import           Data.Default           (Default(..))
import           Data.Word              (Word8)

import           Prelude                hiding (span)

-- LexerParser Monad
type LP = StateT LPState (Except (Located Error))

type Byte = Word8

-- from GHC's Lexer
data LayoutContext
  = NoLayout            -- top level definitions
  | Layout Int
  deriving Show

-- from GHC's Lexer
data LPState
  = LPState
        { _lp_buffer    :: String
        , _lp_last_char :: Char
        , _lp_loc       :: SrcLoc               -- current location (end of prev token + 1)
        , _lp_bytes     :: [Byte]
        -- , _lp_last_tk   :: Maybe Token
        -- , _lp_last_loc  :: SrcSpan              -- location of previous token
        -- , _lp_last_len  :: Int                  -- length of previous token
        , _lp_lex_state :: [Int]                -- lexer states stack
        , _lp_context   :: [LayoutContext]      -- contexts stack
        , _lp_srcfiles  :: [String]
        , _lp_warnings  :: [Located Warning]
        }

makeLenses ''LPState

instance STWarnings LPState where
        st_warnings = lp_warnings

instance Default LPState where
        def = LPState
                { _lp_buffer    = ""
                , _lp_last_char = '\n'
                , _lp_loc       = SrcLoc "" 1 1
                , _lp_bytes     = []
                -- , _lp_last_tk   = Nothing
                -- , _lp_last_loc  = SrcSpanNoInfo
                -- , _lp_last_len  = 0
                , _lp_lex_state = []
                , _lp_context   = []
                , _lp_srcfiles  = []
                , _lp_warnings  = []
                }

----------------------------------------
-- LPState's manipulation

getOffside :: SrcSpan -> LP Ordering
getOffside span = do
        stk <- use lp_context
        let indent = view spn_scol span
        return $ case stk of
                Layout indent' : _ -> compare indent indent'
                _                  -> GT
