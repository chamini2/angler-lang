--------------------------------------------------------------------------------
-- University Simón Bolívar, 2015
--
-- Angler's Lexer
--
-- Inspired in GHC's Lexer
--------------------------------------------------------------------------------
{
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Language.Angler.Parser.Lexer (lexToken, runLP, runLexer, lexer) where

import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.Parser.Token

import           Control.Monad.Identity    (Identity(..))
import           Control.Monad.Error       (ErrorT(..), throwError)
import           Control.Monad.State       (StateT(..), get, gets, modify)
import qualified Data.Bits                 ((.&.), shiftR)
import           Data.Either               (isRight)
-- import           Data.List                 (unfoldr)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map (lookup, fromList)
import           Data.Word                 (Word8)

import           Debug.Trace               (trace, traceShow)

import           Prelude                   hiding (id, lookup, span)

}

----------------------------------------
-- sets

$nl           = [\n \r \f]
$whitechar    = [$nl \v \ ]
$white_no_nl  = $whitechar # \n
$tab          = \t

$digit        = 0-9
$hexdigit     = [ $digit a-f A-F]
$symbolstart  = [ \- \! \# \$ \% \& \* \+ \/ \< \= \> \^ \| \~ \? \` \[ \] \, \: \\ ]
$symbol       = [ $symbolstart \' ]             -- consider character literals
$small        = a-z
$large        = A-Z
$alpha        = [ $small $large ]

$escape_chars = [ abfnrtv\\'\"\? ]

----------------------------------------
-- regex

@number       = $digit+

-- identifiers
@opalpha      = $alpha [ $alpha $digit ]*
@identalpha   = ((\_)? @opalpha)+ (\_)?

@opsymbol     = $symbolstart $symbol*
@identsymbol  = ((\_)? @opsymbol)+ (\_)?

@identifier   = @identalpha | @identsymbol

-- for namespaces (modules)
@namespace    = (@identalpha \.)*
@qualified    = @namespace @identifier
--------------------------------------------------------------------------------

angler :-

-- all states: skip whitespace
$white_no_nl+   ;
$tab            ;

-- 'bol' state: beginning of a line. Slurp up all the whitespace (including
-- blank lines) until we find a non-whitespace character, then do layout
-- processing.
<bol> {
        $whitechar* $
                ;
        ()      { processLayout }
}

<0> {
        @identifier
                { identifier }
        \n      { begin bol }
}

<layout> {
        \n      ;
        ()      { newLayoutContext TkVLCurly }
}

<empty_layout> {
        ()      { emptyLayout }
}

{

--------------------------------------------------------------------------------

reserved :: Map String Token
reserved = Map.fromList
        [ ("where" , TkWhere)
        , ("forall", TkForall)
        , ("exists", TkExists)
        , ("with"  , TkWith)
        , ("on"    , TkOn)
        , ("is"    , TkIs)
        ]

----------------------------------------
-- Alex requirements

type Byte = Word8

-- from Alex's monad wrapper
type AlexInput = ( SrcLoc               -- current position
                 , Char                 -- previous char
                 , [Byte]               -- pending bytes on current char
                 , String               -- current input string
                 )

-- from Alex's monad wrapper
-- alexIgnoreBytes :: AlexInput -> AlexInput
-- alexIgnoreBytes (l,c,_,s) = (l,c,[],s)

-- from Alex's monad wrapper
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

-- from Alex's monad wrapper
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte inp = case inp of
        (p, c, (b:bs), s    ) -> Just (b,(p,c,bs,s))
        (_, _, []    , []   ) -> Nothing
        (p, _, []    , (c:s)) -> let p'     = locMove p c
                                     b : bs = utf8Encode c
                                 in p' `seq` Just (b, (p', c, bs, s))
    where
        -- from Alex's monad wrapper
        -- Encode a Haskell String to a list of Bytes, in UTF8 format.
        utf8Encode :: Char -> [Byte]
        utf8Encode = map fromIntegral . go . ord
            where
                go oc
                  | oc <= 0x7f   = [oc]
                  | oc <= 0x7ff  = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                                   , 0x80 + oc Data.Bits..&. 0x3f
                                   ]
                  | oc <= 0xffff = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                                   , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                   , 0x80 + oc Data.Bits..&. 0x3f
                                   ]
                  | otherwise    = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                                   , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                                   , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                                   , 0x80 + oc Data.Bits..&. 0x3f
                                   ]

----------------------------------------
-- Lexer monad

-- from GHC's Lexer
data LayoutContext
  = NoLayout            -- top level definitions
  | Layout Int
  deriving (Show)

-- from GHC's Lexer
data LPState
  = LPState
        { lp_buffer     :: String
        , lp_last_char  :: Char
        , lp_loc        :: SrcLoc               -- current location (end of prev token + 1)
        , lp_bytes      :: [Byte]
        --, lp_last_tk    :: Maybe Token
        --, lp_last_loc   :: SrcSpan              -- location of previous token
        --, lp_last_len   :: Int                  -- length of previous token
        , lp_lex_state  :: [Int]                -- lexer states stack
        , lp_context    :: [LayoutContext]      -- contexts stack
        , lp_srcfiles   :: [String]
        }

type LP a = StateT LPState (ErrorT (Located Error) Identity) a

type LPAction a = SrcSpan -> String -> Int -> LP a
type Action = LPAction (Located Token)

runLP :: String -> SrcLoc -> LP a -> Either (Located Error) (a, LPState)
runLP input loc = runIdentity . runErrorT . flip runStateT initialLPState
    where
        initialLPState = LPState
                { lp_buffer    = input
                , lp_last_char = '\n'
                , lp_loc       = loc
                , lp_bytes     = []
                --, last_tk      = Nothing
                --, last_loc     = srcLocSpan loc loc
                --, last_len     = 0
                , lp_lex_state = [bol, 0]               -- start in 'bol' state
                , lp_context   = []
                , lp_srcfiles  = []
                }

execLP :: String -> SrcLoc -> LP a -> Either (Located Error) LPState
execLP input loc = check . runLP input loc
    where
        check r = if isRight r
                then let Right (_, st) = r in Right st
                else let Left l        = r in Left l

evalLP :: String -> SrcLoc -> LP a -> Either (Located Error) a
evalLP input loc = check . runLP input loc
    where
        check r = if isRight r
                then let Right (x, _st) = r in Right x
                else let Left l         = r in Left l

----------------------------------------
-- LPState's manipulation

getInput :: LP AlexInput
getInput = get >>= \s -> let l  = lp_loc s
                             c  = lp_last_char s
                             bs = lp_bytes s
                             b  = lp_buffer s
                         in return (l, c, bs, b)

setInput :: AlexInput -> LP ()
setInput (loc,c,bs,inp) = modify $ \s ->
        s{ lp_loc = loc, lp_buffer = inp, lp_bytes = bs, lp_last_char = c }

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
        let indent = srcSpanStartCol span
        return $ case stk of
                Layout indent' : _ -> compare indent indent'
                _                  -> GT

----------------------------------------
-- Lexer actions

token :: Token -> Action
token tk span _buf _len = return (Loc span tk)

layoutToken :: Token -> Action
layoutToken tk span _buf _len = pushLexState layout >> return (Loc span tk)

--idtoken :: String -> Action
--idtoken id span _buf _len = return (Loc span (TkIdentifier id))

begin :: Int -> Action
begin ls _span _buf _len = pushLexState ls >> lexToken

-- from GHC's lexer
identifier :: Action
identifier span buf len = let str = take len buf in
        case Map.lookup str reserved of
                Just tk -> maybeLayout tk >> return (Loc span tk)
                Nothing -> return (Loc span (TkIdentifier str))
    where
        -- certain keywords put us in the "layout" state, where we might
        -- add an opening curly brace.
        maybeLayout :: Token -> LP ()
        maybeLayout tk = case tk of
                TkWhere -> pushLexState layout
                _       -> return ()

newLayoutContext :: Token -> Action
newLayoutContext tk span _buf len = do
        popLexState
        (l,_,_,_) <- getInput
        let offset = srcLocCol l - len
        ctx <- gets lp_context
        case ctx of
                Layout prev_off : _ | prev_off >= offset ->
                        -- pushLexState empty_layout
                        -- return (Loc span tk)
                        throwError (Loc span (LexError LErrEmptyLayout))
                _ -> pushContext (Layout offset) >> return (Loc span tk)

emptyLayout :: Action
emptyLayout span _buf _len = popLexState >> pushLexState bol >> return (Loc span TkVRCurly)

processLayout :: Action
processLayout span _input _len = do
        pos <- getOffside span
        case pos of
                -- not popping the lexState, we might have a ';' or another '}' to insert
                -- inserting '}'
                LT -> trace "}" (return ()) >> popContext >> return (Loc span TkVRCurly)
                -- inserting ';'
                EQ -> trace ";" (return ()) >> popLexState >> return (Loc span TkSemicolon)
                -- keep lexing as same in line
                GT -> trace "\\" (return ()) >> popLexState >> lexToken

--------------------------------------------------------------------------------
-- exposed functions
 
runLexer :: String -> Either (Located Error) [Located Token]
runLexer input = evalLP input (SrcLoc "" 1 1) lexTokens
    where
        lexTokens = lexToken >>= \tk -> case tk of
                Loc _ TkEOF -> return [tk]
                _           -> lexTokens >>= return . ((:) tk)

lexer :: (Located Token -> LP a) -> LP a
lexer = (>>=) lexToken

lexToken :: LP (Located Token)
lexToken = do
        inp@(l,_c,_bs,b) <- getInput
        ls <- peekLexState
        case alexScan inp ls of
                AlexEOF                           -> return (Loc (srcLocSpan l l) TkEOF)
                AlexError (l',_,_,c':_)             ->
                        throwError (Loc (srcLocSpan l l') (LexError (LErrUnexpectedCharacter c')))
                AlexSkip  inp' _len               -> setInput inp' >> lexToken
                -- AlexToken inp'@(l',_,_,_) len act -> setInput inp' >> act (srcLocSpan l l') b len
                AlexToken inp'@(l',_,_,_) len act -> do
                        gets lp_lex_state >>= \lss -> trace (" states: " ++ show lss) (return ())
                        gets lp_context   >>= \lcs -> trace ("context: " ++ show lcs) (return ())
                        setInput inp' >> act (srcLocSpan l l') b len

}
