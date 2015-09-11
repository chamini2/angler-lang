--------------------------------------------------------------------------------
-- University Simón Bolívar, 2015
--
-- Angler's Lexer
--
-- Inspired in GHC's Lexer
--------------------------------------------------------------------------------
{
{-# OPTIONS_GHC -funbox-strict-fields -w #-}

module Language.Angler.Parser.Lexer
        ( lexer
        -- , lexToken
        , lexTokens

        , runLP
        , execLP
        , evalLP
        ) where

import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.Parser.Token
import           Language.Angler.Parser.LP

import qualified Codec.Binary.UTF8.String      as UTF8 (encode)
import           Control.Lens
import           Control.Monad                 (liftM)
import           Control.Monad.Identity        (Identity(runIdentity))
import           Control.Monad.Except          (runExceptT)
import           Control.Monad.State           (StateT(runStateT))
import qualified Data.Bits                     ((.&.), shiftR)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map (lookup, fromList)
import           Data.Word                     (Word8)


import           Prelude                       hiding (lookup, span)

}

----------------------------------------
-- sets

$nl           = [\n \r \f]
$whitechar    = [$nl \v \ ]
$white_no_nl  = $whitechar # \n

$digit        = 0 - 9
$hexdigit     = [ $digit a - f A - F ]
$symbol       = [ \- \! \# \$ \% \& \* \+ \/ \\ \< \= \> \^ \| \~ \? \` \[ \] \: \. ]
$small        = a - z
$large        = A - Z
$alpha        = [ $small $large ]

$escape_chars = [ a b f n r t v \\ \' \" ]                              -- "

----------------------------------------
-- regex

@int          = $digit+
@hex          = 0 x $hexdigit+
@float        = @int \. @int

@char         = \' ($printable # [\\ \'] | \\ $escape_chars)  \'
@string       = \" ($printable # [\\ \"] | \\ $escape_chars)* \"        -- "

-- identifiers
@opalpha      = $alpha [ $alpha $digit \' ]*
@idalpha      = ((\_)? @opalpha)+ (\_)?

@opsymbol     = $symbol [ $symbol \' ]*
@idsymbol     = ((\_)? @opsymbol)+ (\_)?

@op           = @opalpha | @opsymbol
                -- with '_'            | without '_'
@ident        = (\_)? (@op \_)+ (@op)? | @op
-- @ident        = @idalpha | @idsymbol

-- for modules (paths)
@path         = (@idalpha \.)*
-- @imprt        = @path @idalpha
@qualf        = @path @ident
--------------------------------------------------------------------------------

angler :-

-- all states: skip whitespace
$white_no_nl+   ;
\t              ;               -- XXX: maybe give a warning

-- comments and 'comment' state: every time a "{-" is seen, a 'comment' state
-- is pushed, that way we have nested comments
"--" .*         ;
"{-"            { push comment }                -- nested comments

<comment> {
        "-}"    { pop }
        .       ;
        \n      ;
}

-- 'bol' state: beginning of a line. Slurp up all the whitespace (including
-- blank lines) until we find a non-whitespace character, then do layout
-- processing.
<bol> {
        \n      ;
        ()      / { nextIsEOF } { pop }         -- don't produce a ';' at the end
        ()      { processLayout }
}

<0> {
        \n      { push bol }
        @ident  { identifier TkIdentifier }
        -- @imprt  { identifier TkImportPath }
        @qualf  { identifier TkQualified  }

        @int    { tokenStore (TkInteger . read) }
        @char   { tokenStore (TkChar    . read) }
        @string { tokenStore (TkString  . read) }

        \;      { token TkSemicolon }
        \,      { token TkComma     }
        -- \@      { token TkAt        }

        \(      { token TkLParen }
        \)      { token TkRParen }
        \{      { token TkLCurly }
        \}      { token TkRCurly }

        \_      { token TkUnderscore }

        -- errors
        -- ''      { lexError EmptyChar }
        -- '@ident { lexError QuoteStartIdent }
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
        -- words
        [ ("export"   , TkExport    )
        , ("import"   , TkImport    )
        , ("as"       , TkAs        )
        , ("open"     , TkOpen      )
        , ("reopen"   , TkReopen    )
        , ("closed"   , TkClosed    )
        , ("with"     , TkWith      )
        , ("where"    , TkWhere     )
        , ("forall"   , TkForall    )
        , ("exists"   , TkExists    )
        , ("select"   , TkSelect    )
        -- , ("behaviour", TkBehaviour )
        -- , ("on"    , TkOn        )
        -- , ("is"    , TkIs        )

        -- symbols
        , (":"        , TkColon     )
        -- , (";"        , TkSemicolon )
        , ("."        , TkDot       )
        , ("->"       , TkArrow     )
        , ("\\"       , TkBackslash )
        , ("="        , TkEquals    )
        -- , (","        , TkComma     )
        -- , ("("        , TkLParen    )
        -- , (")"        , TkRParen    )
        -- , ("{"        , TkLCurly    )
        -- , ("}"        , TkRCurly    )
        -- , ("_"        , TkUnderscore)
        ]

----------------------------------------
-- Alex requirements

-- from Alex's monad wrapper
type AlexInput
  = ( SrcLoc                    -- current position
    , Char                      -- previous char
    , [Byte]                    -- pending bytes on current char
    , String                    -- current buffer string
    )

-- from Alex's monad wrapper
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

-- from Alex's monad wrapper
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte inp = case inp of
        (p, c, (b:bs), s    ) -> Just (b,(p,c,bs,s))
        (_, _, []    , []   ) -> Nothing
        (p, _, []    , (c:s)) -> let p'     = locMove p c
                                     b : bs = UTF8.encode [c]
                                 in p' `seq` Just (b, (p', c, bs, s))

----------------------------------------
-- Lexer monad

type LPAction a = SrcSpan -> String -> Int -> LP a
type Action = LPAction (Located Token)

----------------------------------------
-- LPState's manipulation

getInput :: LP AlexInput
getInput = use id >>= \s -> let l  = view lp_loc s
                                c  = view lp_last_char s
                                bs = view lp_bytes s
                                b  = view lp_buffer s
                            in return (l, c, bs, b)

setInput :: AlexInput -> LP ()
setInput (loc,c,bs,inp) = do
        lp_loc       .= loc
        lp_buffer    .= inp
        lp_bytes     .= bs
        lp_last_char .= c

----------------------------------------
-- Lexer predicates

type AlexPredicate = AlexAccPred ()

nextIsEOF :: AlexPredicate
nextIsEOF _ _ _ (_,_,_,b) = null b

----------------------------------------
-- Lexer actions

token :: Token -> Action
token tk span _buf _len = return (Loc span tk)

tokenStore :: (String -> Token) -> Action
tokenStore fnTk span buf len = let tk = fnTk (take len buf)
                               in return (Loc span tk)

push :: Int -> Action
push ls _span _buf _len = pushLP lp_lex_state ls >> lexToken

pop :: Action
pop _span _buf _len = popLP lp_lex_state >> lexToken

-- from GHC's lexer
identifier :: (String -> Token) -> Action
identifier idTk span buf len = case Map.lookup str reserved of
        Just tk -> maybeLayout tk >> return (Loc span tk)
        Nothing -> return (Loc span (idTk str))
    where
        str :: String
        str = take len buf
        -- certain keywords put us in the "layout" state, where we might
        -- add an opening curly brace.
        maybeLayout :: Token -> LP ()
        maybeLayout tk = case tk of
                TkWhere -> pushLP lp_lex_state layout
                TkWith  -> pushLP lp_lex_state layout   -- for type constructors
                _       -> return ()

newLayoutContext :: Token -> Action
newLayoutContext tk span _buf len = do
        popLP lp_lex_state
        (l,_,_,_) <- getInput
        let offset = srcLocCol l - len
        ctx <- use lp_context
        case ctx of
                Layout prev_off : _
                        | prev_off >= offset ->
                                -- token is indented to the left of the previous context.
                                -- we must generate a {} sequence now.
                                pushLP lp_lex_state empty_layout >> return (Loc span tk)
                _ -> pushLP lp_context (Layout offset) >> return (Loc span tk)

emptyLayout :: Action
emptyLayout span _buf _len = do
        popLP lp_lex_state
        pushLP lp_lex_state bol
        return (Loc span TkVRCurly)

processLayout :: Action
processLayout span _buf _len = do
        pos <- getOffside span
        case pos of
                -- not popping the lexState, we might have a ';' or another '}' to insert
                -- inserting '}'
                LT -> popLP lp_context >> return (Loc span TkVRCurly)
                -- inserting ';'
                EQ -> popLP lp_lex_state >> return (Loc span TkVSemicolon)
                -- keep lexing as if in the same line
                GT -> popLP lp_lex_state >> lexToken

--------------------------------------------------------------------------------
-- exposed functions

runLP :: String -> SrcLoc -> LP a -> Either (Located Error) (a, LPState)
runLP input loc = runIdentity . runExceptT . flip runStateT initialST
    where
        initialST :: LPState
        initialST = LPState
                { _lp_buffer    = input
                , _lp_last_char = '\n'
                , _lp_loc       = loc
                , _lp_bytes     = []
                -- , _lp_last_tk   = Nothing
                -- , _lp_last_loc  = srcLocSpan loc loc
                -- , _lp_last_len  = 0
                , _lp_lex_state = [layout, 0]           -- start in 'layout' state to
                                                        -- introduce the global layout
                , _lp_context   = []
                , _lp_srcfiles  = []
                }

execLP :: String -> SrcLoc -> LP a -> Either (Located Error) LPState
execLP input loc = over _Right snd . runLP input loc

evalLP :: String -> SrcLoc -> LP a -> Either (Located Error) a
evalLP input loc = over _Right fst . runLP input loc
 
----------------------------------------
-- lexer handling

lexer :: (Located Token -> LP a) -> LP a
lexer = (>>=) lexToken

lexToken :: LP (Located Token)
lexToken = do
        inp@(l,_c,_bs,b) <- getInput
        ls <- peekLP lp_lex_state
        case alexScan inp ls of
                AlexEOF -> use lp_context >>= \ctx -> if not (null ctx)
                        -- closing all the open layouts we had
                        then popLP lp_context >> return (Loc (srcLocSpan l l) TkVRCurly)
                        else return (Loc (srcLocSpan l l) TkEOF)
                AlexError (l',_,_,c':_) ->
                        throwError (Loc (srcLocSpan l l) (LexError (LErrUnexpectedCharacter c')))
                AlexSkip  inp' _len -> setInput inp' >> lexToken
                AlexToken inp'@(l',_,_,_) len act -> setInput inp' >> act (srcLocSpan l l') b len

lexTokens :: LP [Located Token]
lexTokens = do
        tk  <- lexToken
        tks <- case view loc_insd tk of
                TkEOF -> return []
                _     -> lexTokens
        return (tk:tks)

}
