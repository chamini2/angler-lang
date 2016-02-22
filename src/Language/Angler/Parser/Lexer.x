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
        , lexProgram

        , runLP
        , execLP
        , evalLP

        , popContext
        ) where

import           Language.Angler.Error
import           Language.Angler.Monad
import           Language.Angler.SrcLoc
import           Language.Angler.Parser.Token
import           Language.Angler.Parser.LP

import qualified Codec.Binary.UTF8.String      as UTF8 (encode)
import           Control.Lens
import           Control.Monad                 (liftM)
import           Control.Monad.Except          (runExcept)
import           Control.Monad.State           (StateT(..))
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
$symbol       = [ \- \! \@ \# \$ \% \& \* \+ \/ \\ \< \= \> \^ \| \~ \? \` \[ \] \: \; \. ]
$small        = a - z
$large        = A - Z
$alpha        = [ $small $large ]

$escape_chars = [ a b f n r t v \\ \' \" ]                              -- "

----------------------------------------
-- regex

@num          = $digit+
@hex          = 0 x $hexdigit+
@float        = @num \. @num

@char         = \' ($printable # [\\ \'] | \\ $escape_chars)  \'
@string       = \" ($printable # [\\ \"] | \\ $escape_chars)* \"        -- "

-- identifiers
@opalpha      = $alpha [ $alpha $digit \' ]*

@opsymbol     = $symbol [ $symbol \' ]*

-- without '_'
@op           = @opalpha | @opsymbol

-- with '_'
@closed       = (@op \_)* @op
@ident        = (\_)? (@op \_)* @op (\_)?

-- for modules (paths)
@path         = (@opalpha \.)*
@qualf        = @path @ident
--------------------------------------------------------------------------------

angler :-

-- all states: skip whitespace
$white_no_nl+   ;
\t              { warnTab }

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

        -- identifiers
        @ident  { identifier (fixity TkIdentifier) }
        @qualf  { identifier TkQualified  }

        @num    { tokenStore (TkNumber . read) }
        @char   { tokenStore (TkChar   . read) }
        @string { tokenStore (TkString . read) }

        \,      { token TkComma     }
        -- \@      { token TkAt        }

        \(      { token TkLParen }
        \)      { token TkRParen }
        \{      { token TkLCurly }
        \}      { token TkRCurly }

        \_      { token TkUnderscore }
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
        [ ("module"   , TkModule    )
        , ("exports"  , TkExports   )
        , ("import"   , TkImport    )
        , ("as"       , TkAs        )
        , ("open"     , TkOpen      )
        , ("reopen"   , TkReopen    )
        , ("closed"   , TkClosed    )
        , ("with"     , TkWith      )
        , ("where"    , TkWhere     )
        , ("let"      , TkLet       )
        , ("in"       , TkIn        )
        , ("forall"   , TkForall    )
        , ("exists"   , TkExists    )
        , ("select"   , TkSelect    )
        , ("case"     , TkCase      )
        , ("of"       , TkOf        )
        -- , ("behaviour", TkBehaviour )
        -- , ("on"       , TkOn        )
        -- , ("is"       , TkIs        )
        , ("operator" , TkOperator  )
        , ("prefix"   , TkPrefix    )
        , ("postfix"  , TkPostfix   )
        , ("infixL"   , TkInfixL    )
        , ("infixR"   , TkInfixR    )
        , ("infixN"   , TkInfixN    )

        -- symbols
        , (":"        , TkColon     )
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

throwLError :: LexError -> SrcSpan -> LP a
throwLError err span = throwError (Loc span (LexError err))

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

warnTab :: Action
warnTab span _buf _len = warn (Loc span TabCharacter) >> lexToken

token :: Token -> Action
token tk span _buf _len = return (Loc span tk)

tokenStore :: (String -> Token) -> Action
tokenStore fnTk span buf len = let tk = fnTk (take len buf)
                               in return (Loc span tk)

push :: Int -> Action
push ls _span _buf _len = pushM lp_lex_state ls >> lexToken

pop :: Action
pop _span _buf _len = popM lp_lex_state >> lexToken

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
                TkWhere -> pushM lp_lex_state layout    -- for .. where ..
                TkWith  -> pushM lp_lex_state layout    -- for type constructors
                TkLet   -> pushM lp_lex_state layout    -- for let .. in ..
                TkOf    -> pushM lp_lex_state layout    -- for case .. of ..
                _       -> return ()

fixity :: (String -> Fixity -> Token) -> String -> Token
fixity idTk str = idTk str $ case (head str, last str) of
        ('_', '_') -> Infix
        (_  , '_') -> Prefix
        ('_', _  ) -> Postfix
        _          -> if any (=='_') str then Closedfix else Nofix

newLayoutContext :: Token -> Action
newLayoutContext tk span _buf len = do
        popM lp_lex_state
        (l,_,_,_) <- getInput
        let offset = srcLocCol l - len
        ctx <- use lp_context
        case ctx of
                Layout prev_off : _
                    | prev_off >= offset ->
                        -- token is indented to the left of the previous context.
                        -- we must generate a {} sequence now.
                        pushM lp_lex_state empty_layout >> return (Loc span tk)
                _ -> pushM lp_context (Layout offset) >> return (Loc span tk)

popContext :: LP ()
popContext = popM lp_context

emptyLayout :: Action
emptyLayout span _buf _len = do
        popM lp_lex_state
        pushM lp_lex_state bol
        return (Loc span TkVRCurly)

processLayout :: Action
processLayout span _buf _len = do
        pos <- getOffside span
        case pos of
                -- not popping the lexState, we might have a ';' or another '}' to insert
                -- inserting '}'
                LT -> popM lp_context >> return (Loc span TkVRCurly)
                -- inserting ';'
                EQ -> popM lp_lex_state >> return (Loc span TkVSemicolon)
                -- keep lexing as if in the same line
                GT -> popM lp_lex_state >> lexToken

--------------------------------------------------------------------------------
-- exposed functions

runLP :: String -> SrcLoc -> LP a -> Either (Located Error) (a, LPState)
runLP input loc = runExcept . flip runStateT initialST
    where
        initialST :: LPState
        initialST = def
                { _lp_buffer    = input
                , _lp_last_char = '\n'
                , _lp_loc       = loc
                -- , _lp_bytes     = []
                -- , _lp_last_tk   = Nothing
                -- , _lp_last_loc  = srcLocSpan loc loc
                -- , _lp_last_len  = 0
                , _lp_lex_state = [layout, 0]           -- start in 'layout' state to
                                                        -- introduce the global layout
                -- , _lp_context   = []
                -- , _lp_srcfiles  = []
                -- , _lp_warnings  = []
                }

execLP :: String -> SrcLoc -> LP a -> Either (Located Error) LPState
execLP input loc = over _Right snd . runLP input loc

evalLP :: String -> SrcLoc -> LP a -> Either (Located Error) (a, [Located Warning])
evalLP input loc = over (_Right._2) (view lp_warnings) . runLP input loc

lexProgram :: String -> SrcLoc -> Either (Located Error) ([Located Token], [Located Warning])
lexProgram input loc = evalLP input loc lexTokens
    where
        lexTokens :: LP [Located Token]
        lexTokens = do
                tk  <- lexToken
                tks <- case view loc_insd tk of
                        TkEOF -> return []
                        _     -> lexTokens
                return (tk:tks)

----------------------------------------
-- lexer handling

lexer :: (Located Token -> LP a) -> LP a
lexer = (>>=) lexToken

lexToken :: LP (Located Token)
lexToken = do
        inp@(l,_c,_bs,b) <- getInput
        ls <- peekM lp_lex_state
        case alexScan inp ls of
                AlexEOF -> do
                    ls' <- peekM lp_lex_state
                    if ls' == comment
                        then throwLError LErrUnterminatedComment (srcLocSpan l l)
                        else do
                        ctx <- use lp_context
                        if not (null ctx)
                            -- closing all the open layouts we had
                            then popM lp_context >> return (Loc (srcLocSpan l l) TkVRCurly)
                            else return (Loc (srcLocSpan l l) TkEOF)
                AlexError (l',_,_,c':_) ->
                        throwLError (LErrUnexpectedCharacter c') (srcLocSpan l l)
                AlexSkip  inp' _len -> setInput inp' >> lexToken
                AlexToken inp'@(l',_,_,_) len act -> setInput inp' >> act (srcLocSpan l l') b len

}
