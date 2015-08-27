--------------------------------------------------------------------------------
-- University Simón Bolívar, 2015
--
-- Angler's Lexer
--
-- Inspired in GHC's Lexer
--------------------------------------------------------------------------------
{
{-# OPTIONS_GHC -funbox-strict-fields -w #-}

module Language.Angler.Parser.Lexer (lexer, runLP, execLP, evalLP) where

import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.Parser.Token
import           Language.Angler.Parser.LP

import           Control.Monad.Identity        (Identity(runIdentity))
import           Control.Monad.Error           (ErrorT(runErrorT))
import           Control.Monad.State           (StateT(runStateT))
import qualified Data.Bits                     ((.&.), shiftR)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map (lookup, fromList)
import           Data.Word                     (Word8)

import           Debug.Trace                   (trace, traceShow)

import           Prelude                       hiding (id, lookup, span)

}

----------------------------------------
-- sets

$nl           = [\n \r \f]
$whitechar    = [$nl \v \ ]
$white_no_nl  = $whitechar # \n
$tab          = \t

$digit        = 0-9
$hexdigit     = [ $digit a-f A-F]
$symbol       = [ \- \! \# \$ \% \& \* \+ \/ \< \= \> \^ \| \~ \? \` \[ \] \, \: \\ \. ]
$small        = a-z
$large        = A-Z
$alpha        = [ $small $large ]

$escape_chars = [ abfnrtv\\'\"\? ]

----------------------------------------
-- regex

@number       = $digit+

-- identifiers
@opalpha      = $alpha [ $alpha $digit \' ]*
@identalpha   = ((\_)? @opalpha)+ (\_)?

@opsymbol     = $symbol [ $symbol \' ]*
@identsymbol  = ((\_)? @opsymbol)+ (\_)?

@identifier   = @identalpha | @identsymbol

-- for namespaces (modules)
@namespace    = (@identalpha \.)*
@qualified    = @namespace @identifier
--------------------------------------------------------------------------------

angler :-

-- all states: skip whitespace
$white_no_nl+   ;
$tab            ;               -- XXX: maybe give a warning

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
        @identifier
                { identifier TkIdentifier }
        @qualified
                { identifier TkQualified }
        \(
                { token TkLParen }
        \)
                { token TkRParen }
        \_
                { token TkUnderscore }
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
        [ ("export", TkExport    )
        , ("import", TkImport    )
        , ("as"    , TkAs        )
        , ("where" , TkWhere     )
        , ("forall", TkForall    )
        , ("exists", TkExists    )
        , ("with"  , TkWith      )
        , ("on"    , TkOn        )
        , ("is"    , TkIs        )

        -- symbols
        , (":"     , TkColon     )
        , ("."     , TkDot       )
        , ("->"    , TkArrow     )
        , ("="     , TkEquals    )
        , (","     , TkComma     )
        , (","     , TkComma     )
        -- , ("("     , TkLParen    )
        -- , (")"     , TkRParen    )
        , ("{"     , TkLCurly    )
        , ("}"     , TkRCurly    )
        -- , ("_"     , TkUnderscore)
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

type LPAction a = SrcSpan -> String -> Int -> LP a
type Action = LPAction (Located Token)

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

----------------------------------------
-- Lexer predicates

type AlexPredicate = AlexAccPred ()

nextIsEOF :: AlexPredicate
nextIsEOF _ _ _ (_,_,_,b) = null b

----------------------------------------
-- Lexer actions

token :: Token -> Action
token tk span _buf _len = return (Loc span tk)

layoutToken :: Token -> Action
layoutToken tk span _buf _len = pushLexState layout >> return (Loc span tk)

push :: Int -> Action
push ls _span _buf _len = pushLexState ls >> lexToken

pop :: Action
pop _span _buf _len = popLexState >> lexToken

-- from GHC's lexer
identifier :: (String -> Token) -> Action
identifier idTk span buf len = let str = take len buf in
        case Map.lookup str reserved of
                Just tk -> maybeLayout tk >> return (Loc span tk)
                Nothing -> return (Loc span (idTk str))
    where
        -- certain keywords put us in the "layout" state, where we might
        -- add an opening curly brace.
        maybeLayout :: Token -> LP ()
        maybeLayout tk = case tk of
                TkWhere -> pushLexState layout
                _       -> return ()

newLayoutContext :: Token -> Action
newLayoutContext tk span _buf len = do
        -- trace "--------- {" popLexState
        popLexState
        (l,_,_,_) <- getInput
        let offset = srcLocCol l - len
        ctx <- gets lp_context
        case ctx of
                Layout prev_off : _
                        | prev_off >= offset ->
                                -- token is indented to the left of the previous context.
                                -- we must generate a {} sequence now.
                                pushLexState empty_layout >> return (Loc span tk)
                _ -> pushContext (Layout offset) >> return (Loc span tk)

emptyLayout :: Action
emptyLayout span _buf _len = do
        -- trace "--------- }" popLexState
        popLexState
        pushLexState bol
        return (Loc span TkVRCurly)

processLayout :: Action
processLayout span _buf _len = do
        pos <- getOffside span
        case pos of
                -- not popping the lexState, we might have a ';' or another '}' to insert
                -- inserting '}'
                LT -> -- trace "--------- }" $
                        popContext >> return (Loc span TkVRCurly)
                -- inserting ';'
                EQ -> do -- trace "--------- ;" $
                        popLexState >> return (Loc span TkSemicolon)
                -- keep lexing as same in line
                GT -> -- trace "--------- |" $
                        popLexState >> lexToken

--------------------------------------------------------------------------------
-- exposed functions

runLP :: String -> SrcLoc -> LP a -> Either (Located Error) (a, LPState)
runLP input loc = runIdentity . runErrorT . flip runStateT initialLPState . (\act -> return 2 >> act)
    where
        initialLPState = LPState
                { lp_buffer    = input
                , lp_last_char = '\n'
                , lp_loc       = loc
                , lp_bytes     = []
                -- , last_tk      = Nothing
                -- , last_loc     = srcLocSpan loc loc
                -- , last_len     = 0
                , lp_lex_state = [layout, 0]            -- start in 'layout' state to
                                                        -- introduce the global layout
                , lp_context   = []
                , lp_srcfiles  = []
                }

execLP :: String -> SrcLoc -> LP a -> Either (Located Error) LPState
execLP input loc = check . runLP input loc
    where
        check r = case r of
                Right (_, st) -> Right st
                Left  l       -> Left l

evalLP :: String -> SrcLoc -> LP a -> Either (Located Error) a
evalLP input loc = check . runLP input loc
    where
        check r = case r of
                Right (a, _st) -> Right a
                Left  l        -> Left l
 
----------------------------------------
-- lexer handling

lexer :: (Located Token -> LP a) -> LP a
lexer = (>>=) lexToken

lexToken :: LP (Located Token)
lexToken = do
        inp@(l,_c,_bs,b) <- getInput
        ls <- peekLexState
        -- trace (showAlex b (alexScan inp ls)) $
                -- traceShow l $
                        -- gets lp_lex_state >>= \lss -> trace (" states: " ++ show lss) $ gets lp_context   >>= \lcs -> trace ("context: " ++ show lcs) (return ())
        case alexScan inp ls of
                AlexEOF -> do
                        ctx <- gets lp_context
                        case ctx of
                                -- closing all the open layouts we had
                                _ : _  -> popContext >> return (Loc (srcLocSpan l l) TkVRCurly)
                                _      -> return (Loc (srcLocSpan l l) TkEOF)
                AlexError (l',_,_,c':_) ->
                        throwError (Loc (srcLocSpan l l') (LexError (LErrUnexpectedCharacter c')))
                AlexSkip  inp' _len -> setInput inp' >> lexToken
                AlexToken inp'@(l',_,_,_) len act -> setInput inp' >> act (srcLocSpan l l') b len
    -- where
    --     showAlex b as = case as of
    --             AlexEOF -> "AlexEOF"
    --             AlexError (l,_,_,_) -> "AlexError at " ++ show l
    --             AlexSkip _ l -> "AlexSkip (" ++ show l ++ ") " ++ show (take l b)
    --             AlexToken (_,_,_,_) l _-> "AlexToken (" ++ show l ++ ") " ++ show (take l b)

-- runLexer :: String -> Either (Located Error) [Located Token]
-- runLexer input = evalLP input (SrcLoc "" 1 1) lexTokens
--     where
--         lexTokens = do
--                 tk <- lexToken
--                 case tk of
--                         Loc _ TkEOF -> return [tk]
--                         _           -> lexTokens >>= return . ((:) tk)

}
