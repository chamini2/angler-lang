module Language.Angler.SrcLoc
        ( SrcLoc(..)
        , locMove

        , SrcSpan(..)
        , srcLocSpan

        , srcSpanSLoc
        , srcSpanELoc

        , srcSpanSLine
        , srcSpanSCol
        , srcSpanELine
        , srcSpanECol

        , Located(..)
        , unlocate
        , srcLocatedSpan
        ) where

import           Prelude hiding (span)

-- is only used in the lexer
data SrcLoc
  = SrcLoc
        { srcLocFile    :: String
        , srcLocLine    :: Int     -- line number, begins at 1
        , srcLocCol     :: Int     -- column number, begins at 1
        }
  deriving Show

locMove :: SrcLoc -> Char -> SrcLoc
locMove (SrcLoc file line col) chr = case chr of
        '\t' -> SrcLoc file line      (((col+3) `div` 4)*4 + 1)       -- tabs have size 4
        '\n' -> SrcLoc file (line+1)  1
        _    -> SrcLoc file line      (col+1)

-- used for errors after the lexer
data SrcSpan
  = SrcSpanNoInfo
  | SrcSpanPoint String Int Int
        -- { srcSpanFile   :: String
        -- , srcSpanLine   :: Int
        -- , srcSpanCol    :: Int
        -- }
  | SrcSpanOneLine String Int Int Int
        -- { srcSpanFile   :: String
        -- , srcSpanLine   :: Int
        -- , srcSpanSCol   :: Int
        -- , srcSpanECol   :: Int
        -- }
  | SrcSpanMultiline String Int Int Int Int
        -- { srcSpanFile   :: String
        -- , srcSpanSLine  :: Int
        -- , srcSpanSCol   :: Int
        -- , srcSpanELine  :: Int
        -- , srcSpanECol   :: Int
        -- }
  deriving Show

srcSpanSLoc :: SrcSpan -> SrcLoc
srcSpanSLoc span = case span of
        SrcSpanNoInfo                    -> error "SrcLoc.srcSpanSLoc: SrcSpanNoInfo"
        SrcSpanPoint     f l  c          -> SrcLoc f l c
        SrcSpanOneLine   f l  sc _ec     -> SrcLoc f l sc
        SrcSpanMultiline f sl sc _el _ec -> SrcLoc f sl sc

srcSpanELoc :: SrcSpan -> SrcLoc
srcSpanELoc span = case span of
        SrcSpanNoInfo                    -> error "SrcLoc.srcSpanSLoc: SrcSpanNoInfo"
        SrcSpanPoint     f l   c         -> SrcLoc f l c
        SrcSpanOneLine   f l   _sc ec    -> SrcLoc f l ec
        SrcSpanMultiline f _sl _sc el ec -> SrcLoc f el ec

srcLocSpan :: SrcLoc -> SrcLoc -> SrcSpan
srcLocSpan (SrcLoc f l1 c1) (SrcLoc f' l2 c2) = case (compare f f', compare l1 l2, compare c1 c2) of
        (EQ, EQ, EQ) -> SrcSpanPoint f l1 c1
        (EQ, EQ, LT) -> SrcSpanOneLine f l1 c1 c2
        (EQ, EQ, GT) -> SrcSpanOneLine f l1 c2 c1
        (EQ, LT, _ ) -> SrcSpanMultiline f l1 c1 l2 c2
        (EQ, GT, _ ) -> SrcSpanMultiline f l2 c2 l1 c1
        (_ , _ , _ ) -> error "SrcLoc.srcLocSpan: different files for SrcSpan"


srcSpanSLine :: SrcSpan -> Int
srcSpanSLine span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanSLine: SrcSpanNoInfo"
        SrcSpanPoint     _f l  _c          -> l
        SrcSpanOneLine   _f l  _sc _ec     -> l
        SrcSpanMultiline _f sl _sc _el _ec -> sl

srcSpanSCol :: SrcSpan -> Int
srcSpanSCol span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanSCol: SrcSpanNoInfo"
        SrcSpanPoint     _f _l  c          -> c
        SrcSpanOneLine   _f _l  sc _ec     -> sc
        SrcSpanMultiline _f _sl sc _el _ec -> sc

srcSpanELine :: SrcSpan -> Int
srcSpanELine span = case span of
        SrcSpanNoInfo                       -> error "SrcLoc.srcSpanELine: SrcSpanNoInfo"
        SrcSpanPoint     _f l   _c          -> l
        SrcSpanOneLine   _f l   _sc _ec     -> l
        SrcSpanMultiline _f _sl _sc el  _ec -> el

srcSpanECol :: SrcSpan -> Int
srcSpanECol span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanECol: SrcSpanNoInfo"
        SrcSpanPoint     _f _l  c          -> c
        SrcSpanOneLine   _f _l  _sc ec     -> ec
        SrcSpanMultiline _f _sl _sc _el ec -> ec

data Located e = Loc SrcSpan e
  deriving Show

unlocate :: Located e -> e
unlocate (Loc _ e) = e

srcLocatedSpan :: Located a -> Located b -> SrcSpan
srcLocatedSpan (Loc s _) (Loc e _) = srcLocSpan (srcSpanSLoc s) (srcSpanELoc e)
