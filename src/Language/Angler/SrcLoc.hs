module Language.Angler.SrcLoc
        ( SrcLoc(..)
        , SrcSpan(..)
        , srcSpanStartCol
        , Located(..)
        , locMove
        , srcLocSpan
        ) where

data SrcLoc
  = SrcLoc              { srcLocFile    :: String
                        , srcLocLine    :: Int     -- line number, begins at 1
                        , srcLocCol     :: Int     -- column number, begins at 1
                        }

locMove :: SrcLoc -> Char -> SrcLoc
locMove (SrcLoc file line col) chr = case chr of
        '\t' -> SrcLoc file line      (((col+3) `div` 4)*4 + 1)       -- tabs have size 4
        '\n' -> SrcLoc file (line+1)  1
        _    -> SrcLoc file line      (col+1)

data SrcSpan
  = SrcSpanNoInfo
  | SrcSpanPoint        { srcSpanFile   :: String
                        , srcSpanLine   :: Int
                        , srcSpanCol    :: Int
                        }
  | SrcSpanOneLine      { srcSpanFile   :: String
                        , srcSpanLine   :: Int
                        , srcSpanSCol   :: Int
                        , srcSpanECol   :: Int
                        }
  | SrcSpanMultiline    { srcSpanFile   :: String
                        , srcSpanSLine  :: Int
                        , srcSpanSCol   :: Int
                        , srcSpanELine  :: Int
                        , srcSpanECol   :: Int
                        }
  deriving Show

srcSpanStartCol :: SrcSpan -> Int
srcSpanStartCol span = case span of
        SrcSpanNoInfo                        -> error "SrcLoc.srcSpanStartCol: SrcSpanNoInfo"
        SrcSpanPoint { srcSpanCol = c }      -> c
        SrcSpanOneLine { srcSpanSCol = c }   -> c
        SrcSpanMultiline { srcSpanSCol = c } -> c

srcLocSpan :: SrcLoc -> SrcLoc -> SrcSpan
srcLocSpan (SrcLoc f l1 c1) (SrcLoc f' l2 c2) = case (compare f f', compare l1 l2, compare c1 c2) of
        (EQ, EQ, EQ) -> SrcSpanPoint f l1 c1
        (EQ, EQ, LT) -> SrcSpanOneLine f l1 c1 c2
        (EQ, EQ, GT) -> SrcSpanOneLine f l1 c2 c1
        (EQ, LT, _ ) -> SrcSpanMultiline f l1 c1 l2 c2
        (EQ, GT, _ ) -> SrcSpanMultiline f l2 c2 l1 c1
        (_ , _ , _ ) -> error "SrcLoc.srcLocSpan: different files for SrcSpan"

data Located e = Loc SrcSpan e
  deriving Show
