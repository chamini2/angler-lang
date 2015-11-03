{-# LANGUAGE RankNTypes #-}

module Language.Angler.SrcLoc
        ( SrcLoc(..)
        , startLoc
        , locMove

        , SrcSpan(..)
        , srcLocSpan

        , srcSpanSLoc
        , srcSpanELoc

        , spn_file  -- , srcSpanFile
        , spn_sline -- , srcSpanSLine
        , spn_scol  -- , srcSpanSCol
        , spn_eline -- , srcSpanELine
        , spn_ecol  -- , srcSpanECol

        , Located(..)
        , loc_span
        , loc_insd
        , srcSpanSpan
        , srcLocatedSpan
        ) where

import           PrettyShow

import           Control.Lens
import           Data.List    (intercalate)

import           Prelude      hiding (span)

-- is only used in the lexer
data SrcLoc
  = SrcLoc
        { srcLocFile    :: FilePath
        , srcLocLine    :: Int     -- line number, begins at 1
        , srcLocCol     :: Int     -- column number, begins at 1
        }

locationSeparator :: [String] -> String
locationSeparator strs = intercalate ":" strs ++ ":"

instance Show SrcLoc where
        show (SrcLoc f l c) = locationSeparator (f : fmap show [l, c])

startLoc :: FilePath -> SrcLoc
startLoc f = SrcLoc f 1 1

locMove :: SrcLoc -> Char -> SrcLoc
locMove (SrcLoc file l c) chr = case chr of
        '\t' -> SrcLoc file l      (((c+3) `div` 4)*4 + 1)  -- tabs have size 4
        '\n' -> SrcLoc file (l+1)  1
        _    -> SrcLoc file l      (c+1)

-- used for errors after the lexer
data SrcSpan
  = SrcSpanNoInfo
  | SrcSpanPoint FilePath Int Int
  | SrcSpanOneLine FilePath Int Int Int
  | SrcSpanMultiline FilePath Int Int Int Int

spn_file :: Lens' SrcSpan FilePath
spn_file spn_fn spn = wrap <$> spn_fn (srcSpanFile spn)
    where
        wrap :: FilePath -> SrcSpan
        wrap f = case spn of
                SrcSpanNoInfo                  -> SrcSpanNoInfo
                SrcSpanPoint     _ l  c        -> SrcSpanPoint     f l  c
                SrcSpanOneLine   _ l  c1 c2    -> SrcSpanOneLine   f l  c1 c2
                SrcSpanMultiline _ l1 c1 l2 c2 -> SrcSpanMultiline f l1 c1 l2 c2

spn_sline :: Lens' SrcSpan Int
spn_sline spn_fn spn = wrap <$> spn_fn (srcSpanSLine spn)
    where
        wrap :: Int -> SrcSpan
        wrap lin = case spn of
                SrcSpanNoInfo                 -> SrcSpanNoInfo
                SrcSpanPoint     f _ c        -> SrcSpanPoint     f lin c
                SrcSpanOneLine   f _ c1 c2    -> SrcSpanOneLine   f lin c1 c2
                SrcSpanMultiline f _ c1 l2 c2 -> SrcSpanMultiline f lin c1 l2 c2

spn_scol :: Lens' SrcSpan Int
spn_scol spn_fn spn = wrap <$> spn_fn (srcSpanSCol spn)
    where
        wrap :: Int -> SrcSpan
        wrap col = case spn of
                SrcSpanNoInfo                 -> SrcSpanNoInfo
                SrcSpanPoint     f l  _       -> SrcSpanPoint     f l  col
                SrcSpanOneLine   f l  _ c2    -> SrcSpanOneLine   f l  col c2
                SrcSpanMultiline f l1 _ l2 c2 -> SrcSpanMultiline f l1 col l2 c2

spn_eline :: Lens' SrcSpan Int
spn_eline spn_fn spn = wrap <$> spn_fn (srcSpanELine spn)
    where
        wrap :: Int -> SrcSpan
        wrap lin = case spn of
                SrcSpanNoInfo                  -> SrcSpanNoInfo
                SrcSpanPoint     f _  c        -> SrcSpanPoint     f lin  c
                SrcSpanOneLine   f _  c1 c2    -> SrcSpanOneLine   f lin  c1 c2
                SrcSpanMultiline f l1 c1 _  c2 -> SrcSpanMultiline f l1    c1 lin  c2

spn_ecol :: Lens' SrcSpan Int
spn_ecol spn_fn spn = wrap <$> spn_fn (srcSpanECol spn)
    where
        wrap :: Int -> SrcSpan
        wrap col = case spn of
                SrcSpanNoInfo                 -> SrcSpanNoInfo
                SrcSpanPoint     f l  _       -> SrcSpanPoint     f l  col
                SrcSpanOneLine   f l  c1 _    -> SrcSpanOneLine   f l  c1 col
                SrcSpanMultiline f l1 c1 l2 _ -> SrcSpanMultiline f l1 c1 l2 col


instance Show SrcSpan where
        show spn = case spn of
                SrcSpanNoInfo                  -> "<no location info>:"
                SrcSpanPoint f l c             -> locationSeparator (f : fmap show [(l, c)])
                SrcSpanOneLine f l c1 c2       -> locationSeparator (f : fmap show [(l, c1), (l, c2)])
                SrcSpanMultiline f l1 c1 l2 c2 -> locationSeparator (f : fmap show [(l1, c1), (l2, c2)])

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
srcLocSpan (SrcLoc f1 l1 c1) (SrcLoc f2 l2 c2) = if f1 == f2
        then case (compare l1 l2, compare c1 c2) of
                (EQ, EQ) -> SrcSpanPoint     f1 l1 c1
                (EQ, LT) -> SrcSpanOneLine   f1 l1 c1 c2
                (EQ, GT) -> SrcSpanOneLine   f1 l1 c2 c1
                (LT, _ ) -> SrcSpanMultiline f1 l1 c1 l2 c2
                (GT, _ ) -> SrcSpanMultiline f1 l2 c2 l1 c1
        else error "SrcLoc.srcLocSpan: different files for SrcSpan"


srcSpanFile :: SrcSpan -> FilePath
srcSpanFile span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanFile: SrcSpanNoInfo"
        SrcSpanPoint     f _l  _c          -> f
        SrcSpanOneLine   f _l  _sc _ec     -> f
        SrcSpanMultiline f _sl _sc _el _ec -> f

srcSpanSLine :: SrcSpan -> Int
srcSpanSLine span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanSLine: SrcSpanNoInfo"
        SrcSpanPoint     _f l  _c          -> l
        SrcSpanOneLine   _f l  _sc     _ec -> l
        SrcSpanMultiline _f sl _sc _el _ec -> sl

srcSpanSCol :: SrcSpan -> Int
srcSpanSCol span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanSCol: SrcSpanNoInfo"
        SrcSpanPoint     _f _l  c          -> c
        SrcSpanOneLine   _f _l  sc     _ec -> sc
        SrcSpanMultiline _f _sl sc _el _ec -> sc

srcSpanELine :: SrcSpan -> Int
srcSpanELine span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanELine: SrcSpanNoInfo"
        SrcSpanPoint     _f l   _c         -> l
        SrcSpanOneLine   _f l   _sc    _ec -> l
        SrcSpanMultiline _f _sl _sc el _ec -> el

srcSpanECol :: SrcSpan -> Int
srcSpanECol span = case span of
        SrcSpanNoInfo                      -> error "SrcLoc.srcSpanECol: SrcSpanNoInfo"
        SrcSpanPoint     _f _l  c          -> c
        SrcSpanOneLine   _f _l  _sc     ec -> ec
        SrcSpanMultiline _f _sl _sc _el ec -> ec

data Located e
  = Loc
        { _loc_span :: SrcSpan
        , _loc_insd :: e
        }

makeLenses ''Located

instance Show e => Show (Located e) where
        show (Loc spn e) = show spn ++ "\t\t" ++ show e

instance PrettyShow e => PrettyShow (Located e) where
        pshow (Loc spn e) = do
                string (show spn)
                raise >> line
                pshow e
                lower

srcSpanSpan :: SrcSpan -> SrcSpan -> SrcSpan
srcSpanSpan s e = case (s,e) of
        (SrcSpanNoInfo, _            ) -> e
        (_            , SrcSpanNoInfo) -> s
        _                              -> srcLocSpan (srcSpanSLoc s) (srcSpanELoc e)

srcLocatedSpan :: Located a -> Located b -> SrcSpan
srcLocatedSpan (Loc s _) (Loc e _) = srcSpanSpan s e
