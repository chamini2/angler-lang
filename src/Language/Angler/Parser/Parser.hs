{-# OPTIONS_GHC -w #-}
module Language.Angler.Parser.Parser
        ( parseModule
        -- ,
        ) where

import           Language.Angler.Parser.Lexer (lexer)

import           Language.Angler.Error
import           Language.Angler.Parser.LP
import           Language.Angler.Parser.Token
import           Language.Angler.SrcLoc

import           Control.Applicative          (Alternative(..))
import           Data.Sequence                (Seq(..))
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal ((Located Token))
	| HappyErrorToken Int
	| HappyAbsSyn4 (())
	| HappyAbsSyn21 (Seq r)
	| HappyAbsSyn28 (Maybe r)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> ((Located Token))
	-> HappyState ((Located Token)) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState ((Located Token)) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82 :: () => Int -> ({-HappyReduction (LP) = -}
	   Int 
	-> ((Located Token))
	-> HappyState ((Located Token)) (HappyStk HappyAbsSyn -> (LP) HappyAbsSyn)
	-> [HappyState ((Located Token)) (HappyStk HappyAbsSyn -> (LP) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (LP) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51 :: () => ({-HappyReduction (LP) = -}
	   Int 
	-> ((Located Token))
	-> HappyState ((Located Token)) (HappyStk HappyAbsSyn -> (LP) HappyAbsSyn)
	-> [HappyState ((Located Token)) (HappyStk HappyAbsSyn -> (LP) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (LP) HappyAbsSyn)

action_0 (36) = happyShift action_4
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (34) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (57) = happyAccept
action_3 _ = happyFail

action_4 (39) = happyShift action_8
action_4 (7) = happyGoto action_5
action_4 (8) = happyGoto action_6
action_4 (31) = happyGoto action_7
action_4 _ = happyReduce_46

action_5 (34) = happyShift action_2
action_5 (4) = happyGoto action_18
action_5 (12) = happyGoto action_19
action_5 (13) = happyGoto action_20
action_5 (14) = happyGoto action_21
action_5 (22) = happyGoto action_22
action_5 _ = happyFail

action_6 (38) = happyShift action_17
action_6 _ = happyFail

action_7 (40) = happyShift action_16
action_7 (9) = happyGoto action_13
action_7 (27) = happyGoto action_14
action_7 (33) = happyGoto action_15
action_7 _ = happyReduce_38

action_8 (34) = happyShift action_2
action_8 (35) = happyShift action_12
action_8 (4) = happyGoto action_9
action_8 (5) = happyGoto action_10
action_8 (25) = happyGoto action_11
action_8 _ = happyFail

action_9 _ = happyReduce_3

action_10 _ = happyReduce_34

action_11 (51) = happyShift action_31
action_11 _ = happyReduce_6

action_12 _ = happyReduce_2

action_13 _ = happyReduce_50

action_14 _ = happyReduce_5

action_15 (38) = happyShift action_30
action_15 _ = happyFail

action_16 (34) = happyShift action_2
action_16 (35) = happyShift action_12
action_16 (4) = happyGoto action_9
action_16 (5) = happyGoto action_29
action_16 _ = happyFail

action_17 _ = happyReduce_47

action_18 (49) = happyShift action_27
action_18 (54) = happyShift action_28
action_18 (19) = happyGoto action_25
action_18 (28) = happyGoto action_26
action_18 _ = happyReduce_40

action_19 _ = happyReduce_28

action_20 _ = happyReduce_10

action_21 _ = happyReduce_11

action_22 (37) = happyShift action_23
action_22 (38) = happyShift action_24
action_22 _ = happyFail

action_23 _ = happyReduce_4

action_24 (34) = happyShift action_2
action_24 (4) = happyGoto action_18
action_24 (12) = happyGoto action_49
action_24 (13) = happyGoto action_20
action_24 (14) = happyGoto action_21
action_24 _ = happyFail

action_25 _ = happyReduce_41

action_26 (34) = happyShift action_2
action_26 (56) = happyShift action_48
action_26 (4) = happyGoto action_44
action_26 (18) = happyGoto action_45
action_26 (21) = happyGoto action_46
action_26 (32) = happyGoto action_47
action_26 _ = happyReduce_26

action_27 (34) = happyShift action_2
action_27 (35) = happyShift action_12
action_27 (52) = happyShift action_43
action_27 (4) = happyGoto action_9
action_27 (5) = happyGoto action_40
action_27 (15) = happyGoto action_41
action_27 (16) = happyGoto action_42
action_27 _ = happyFail

action_28 (34) = happyShift action_2
action_28 (4) = happyGoto action_37
action_28 (20) = happyGoto action_38
action_28 (24) = happyGoto action_39
action_28 _ = happyFail

action_29 (41) = happyShift action_36
action_29 (10) = happyGoto action_34
action_29 (29) = happyGoto action_35
action_29 _ = happyReduce_42

action_30 (40) = happyShift action_16
action_30 (9) = happyGoto action_33
action_30 _ = happyReduce_39

action_31 (34) = happyShift action_2
action_31 (35) = happyShift action_12
action_31 (4) = happyGoto action_9
action_31 (5) = happyGoto action_32
action_31 _ = happyFail

action_32 _ = happyReduce_35

action_33 _ = happyReduce_51

action_34 _ = happyReduce_43

action_35 (52) = happyShift action_61
action_35 (11) = happyGoto action_59
action_35 (30) = happyGoto action_60
action_35 _ = happyReduce_44

action_36 (34) = happyShift action_2
action_36 (4) = happyGoto action_58
action_36 _ = happyFail

action_37 (50) = happyShift action_57
action_37 _ = happyFail

action_38 _ = happyReduce_32

action_39 (51) = happyShift action_55
action_39 (55) = happyShift action_56
action_39 _ = happyFail

action_40 _ = happyReduce_18

action_41 (34) = happyShift action_2
action_41 (35) = happyShift action_12
action_41 (42) = happyShift action_54
action_41 (52) = happyShift action_43
action_41 (4) = happyGoto action_9
action_41 (5) = happyGoto action_40
action_41 (16) = happyGoto action_53
action_41 _ = happyReduce_12

action_42 _ = happyReduce_16

action_43 (34) = happyShift action_2
action_43 (35) = happyShift action_12
action_43 (52) = happyShift action_43
action_43 (4) = happyGoto action_9
action_43 (5) = happyGoto action_40
action_43 (15) = happyGoto action_52
action_43 (16) = happyGoto action_42
action_43 _ = happyFail

action_44 _ = happyReduce_22

action_45 _ = happyReduce_48

action_46 (50) = happyShift action_51
action_46 _ = happyFail

action_47 (34) = happyShift action_2
action_47 (56) = happyShift action_48
action_47 (4) = happyGoto action_44
action_47 (18) = happyGoto action_50
action_47 _ = happyReduce_27

action_48 _ = happyReduce_23

action_49 _ = happyReduce_29

action_50 _ = happyReduce_49

action_51 (34) = happyShift action_2
action_51 (35) = happyShift action_12
action_51 (52) = happyShift action_43
action_51 (4) = happyGoto action_9
action_51 (5) = happyGoto action_40
action_51 (15) = happyGoto action_68
action_51 (16) = happyGoto action_42
action_51 _ = happyFail

action_52 (34) = happyShift action_2
action_52 (35) = happyShift action_12
action_52 (52) = happyShift action_43
action_52 (53) = happyShift action_67
action_52 (4) = happyGoto action_9
action_52 (5) = happyGoto action_40
action_52 (16) = happyGoto action_53
action_52 _ = happyFail

action_53 _ = happyReduce_17

action_54 (36) = happyShift action_66
action_54 _ = happyFail

action_55 (34) = happyShift action_2
action_55 (4) = happyGoto action_37
action_55 (20) = happyGoto action_65
action_55 _ = happyFail

action_56 _ = happyReduce_24

action_57 (34) = happyShift action_2
action_57 (35) = happyShift action_12
action_57 (52) = happyShift action_43
action_57 (4) = happyGoto action_9
action_57 (5) = happyGoto action_40
action_57 (15) = happyGoto action_64
action_57 (16) = happyGoto action_42
action_57 _ = happyFail

action_58 _ = happyReduce_8

action_59 _ = happyReduce_45

action_60 _ = happyReduce_7

action_61 (34) = happyShift action_2
action_61 (4) = happyGoto action_62
action_61 (23) = happyGoto action_63
action_61 _ = happyFail

action_62 _ = happyReduce_30

action_63 (51) = happyShift action_74
action_63 (53) = happyShift action_75
action_63 _ = happyFail

action_64 (34) = happyShift action_2
action_64 (35) = happyShift action_12
action_64 (52) = happyShift action_43
action_64 (4) = happyGoto action_9
action_64 (5) = happyGoto action_40
action_64 (16) = happyGoto action_53
action_64 _ = happyReduce_25

action_65 _ = happyReduce_33

action_66 (34) = happyShift action_2
action_66 (4) = happyGoto action_18
action_66 (13) = happyGoto action_70
action_66 (14) = happyGoto action_71
action_66 (17) = happyGoto action_72
action_66 (26) = happyGoto action_73
action_66 _ = happyFail

action_67 _ = happyReduce_19

action_68 (34) = happyShift action_2
action_68 (35) = happyShift action_12
action_68 (42) = happyShift action_69
action_68 (52) = happyShift action_43
action_68 (4) = happyGoto action_9
action_68 (5) = happyGoto action_40
action_68 (16) = happyGoto action_53
action_68 _ = happyReduce_14

action_69 (36) = happyShift action_79
action_69 _ = happyFail

action_70 _ = happyReduce_20

action_71 _ = happyReduce_21

action_72 _ = happyReduce_36

action_73 (37) = happyShift action_77
action_73 (38) = happyShift action_78
action_73 _ = happyFail

action_74 (34) = happyShift action_2
action_74 (4) = happyGoto action_76
action_74 _ = happyFail

action_75 _ = happyReduce_9

action_76 _ = happyReduce_31

action_77 _ = happyReduce_13

action_78 (34) = happyShift action_2
action_78 (4) = happyGoto action_18
action_78 (13) = happyGoto action_70
action_78 (14) = happyGoto action_71
action_78 (17) = happyGoto action_81
action_78 _ = happyFail

action_79 (34) = happyShift action_2
action_79 (4) = happyGoto action_18
action_79 (13) = happyGoto action_70
action_79 (14) = happyGoto action_71
action_79 (17) = happyGoto action_72
action_79 (26) = happyGoto action_80
action_79 _ = happyFail

action_80 (37) = happyShift action_82
action_80 (38) = happyShift action_78
action_80 _ = happyFail

action_81 _ = happyReduce_37

action_82 _ = happyReduce_15

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (()
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_6 = happySpecReduce_2  8 happyReduction_6
happyReduction_6 _
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_7 = happyReduce 4 9 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (()
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  10 happyReduction_8
happyReduction_8 _
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_9 = happySpecReduce_3  11 happyReduction_9
happyReduction_9 _
	_
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_10 = happySpecReduce_1  12 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  12 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  13 happyReduction_12
happyReduction_12 _
	_
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_13 = happyReduce 7 13 happyReduction_13
happyReduction_13 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (()
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 14 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (()
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 9 14 happyReduction_15
happyReduction_15 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (()
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  15 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_17 = happySpecReduce_2  15 happyReduction_17
happyReduction_17 _
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_18 = happySpecReduce_1  16 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_19 = happySpecReduce_3  16 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  17 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_21 = happySpecReduce_1  17 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_22 = happySpecReduce_1  18 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_23 = happySpecReduce_1  18 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_24 = happySpecReduce_3  19 happyReduction_24
happyReduction_24 _
	_
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_25 = happySpecReduce_3  20 happyReduction_25
happyReduction_25 _
	_
	_
	 =  HappyAbsSyn4
		 (()
	)

happyReduce_26 = happySpecReduce_0  21 happyReduction_26
happyReduction_26  =  HappyAbsSyn21
		 (empty
	)

happyReduce_27 = happySpecReduce_1  21 happyReduction_27
happyReduction_27 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  22 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (pure happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  22 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <|> pure happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  23 happyReduction_30
happyReduction_30 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (pure happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  23 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <|> pure happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  24 happyReduction_32
happyReduction_32 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (pure happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  24 happyReduction_33
happyReduction_33 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <|> pure happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  25 happyReduction_34
happyReduction_34 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (pure happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  25 happyReduction_35
happyReduction_35 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <|> pure happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  26 happyReduction_36
happyReduction_36 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (pure happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  26 happyReduction_37
happyReduction_37 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <|> pure happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  27 happyReduction_38
happyReduction_38  =  HappyAbsSyn21
		 (empty
	)

happyReduce_39 = happySpecReduce_2  27 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  28 happyReduction_40
happyReduction_40  =  HappyAbsSyn28
		 (Nothing
	)

happyReduce_41 = happySpecReduce_1  28 happyReduction_41
happyReduction_41 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn28
		 (Just happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  29 happyReduction_42
happyReduction_42  =  HappyAbsSyn28
		 (Nothing
	)

happyReduce_43 = happySpecReduce_1  29 happyReduction_43
happyReduction_43 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn28
		 (Just happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_0  30 happyReduction_44
happyReduction_44  =  HappyAbsSyn28
		 (Nothing
	)

happyReduce_45 = happySpecReduce_1  30 happyReduction_45
happyReduction_45 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn28
		 (Just happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  31 happyReduction_46
happyReduction_46  =  HappyAbsSyn28
		 (Nothing
	)

happyReduce_47 = happySpecReduce_2  31 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn28
		 (Just happy_var_1
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  32 happyReduction_48
happyReduction_48 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (pure happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  32 happyReduction_49
happyReduction_49 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <|> pure happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  33 happyReduction_50
happyReduction_50 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn21
		 (pure happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  33 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 <|> pure happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Loc _ TkEOF -> action 57 57 tk (HappyState action) sts stk;
	Loc _ (TkIdentifier _) -> cont 34;
	Loc _ (TkQualified _) -> cont 35;
	Loc _ TkVLCurly -> cont 36;
	Loc _ TkVRCurly -> cont 37;
	Loc _ TkSemicolon -> cont 38;
	Loc _ TkExport -> cont 39;
	Loc _ TkImport -> cont 40;
	Loc _ TkAs -> cont 41;
	Loc _ TkWhere -> cont 42;
	Loc _ TkForall -> cont 43;
	Loc _ TkExists -> cont 44;
	Loc _ TkWith -> cont 45;
	Loc _ TkOn -> cont 46;
	Loc _ TkIs -> cont 47;
	Loc _ TkArrow -> cont 48;
	Loc _ TkColon -> cont 49;
	Loc _ TkEquals -> cont 50;
	Loc _ TkComma -> cont 51;
	Loc _ TkLParen -> cont 52;
	Loc _ TkRParen -> cont 53;
	Loc _ TkLCurly -> cont 54;
	Loc _ TkRCurly -> cont 55;
	Loc _ TkUnderscore -> cont 56;
	_ -> happyError' tk
	})

happyError_ 57 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => LP a -> (a -> LP b) -> LP b
happyThen = (>>=)
happyReturn :: () => a -> LP a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> LP a
happyReturn1 = happyReturn
happyError' :: () => ((Located Token)) -> LP a
happyError' tk = parseError tk

parseModule = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: Located Token -> LP a
parseError (Loc l tk) = throwError (Loc l (ParseError (PErr (show tk))))
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 


{-# LINE 13 "templates/GenericTemplate.hs" #-}


{-# LINE 46 "templates/GenericTemplate.hs" #-}









{-# LINE 67 "templates/GenericTemplate.hs" #-}


{-# LINE 77 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

