{-# OPTIONS_GHC -w #-}
module Lang.Parser where

import Lang.Tokens
import qualified Lang.Lexer as L
import Control.Monad.Error
import qualified Data.Sequence                 as Seq
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,801) ([16384,3066,29056,1017,0,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,32,0,0,0,0,57344,13305,32,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,16384,2074,29056,1017,53760,64,52108,31,0,0,4,0,0,0,0,41984,129,38680,63,3360,49156,64696,1,8297,50688,4069,18432,259,12080,127,0,0,0,0,16594,35840,8139,36864,518,31840,254,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,65028,0,30720,3326,12,0,0,2048,0,0,0,0,0,61440,6652,16,0,0,0,0,0,32572,1542,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,38912,48,0,0,0,0,0,840,12289,32559,0,15364,1663,4,8192,63968,8243,0,64,0,0,32768,4148,58112,2034,41984,129,38680,63,3360,49156,64696,1,8297,50688,4069,18432,259,11824,127,6720,32776,63857,3,16594,35840,8139,36864,518,23648,254,13440,16,62179,7,33188,6144,16279,8192,1037,47296,508,26880,32,58822,15,840,12289,32558,16384,2074,29056,1017,53760,64,52108,31,0,0,0,0,0,0,0,0,0,0,0,0,12446,0,0,61440,388,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,0,0,33792,1,0,0,3104,0,0,12288,97,0,0,2432,3,0,0,0,0,32768,4148,58112,2034,41984,129,38680,63,3360,49156,64696,1,0,0,0,0,0,0,0,6720,32776,63857,3,0,0,0,0,0,512,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,53760,64,52108,31,1680,24578,65116,0,0,0,0,16384,62400,16487,0,0,0,0,0,8297,50688,4069,4096,59264,32975,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pig","Prog","Drct","Expr","If","IfList","Seq","ListLit","Appl","List","Atom","Val","if","elif","else","while","do","print","read","exit","help","rm","clear","load","'+'","'-'","'*'","'/'","'=>'","'='","'^'","'<'","'>'","'=='","'!='","'<>'","'><'","'-<'","'>-'","'&&'","'||'","')'","'('","'}'","'{'","']'","'['","','","';'","true","false","null","NUM","CHAR","STR","VAR","%eof"]
        bit_start = st * 59
        bit_end = (st + 1) * 59
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..58]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (15) = happyShift action_16
action_0 (18) = happyShift action_17
action_0 (20) = happyShift action_18
action_0 (21) = happyShift action_19
action_0 (22) = happyShift action_3
action_0 (23) = happyShift action_4
action_0 (24) = happyShift action_5
action_0 (25) = happyShift action_6
action_0 (26) = happyShift action_7
action_0 (28) = happyShift action_20
action_0 (40) = happyShift action_21
action_0 (41) = happyShift action_22
action_0 (45) = happyShift action_23
action_0 (46) = happyShift action_24
action_0 (47) = happyShift action_25
action_0 (49) = happyShift action_26
action_0 (52) = happyShift action_27
action_0 (53) = happyShift action_28
action_0 (54) = happyShift action_29
action_0 (55) = happyShift action_30
action_0 (56) = happyShift action_31
action_0 (57) = happyShift action_32
action_0 (58) = happyShift action_33
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_9
action_0 (7) = happyGoto action_10
action_0 (9) = happyGoto action_11
action_0 (10) = happyGoto action_12
action_0 (11) = happyGoto action_13
action_0 (13) = happyGoto action_14
action_0 (14) = happyGoto action_15
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (22) = happyShift action_3
action_1 (23) = happyShift action_4
action_1 (24) = happyShift action_5
action_1 (25) = happyShift action_6
action_1 (26) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 (58) = happyShift action_67
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 (57) = happyShift action_66
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (59) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (27) = happyShift action_52
action_9 (28) = happyShift action_53
action_9 (29) = happyShift action_54
action_9 (30) = happyShift action_55
action_9 (33) = happyShift action_56
action_9 (34) = happyShift action_57
action_9 (35) = happyShift action_58
action_9 (36) = happyShift action_59
action_9 (37) = happyShift action_60
action_9 (38) = happyShift action_61
action_9 (39) = happyShift action_62
action_9 (42) = happyShift action_63
action_9 (43) = happyShift action_64
action_9 (51) = happyShift action_65
action_9 _ = happyReduce_2

action_10 _ = happyReduce_10

action_11 _ = happyReduce_11

action_12 _ = happyReduce_9

action_13 (31) = happyShift action_51
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_8

action_15 _ = happyReduce_47

action_16 (15) = happyShift action_16
action_16 (18) = happyShift action_17
action_16 (20) = happyShift action_18
action_16 (21) = happyShift action_19
action_16 (28) = happyShift action_20
action_16 (40) = happyShift action_21
action_16 (41) = happyShift action_22
action_16 (45) = happyShift action_23
action_16 (46) = happyShift action_24
action_16 (47) = happyShift action_25
action_16 (49) = happyShift action_26
action_16 (52) = happyShift action_27
action_16 (53) = happyShift action_28
action_16 (54) = happyShift action_29
action_16 (55) = happyShift action_30
action_16 (56) = happyShift action_31
action_16 (57) = happyShift action_32
action_16 (58) = happyShift action_33
action_16 (6) = happyGoto action_49
action_16 (7) = happyGoto action_10
action_16 (8) = happyGoto action_50
action_16 (9) = happyGoto action_11
action_16 (10) = happyGoto action_12
action_16 (11) = happyGoto action_13
action_16 (13) = happyGoto action_14
action_16 (14) = happyGoto action_15
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (15) = happyShift action_16
action_17 (18) = happyShift action_17
action_17 (20) = happyShift action_18
action_17 (21) = happyShift action_19
action_17 (28) = happyShift action_20
action_17 (40) = happyShift action_21
action_17 (41) = happyShift action_22
action_17 (45) = happyShift action_23
action_17 (46) = happyShift action_24
action_17 (47) = happyShift action_25
action_17 (49) = happyShift action_26
action_17 (52) = happyShift action_27
action_17 (53) = happyShift action_28
action_17 (54) = happyShift action_29
action_17 (55) = happyShift action_30
action_17 (56) = happyShift action_31
action_17 (57) = happyShift action_32
action_17 (58) = happyShift action_33
action_17 (6) = happyGoto action_48
action_17 (7) = happyGoto action_10
action_17 (9) = happyGoto action_11
action_17 (10) = happyGoto action_12
action_17 (11) = happyGoto action_13
action_17 (13) = happyGoto action_14
action_17 (14) = happyGoto action_15
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (45) = happyShift action_47
action_18 (11) = happyGoto action_46
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_15

action_20 (15) = happyShift action_16
action_20 (18) = happyShift action_17
action_20 (20) = happyShift action_18
action_20 (21) = happyShift action_19
action_20 (28) = happyShift action_20
action_20 (40) = happyShift action_21
action_20 (41) = happyShift action_22
action_20 (45) = happyShift action_23
action_20 (46) = happyShift action_24
action_20 (47) = happyShift action_25
action_20 (49) = happyShift action_26
action_20 (52) = happyShift action_27
action_20 (53) = happyShift action_28
action_20 (54) = happyShift action_29
action_20 (55) = happyShift action_30
action_20 (56) = happyShift action_31
action_20 (57) = happyShift action_32
action_20 (58) = happyShift action_33
action_20 (6) = happyGoto action_45
action_20 (7) = happyGoto action_10
action_20 (9) = happyGoto action_11
action_20 (10) = happyGoto action_12
action_20 (11) = happyGoto action_13
action_20 (13) = happyGoto action_14
action_20 (14) = happyGoto action_15
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (15) = happyShift action_16
action_21 (18) = happyShift action_17
action_21 (20) = happyShift action_18
action_21 (21) = happyShift action_19
action_21 (28) = happyShift action_20
action_21 (40) = happyShift action_21
action_21 (41) = happyShift action_22
action_21 (45) = happyShift action_23
action_21 (46) = happyShift action_24
action_21 (47) = happyShift action_25
action_21 (49) = happyShift action_26
action_21 (52) = happyShift action_27
action_21 (53) = happyShift action_28
action_21 (54) = happyShift action_29
action_21 (55) = happyShift action_30
action_21 (56) = happyShift action_31
action_21 (57) = happyShift action_32
action_21 (58) = happyShift action_33
action_21 (6) = happyGoto action_44
action_21 (7) = happyGoto action_10
action_21 (9) = happyGoto action_11
action_21 (10) = happyGoto action_12
action_21 (11) = happyGoto action_13
action_21 (13) = happyGoto action_14
action_21 (14) = happyGoto action_15
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (15) = happyShift action_16
action_22 (18) = happyShift action_17
action_22 (20) = happyShift action_18
action_22 (21) = happyShift action_19
action_22 (28) = happyShift action_20
action_22 (40) = happyShift action_21
action_22 (41) = happyShift action_22
action_22 (45) = happyShift action_23
action_22 (46) = happyShift action_24
action_22 (47) = happyShift action_25
action_22 (49) = happyShift action_26
action_22 (52) = happyShift action_27
action_22 (53) = happyShift action_28
action_22 (54) = happyShift action_29
action_22 (55) = happyShift action_30
action_22 (56) = happyShift action_31
action_22 (57) = happyShift action_32
action_22 (58) = happyShift action_33
action_22 (6) = happyGoto action_43
action_22 (7) = happyGoto action_10
action_22 (9) = happyGoto action_11
action_22 (10) = happyGoto action_12
action_22 (11) = happyGoto action_13
action_22 (13) = happyGoto action_14
action_22 (14) = happyGoto action_15
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (15) = happyShift action_16
action_23 (18) = happyShift action_17
action_23 (20) = happyShift action_18
action_23 (21) = happyShift action_19
action_23 (28) = happyShift action_20
action_23 (40) = happyShift action_21
action_23 (41) = happyShift action_22
action_23 (44) = happyShift action_42
action_23 (45) = happyShift action_23
action_23 (46) = happyShift action_24
action_23 (47) = happyShift action_25
action_23 (49) = happyShift action_26
action_23 (52) = happyShift action_27
action_23 (53) = happyShift action_28
action_23 (54) = happyShift action_29
action_23 (55) = happyShift action_30
action_23 (56) = happyShift action_31
action_23 (57) = happyShift action_32
action_23 (58) = happyShift action_33
action_23 (6) = happyGoto action_40
action_23 (7) = happyGoto action_10
action_23 (9) = happyGoto action_11
action_23 (10) = happyGoto action_12
action_23 (11) = happyGoto action_13
action_23 (12) = happyGoto action_41
action_23 (13) = happyGoto action_14
action_23 (14) = happyGoto action_15
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_39

action_25 (15) = happyShift action_16
action_25 (18) = happyShift action_17
action_25 (20) = happyShift action_18
action_25 (21) = happyShift action_19
action_25 (28) = happyShift action_20
action_25 (40) = happyShift action_21
action_25 (41) = happyShift action_22
action_25 (45) = happyShift action_23
action_25 (46) = happyShift action_24
action_25 (47) = happyShift action_25
action_25 (49) = happyShift action_26
action_25 (52) = happyShift action_27
action_25 (53) = happyShift action_28
action_25 (54) = happyShift action_29
action_25 (55) = happyShift action_30
action_25 (56) = happyShift action_31
action_25 (57) = happyShift action_32
action_25 (58) = happyShift action_33
action_25 (6) = happyGoto action_38
action_25 (7) = happyGoto action_10
action_25 (9) = happyGoto action_39
action_25 (10) = happyGoto action_12
action_25 (11) = happyGoto action_13
action_25 (13) = happyGoto action_14
action_25 (14) = happyGoto action_15
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (15) = happyShift action_16
action_26 (18) = happyShift action_17
action_26 (20) = happyShift action_18
action_26 (21) = happyShift action_19
action_26 (28) = happyShift action_20
action_26 (40) = happyShift action_21
action_26 (41) = happyShift action_22
action_26 (45) = happyShift action_23
action_26 (46) = happyShift action_24
action_26 (47) = happyShift action_25
action_26 (48) = happyShift action_37
action_26 (49) = happyShift action_26
action_26 (52) = happyShift action_27
action_26 (53) = happyShift action_28
action_26 (54) = happyShift action_29
action_26 (55) = happyShift action_30
action_26 (56) = happyShift action_31
action_26 (57) = happyShift action_32
action_26 (58) = happyShift action_33
action_26 (6) = happyGoto action_35
action_26 (7) = happyGoto action_10
action_26 (9) = happyGoto action_11
action_26 (10) = happyGoto action_12
action_26 (11) = happyGoto action_13
action_26 (12) = happyGoto action_36
action_26 (13) = happyGoto action_14
action_26 (14) = happyGoto action_15
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_48

action_28 _ = happyReduce_49

action_29 _ = happyReduce_50

action_30 _ = happyReduce_51

action_31 _ = happyReduce_52

action_32 _ = happyReduce_53

action_33 (45) = happyShift action_34
action_33 _ = happyReduce_46

action_34 (45) = happyShift action_47
action_34 (52) = happyShift action_27
action_34 (53) = happyShift action_28
action_34 (54) = happyShift action_29
action_34 (55) = happyShift action_30
action_34 (56) = happyShift action_31
action_34 (57) = happyShift action_32
action_34 (58) = happyShift action_91
action_34 (11) = happyGoto action_13
action_34 (13) = happyGoto action_90
action_34 (14) = happyGoto action_15
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (27) = happyShift action_52
action_35 (28) = happyShift action_53
action_35 (29) = happyShift action_54
action_35 (30) = happyShift action_55
action_35 (33) = happyShift action_56
action_35 (34) = happyShift action_57
action_35 (35) = happyShift action_58
action_35 (36) = happyShift action_59
action_35 (37) = happyShift action_60
action_35 (38) = happyShift action_61
action_35 (39) = happyShift action_62
action_35 (42) = happyShift action_63
action_35 (43) = happyShift action_64
action_35 (50) = happyShift action_88
action_35 (51) = happyShift action_65
action_35 _ = happyReduce_45

action_36 (48) = happyShift action_89
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_41

action_38 (27) = happyShift action_52
action_38 (28) = happyShift action_53
action_38 (29) = happyShift action_54
action_38 (30) = happyShift action_55
action_38 (33) = happyShift action_56
action_38 (34) = happyShift action_57
action_38 (35) = happyShift action_58
action_38 (36) = happyShift action_59
action_38 (37) = happyShift action_60
action_38 (38) = happyShift action_61
action_38 (39) = happyShift action_62
action_38 (42) = happyShift action_63
action_38 (43) = happyShift action_64
action_38 (51) = happyShift action_65
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (27) = happyReduce_37
action_39 (28) = happyReduce_37
action_39 (29) = happyReduce_37
action_39 (30) = happyReduce_37
action_39 (33) = happyReduce_37
action_39 (34) = happyReduce_37
action_39 (35) = happyReduce_37
action_39 (36) = happyReduce_37
action_39 (37) = happyReduce_37
action_39 (38) = happyReduce_37
action_39 (39) = happyReduce_37
action_39 (42) = happyReduce_37
action_39 (43) = happyReduce_37
action_39 (51) = happyReduce_37
action_39 _ = happyReduce_37

action_40 (27) = happyShift action_52
action_40 (28) = happyShift action_53
action_40 (29) = happyShift action_54
action_40 (30) = happyShift action_55
action_40 (33) = happyShift action_56
action_40 (34) = happyShift action_57
action_40 (35) = happyShift action_58
action_40 (36) = happyShift action_59
action_40 (37) = happyShift action_60
action_40 (38) = happyShift action_61
action_40 (39) = happyShift action_62
action_40 (42) = happyShift action_63
action_40 (43) = happyShift action_64
action_40 (44) = happyShift action_87
action_40 (50) = happyShift action_88
action_40 (51) = happyShift action_65
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (44) = happyShift action_86
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_43

action_43 (27) = happyShift action_52
action_43 (28) = happyShift action_53
action_43 (29) = happyShift action_54
action_43 (30) = happyShift action_55
action_43 (33) = happyShift action_56
action_43 (34) = happyShift action_57
action_43 (35) = happyShift action_58
action_43 (36) = happyShift action_59
action_43 (37) = happyShift action_60
action_43 (38) = happyShift action_61
action_43 (39) = happyShift action_62
action_43 (42) = happyShift action_63
action_43 (43) = happyShift action_64
action_43 (51) = happyShift action_65
action_43 _ = happyReduce_30

action_44 (27) = happyShift action_52
action_44 (28) = happyShift action_53
action_44 (29) = happyShift action_54
action_44 (30) = happyShift action_55
action_44 (33) = happyShift action_56
action_44 (34) = happyShift action_57
action_44 (35) = happyShift action_58
action_44 (36) = happyShift action_59
action_44 (37) = happyShift action_60
action_44 (38) = happyShift action_61
action_44 (39) = happyShift action_62
action_44 (42) = happyShift action_63
action_44 (43) = happyShift action_64
action_44 (51) = happyShift action_65
action_44 _ = happyReduce_29

action_45 (29) = happyShift action_54
action_45 (30) = happyShift action_55
action_45 (33) = happyShift action_56
action_45 (34) = happyShift action_57
action_45 (35) = happyShift action_58
action_45 (36) = happyShift action_59
action_45 (37) = happyShift action_60
action_45 (38) = happyShift action_61
action_45 (39) = happyShift action_62
action_45 (51) = happyShift action_65
action_45 _ = happyReduce_22

action_46 _ = happyReduce_16

action_47 (15) = happyShift action_16
action_47 (18) = happyShift action_17
action_47 (20) = happyShift action_18
action_47 (21) = happyShift action_19
action_47 (28) = happyShift action_20
action_47 (40) = happyShift action_21
action_47 (41) = happyShift action_22
action_47 (44) = happyShift action_42
action_47 (45) = happyShift action_23
action_47 (46) = happyShift action_24
action_47 (47) = happyShift action_25
action_47 (49) = happyShift action_26
action_47 (52) = happyShift action_27
action_47 (53) = happyShift action_28
action_47 (54) = happyShift action_29
action_47 (55) = happyShift action_30
action_47 (56) = happyShift action_31
action_47 (57) = happyShift action_32
action_47 (58) = happyShift action_33
action_47 (6) = happyGoto action_35
action_47 (7) = happyGoto action_10
action_47 (9) = happyGoto action_11
action_47 (10) = happyGoto action_12
action_47 (11) = happyGoto action_13
action_47 (12) = happyGoto action_41
action_47 (13) = happyGoto action_14
action_47 (14) = happyGoto action_15
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (19) = happyShift action_85
action_48 (27) = happyShift action_52
action_48 (28) = happyShift action_53
action_48 (29) = happyShift action_54
action_48 (30) = happyShift action_55
action_48 (33) = happyShift action_56
action_48 (34) = happyShift action_57
action_48 (35) = happyShift action_58
action_48 (36) = happyShift action_59
action_48 (37) = happyShift action_60
action_48 (38) = happyShift action_61
action_48 (39) = happyShift action_62
action_48 (42) = happyShift action_63
action_48 (43) = happyShift action_64
action_48 (51) = happyShift action_65
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (19) = happyShift action_84
action_49 (27) = happyShift action_52
action_49 (28) = happyShift action_53
action_49 (29) = happyShift action_54
action_49 (30) = happyShift action_55
action_49 (33) = happyShift action_56
action_49 (34) = happyShift action_57
action_49 (35) = happyShift action_58
action_49 (36) = happyShift action_59
action_49 (37) = happyShift action_60
action_49 (38) = happyShift action_61
action_49 (39) = happyShift action_62
action_49 (42) = happyShift action_63
action_49 (43) = happyShift action_64
action_49 (51) = happyShift action_65
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (17) = happyShift action_83
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (15) = happyShift action_16
action_51 (18) = happyShift action_17
action_51 (20) = happyShift action_18
action_51 (21) = happyShift action_19
action_51 (28) = happyShift action_20
action_51 (40) = happyShift action_21
action_51 (41) = happyShift action_22
action_51 (45) = happyShift action_23
action_51 (46) = happyShift action_24
action_51 (47) = happyShift action_25
action_51 (49) = happyShift action_26
action_51 (52) = happyShift action_27
action_51 (53) = happyShift action_28
action_51 (54) = happyShift action_29
action_51 (55) = happyShift action_30
action_51 (56) = happyShift action_31
action_51 (57) = happyShift action_32
action_51 (58) = happyShift action_33
action_51 (6) = happyGoto action_82
action_51 (7) = happyGoto action_10
action_51 (9) = happyGoto action_11
action_51 (10) = happyGoto action_12
action_51 (11) = happyGoto action_13
action_51 (13) = happyGoto action_14
action_51 (14) = happyGoto action_15
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (15) = happyShift action_16
action_52 (18) = happyShift action_17
action_52 (20) = happyShift action_18
action_52 (21) = happyShift action_19
action_52 (28) = happyShift action_20
action_52 (40) = happyShift action_21
action_52 (41) = happyShift action_22
action_52 (45) = happyShift action_23
action_52 (46) = happyShift action_24
action_52 (47) = happyShift action_25
action_52 (49) = happyShift action_26
action_52 (52) = happyShift action_27
action_52 (53) = happyShift action_28
action_52 (54) = happyShift action_29
action_52 (55) = happyShift action_30
action_52 (56) = happyShift action_31
action_52 (57) = happyShift action_32
action_52 (58) = happyShift action_33
action_52 (6) = happyGoto action_81
action_52 (7) = happyGoto action_10
action_52 (9) = happyGoto action_11
action_52 (10) = happyGoto action_12
action_52 (11) = happyGoto action_13
action_52 (13) = happyGoto action_14
action_52 (14) = happyGoto action_15
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (15) = happyShift action_16
action_53 (18) = happyShift action_17
action_53 (20) = happyShift action_18
action_53 (21) = happyShift action_19
action_53 (28) = happyShift action_20
action_53 (40) = happyShift action_21
action_53 (41) = happyShift action_22
action_53 (45) = happyShift action_23
action_53 (46) = happyShift action_24
action_53 (47) = happyShift action_25
action_53 (49) = happyShift action_26
action_53 (52) = happyShift action_27
action_53 (53) = happyShift action_28
action_53 (54) = happyShift action_29
action_53 (55) = happyShift action_30
action_53 (56) = happyShift action_31
action_53 (57) = happyShift action_32
action_53 (58) = happyShift action_33
action_53 (6) = happyGoto action_80
action_53 (7) = happyGoto action_10
action_53 (9) = happyGoto action_11
action_53 (10) = happyGoto action_12
action_53 (11) = happyGoto action_13
action_53 (13) = happyGoto action_14
action_53 (14) = happyGoto action_15
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (15) = happyShift action_16
action_54 (18) = happyShift action_17
action_54 (20) = happyShift action_18
action_54 (21) = happyShift action_19
action_54 (28) = happyShift action_20
action_54 (40) = happyShift action_21
action_54 (41) = happyShift action_22
action_54 (45) = happyShift action_23
action_54 (46) = happyShift action_24
action_54 (47) = happyShift action_25
action_54 (49) = happyShift action_26
action_54 (52) = happyShift action_27
action_54 (53) = happyShift action_28
action_54 (54) = happyShift action_29
action_54 (55) = happyShift action_30
action_54 (56) = happyShift action_31
action_54 (57) = happyShift action_32
action_54 (58) = happyShift action_33
action_54 (6) = happyGoto action_79
action_54 (7) = happyGoto action_10
action_54 (9) = happyGoto action_11
action_54 (10) = happyGoto action_12
action_54 (11) = happyGoto action_13
action_54 (13) = happyGoto action_14
action_54 (14) = happyGoto action_15
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (15) = happyShift action_16
action_55 (18) = happyShift action_17
action_55 (20) = happyShift action_18
action_55 (21) = happyShift action_19
action_55 (28) = happyShift action_20
action_55 (40) = happyShift action_21
action_55 (41) = happyShift action_22
action_55 (45) = happyShift action_23
action_55 (46) = happyShift action_24
action_55 (47) = happyShift action_25
action_55 (49) = happyShift action_26
action_55 (52) = happyShift action_27
action_55 (53) = happyShift action_28
action_55 (54) = happyShift action_29
action_55 (55) = happyShift action_30
action_55 (56) = happyShift action_31
action_55 (57) = happyShift action_32
action_55 (58) = happyShift action_33
action_55 (6) = happyGoto action_78
action_55 (7) = happyGoto action_10
action_55 (9) = happyGoto action_11
action_55 (10) = happyGoto action_12
action_55 (11) = happyGoto action_13
action_55 (13) = happyGoto action_14
action_55 (14) = happyGoto action_15
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (15) = happyShift action_16
action_56 (18) = happyShift action_17
action_56 (20) = happyShift action_18
action_56 (21) = happyShift action_19
action_56 (28) = happyShift action_20
action_56 (40) = happyShift action_21
action_56 (41) = happyShift action_22
action_56 (45) = happyShift action_23
action_56 (46) = happyShift action_24
action_56 (47) = happyShift action_25
action_56 (49) = happyShift action_26
action_56 (52) = happyShift action_27
action_56 (53) = happyShift action_28
action_56 (54) = happyShift action_29
action_56 (55) = happyShift action_30
action_56 (56) = happyShift action_31
action_56 (57) = happyShift action_32
action_56 (58) = happyShift action_33
action_56 (6) = happyGoto action_77
action_56 (7) = happyGoto action_10
action_56 (9) = happyGoto action_11
action_56 (10) = happyGoto action_12
action_56 (11) = happyGoto action_13
action_56 (13) = happyGoto action_14
action_56 (14) = happyGoto action_15
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (15) = happyShift action_16
action_57 (18) = happyShift action_17
action_57 (20) = happyShift action_18
action_57 (21) = happyShift action_19
action_57 (28) = happyShift action_20
action_57 (40) = happyShift action_21
action_57 (41) = happyShift action_22
action_57 (45) = happyShift action_23
action_57 (46) = happyShift action_24
action_57 (47) = happyShift action_25
action_57 (49) = happyShift action_26
action_57 (52) = happyShift action_27
action_57 (53) = happyShift action_28
action_57 (54) = happyShift action_29
action_57 (55) = happyShift action_30
action_57 (56) = happyShift action_31
action_57 (57) = happyShift action_32
action_57 (58) = happyShift action_33
action_57 (6) = happyGoto action_76
action_57 (7) = happyGoto action_10
action_57 (9) = happyGoto action_11
action_57 (10) = happyGoto action_12
action_57 (11) = happyGoto action_13
action_57 (13) = happyGoto action_14
action_57 (14) = happyGoto action_15
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (15) = happyShift action_16
action_58 (18) = happyShift action_17
action_58 (20) = happyShift action_18
action_58 (21) = happyShift action_19
action_58 (28) = happyShift action_20
action_58 (40) = happyShift action_21
action_58 (41) = happyShift action_22
action_58 (45) = happyShift action_23
action_58 (46) = happyShift action_24
action_58 (47) = happyShift action_25
action_58 (49) = happyShift action_26
action_58 (52) = happyShift action_27
action_58 (53) = happyShift action_28
action_58 (54) = happyShift action_29
action_58 (55) = happyShift action_30
action_58 (56) = happyShift action_31
action_58 (57) = happyShift action_32
action_58 (58) = happyShift action_33
action_58 (6) = happyGoto action_75
action_58 (7) = happyGoto action_10
action_58 (9) = happyGoto action_11
action_58 (10) = happyGoto action_12
action_58 (11) = happyGoto action_13
action_58 (13) = happyGoto action_14
action_58 (14) = happyGoto action_15
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (15) = happyShift action_16
action_59 (18) = happyShift action_17
action_59 (20) = happyShift action_18
action_59 (21) = happyShift action_19
action_59 (28) = happyShift action_20
action_59 (40) = happyShift action_21
action_59 (41) = happyShift action_22
action_59 (45) = happyShift action_23
action_59 (46) = happyShift action_24
action_59 (47) = happyShift action_25
action_59 (49) = happyShift action_26
action_59 (52) = happyShift action_27
action_59 (53) = happyShift action_28
action_59 (54) = happyShift action_29
action_59 (55) = happyShift action_30
action_59 (56) = happyShift action_31
action_59 (57) = happyShift action_32
action_59 (58) = happyShift action_33
action_59 (6) = happyGoto action_74
action_59 (7) = happyGoto action_10
action_59 (9) = happyGoto action_11
action_59 (10) = happyGoto action_12
action_59 (11) = happyGoto action_13
action_59 (13) = happyGoto action_14
action_59 (14) = happyGoto action_15
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (15) = happyShift action_16
action_60 (18) = happyShift action_17
action_60 (20) = happyShift action_18
action_60 (21) = happyShift action_19
action_60 (28) = happyShift action_20
action_60 (40) = happyShift action_21
action_60 (41) = happyShift action_22
action_60 (45) = happyShift action_23
action_60 (46) = happyShift action_24
action_60 (47) = happyShift action_25
action_60 (49) = happyShift action_26
action_60 (52) = happyShift action_27
action_60 (53) = happyShift action_28
action_60 (54) = happyShift action_29
action_60 (55) = happyShift action_30
action_60 (56) = happyShift action_31
action_60 (57) = happyShift action_32
action_60 (58) = happyShift action_33
action_60 (6) = happyGoto action_73
action_60 (7) = happyGoto action_10
action_60 (9) = happyGoto action_11
action_60 (10) = happyGoto action_12
action_60 (11) = happyGoto action_13
action_60 (13) = happyGoto action_14
action_60 (14) = happyGoto action_15
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (15) = happyShift action_16
action_61 (18) = happyShift action_17
action_61 (20) = happyShift action_18
action_61 (21) = happyShift action_19
action_61 (28) = happyShift action_20
action_61 (40) = happyShift action_21
action_61 (41) = happyShift action_22
action_61 (45) = happyShift action_23
action_61 (46) = happyShift action_24
action_61 (47) = happyShift action_25
action_61 (49) = happyShift action_26
action_61 (52) = happyShift action_27
action_61 (53) = happyShift action_28
action_61 (54) = happyShift action_29
action_61 (55) = happyShift action_30
action_61 (56) = happyShift action_31
action_61 (57) = happyShift action_32
action_61 (58) = happyShift action_33
action_61 (6) = happyGoto action_72
action_61 (7) = happyGoto action_10
action_61 (9) = happyGoto action_11
action_61 (10) = happyGoto action_12
action_61 (11) = happyGoto action_13
action_61 (13) = happyGoto action_14
action_61 (14) = happyGoto action_15
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (15) = happyShift action_16
action_62 (18) = happyShift action_17
action_62 (20) = happyShift action_18
action_62 (21) = happyShift action_19
action_62 (28) = happyShift action_20
action_62 (40) = happyShift action_21
action_62 (41) = happyShift action_22
action_62 (45) = happyShift action_23
action_62 (46) = happyShift action_24
action_62 (47) = happyShift action_25
action_62 (49) = happyShift action_26
action_62 (52) = happyShift action_27
action_62 (53) = happyShift action_28
action_62 (54) = happyShift action_29
action_62 (55) = happyShift action_30
action_62 (56) = happyShift action_31
action_62 (57) = happyShift action_32
action_62 (58) = happyShift action_33
action_62 (6) = happyGoto action_71
action_62 (7) = happyGoto action_10
action_62 (9) = happyGoto action_11
action_62 (10) = happyGoto action_12
action_62 (11) = happyGoto action_13
action_62 (13) = happyGoto action_14
action_62 (14) = happyGoto action_15
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (15) = happyShift action_16
action_63 (18) = happyShift action_17
action_63 (20) = happyShift action_18
action_63 (21) = happyShift action_19
action_63 (28) = happyShift action_20
action_63 (40) = happyShift action_21
action_63 (41) = happyShift action_22
action_63 (45) = happyShift action_23
action_63 (46) = happyShift action_24
action_63 (47) = happyShift action_25
action_63 (49) = happyShift action_26
action_63 (52) = happyShift action_27
action_63 (53) = happyShift action_28
action_63 (54) = happyShift action_29
action_63 (55) = happyShift action_30
action_63 (56) = happyShift action_31
action_63 (57) = happyShift action_32
action_63 (58) = happyShift action_33
action_63 (6) = happyGoto action_70
action_63 (7) = happyGoto action_10
action_63 (9) = happyGoto action_11
action_63 (10) = happyGoto action_12
action_63 (11) = happyGoto action_13
action_63 (13) = happyGoto action_14
action_63 (14) = happyGoto action_15
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (15) = happyShift action_16
action_64 (18) = happyShift action_17
action_64 (20) = happyShift action_18
action_64 (21) = happyShift action_19
action_64 (28) = happyShift action_20
action_64 (40) = happyShift action_21
action_64 (41) = happyShift action_22
action_64 (45) = happyShift action_23
action_64 (46) = happyShift action_24
action_64 (47) = happyShift action_25
action_64 (49) = happyShift action_26
action_64 (52) = happyShift action_27
action_64 (53) = happyShift action_28
action_64 (54) = happyShift action_29
action_64 (55) = happyShift action_30
action_64 (56) = happyShift action_31
action_64 (57) = happyShift action_32
action_64 (58) = happyShift action_33
action_64 (6) = happyGoto action_69
action_64 (7) = happyGoto action_10
action_64 (9) = happyGoto action_11
action_64 (10) = happyGoto action_12
action_64 (11) = happyGoto action_13
action_64 (13) = happyGoto action_14
action_64 (14) = happyGoto action_15
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (15) = happyShift action_16
action_65 (18) = happyShift action_17
action_65 (20) = happyShift action_18
action_65 (21) = happyShift action_19
action_65 (28) = happyShift action_20
action_65 (40) = happyShift action_21
action_65 (41) = happyShift action_22
action_65 (45) = happyShift action_23
action_65 (46) = happyShift action_24
action_65 (47) = happyShift action_25
action_65 (49) = happyShift action_26
action_65 (52) = happyShift action_27
action_65 (53) = happyShift action_28
action_65 (54) = happyShift action_29
action_65 (55) = happyShift action_30
action_65 (56) = happyShift action_31
action_65 (57) = happyShift action_32
action_65 (58) = happyShift action_33
action_65 (6) = happyGoto action_38
action_65 (7) = happyGoto action_10
action_65 (9) = happyGoto action_68
action_65 (10) = happyGoto action_12
action_65 (11) = happyGoto action_13
action_65 (13) = happyGoto action_14
action_65 (14) = happyGoto action_15
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_6

action_67 _ = happyReduce_7

action_68 (27) = happyReduce_38
action_68 (28) = happyReduce_38
action_68 (29) = happyReduce_38
action_68 (30) = happyReduce_38
action_68 (33) = happyReduce_38
action_68 (34) = happyReduce_38
action_68 (35) = happyReduce_38
action_68 (36) = happyReduce_38
action_68 (37) = happyReduce_38
action_68 (38) = happyReduce_38
action_68 (39) = happyReduce_38
action_68 (42) = happyReduce_38
action_68 (43) = happyReduce_38
action_68 (51) = happyReduce_38
action_68 _ = happyReduce_38

action_69 (27) = happyShift action_52
action_69 (28) = happyShift action_53
action_69 (29) = happyShift action_54
action_69 (30) = happyShift action_55
action_69 (33) = happyShift action_56
action_69 (34) = happyShift action_57
action_69 (35) = happyShift action_58
action_69 (36) = happyShift action_59
action_69 (37) = happyShift action_60
action_69 (38) = happyShift action_61
action_69 (39) = happyShift action_62
action_69 (51) = happyShift action_65
action_69 _ = happyReduce_24

action_70 (27) = happyShift action_52
action_70 (28) = happyShift action_53
action_70 (29) = happyShift action_54
action_70 (30) = happyShift action_55
action_70 (33) = happyShift action_56
action_70 (34) = happyShift action_57
action_70 (35) = happyShift action_58
action_70 (36) = happyShift action_59
action_70 (37) = happyShift action_60
action_70 (38) = happyShift action_61
action_70 (39) = happyShift action_62
action_70 (51) = happyShift action_65
action_70 _ = happyReduce_23

action_71 (34) = happyShift action_57
action_71 (35) = happyShift action_58
action_71 (36) = happyShift action_59
action_71 (37) = happyShift action_60
action_71 (51) = happyShift action_65
action_71 _ = happyReduce_32

action_72 (34) = happyShift action_57
action_72 (35) = happyShift action_58
action_72 (36) = happyShift action_59
action_72 (37) = happyShift action_60
action_72 (51) = happyShift action_65
action_72 _ = happyReduce_31

action_73 (27) = happyShift action_52
action_73 (28) = happyShift action_53
action_73 (29) = happyShift action_54
action_73 (30) = happyShift action_55
action_73 (33) = happyShift action_56
action_73 (34) = happyShift action_57
action_73 (35) = happyShift action_58
action_73 (36) = happyShift action_59
action_73 (37) = happyShift action_60
action_73 (38) = happyShift action_61
action_73 (39) = happyShift action_62
action_73 (42) = happyShift action_63
action_73 (43) = happyShift action_64
action_73 (51) = happyShift action_65
action_73 _ = happyReduce_26

action_74 (27) = happyShift action_52
action_74 (28) = happyShift action_53
action_74 (29) = happyShift action_54
action_74 (30) = happyShift action_55
action_74 (33) = happyShift action_56
action_74 (34) = happyShift action_57
action_74 (35) = happyShift action_58
action_74 (36) = happyShift action_59
action_74 (37) = happyShift action_60
action_74 (38) = happyShift action_61
action_74 (39) = happyShift action_62
action_74 (42) = happyShift action_63
action_74 (43) = happyShift action_64
action_74 (51) = happyShift action_65
action_74 _ = happyReduce_25

action_75 (27) = happyShift action_52
action_75 (28) = happyShift action_53
action_75 (29) = happyShift action_54
action_75 (30) = happyShift action_55
action_75 (33) = happyShift action_56
action_75 (34) = happyShift action_57
action_75 (35) = happyShift action_58
action_75 (36) = happyShift action_59
action_75 (37) = happyShift action_60
action_75 (38) = happyShift action_61
action_75 (39) = happyShift action_62
action_75 (42) = happyShift action_63
action_75 (43) = happyShift action_64
action_75 (51) = happyShift action_65
action_75 _ = happyReduce_28

action_76 (27) = happyShift action_52
action_76 (28) = happyShift action_53
action_76 (29) = happyShift action_54
action_76 (30) = happyShift action_55
action_76 (33) = happyShift action_56
action_76 (34) = happyShift action_57
action_76 (35) = happyShift action_58
action_76 (36) = happyShift action_59
action_76 (37) = happyShift action_60
action_76 (38) = happyShift action_61
action_76 (39) = happyShift action_62
action_76 (42) = happyShift action_63
action_76 (43) = happyShift action_64
action_76 (51) = happyShift action_65
action_76 _ = happyReduce_27

action_77 (34) = happyShift action_57
action_77 (35) = happyShift action_58
action_77 (36) = happyShift action_59
action_77 (37) = happyShift action_60
action_77 (38) = happyShift action_61
action_77 (39) = happyShift action_62
action_77 (51) = happyShift action_65
action_77 _ = happyReduce_21

action_78 (33) = happyShift action_56
action_78 (34) = happyShift action_57
action_78 (35) = happyShift action_58
action_78 (36) = happyShift action_59
action_78 (37) = happyShift action_60
action_78 (38) = happyShift action_61
action_78 (39) = happyShift action_62
action_78 (51) = happyShift action_65
action_78 _ = happyReduce_20

action_79 (33) = happyShift action_56
action_79 (34) = happyShift action_57
action_79 (35) = happyShift action_58
action_79 (36) = happyShift action_59
action_79 (37) = happyShift action_60
action_79 (38) = happyShift action_61
action_79 (39) = happyShift action_62
action_79 (51) = happyShift action_65
action_79 _ = happyReduce_19

action_80 (29) = happyShift action_54
action_80 (30) = happyShift action_55
action_80 (33) = happyShift action_56
action_80 (34) = happyShift action_57
action_80 (35) = happyShift action_58
action_80 (36) = happyShift action_59
action_80 (37) = happyShift action_60
action_80 (38) = happyShift action_61
action_80 (39) = happyShift action_62
action_80 (51) = happyShift action_65
action_80 _ = happyReduce_18

action_81 (29) = happyShift action_54
action_81 (30) = happyShift action_55
action_81 (33) = happyShift action_56
action_81 (34) = happyShift action_57
action_81 (35) = happyShift action_58
action_81 (36) = happyShift action_59
action_81 (37) = happyShift action_60
action_81 (38) = happyShift action_61
action_81 (39) = happyShift action_62
action_81 (51) = happyShift action_65
action_81 _ = happyReduce_17

action_82 (27) = happyShift action_52
action_82 (28) = happyShift action_53
action_82 (29) = happyShift action_54
action_82 (30) = happyShift action_55
action_82 (33) = happyShift action_56
action_82 (34) = happyShift action_57
action_82 (35) = happyShift action_58
action_82 (36) = happyShift action_59
action_82 (37) = happyShift action_60
action_82 (38) = happyShift action_61
action_82 (39) = happyShift action_62
action_82 (42) = happyShift action_63
action_82 (43) = happyShift action_64
action_82 (51) = happyShift action_65
action_82 _ = happyReduce_54

action_83 (15) = happyShift action_16
action_83 (18) = happyShift action_17
action_83 (20) = happyShift action_18
action_83 (21) = happyShift action_19
action_83 (28) = happyShift action_20
action_83 (40) = happyShift action_21
action_83 (41) = happyShift action_22
action_83 (45) = happyShift action_23
action_83 (46) = happyShift action_24
action_83 (47) = happyShift action_25
action_83 (49) = happyShift action_26
action_83 (52) = happyShift action_27
action_83 (53) = happyShift action_28
action_83 (54) = happyShift action_29
action_83 (55) = happyShift action_30
action_83 (56) = happyShift action_31
action_83 (57) = happyShift action_32
action_83 (58) = happyShift action_33
action_83 (6) = happyGoto action_96
action_83 (7) = happyGoto action_10
action_83 (9) = happyGoto action_11
action_83 (10) = happyGoto action_12
action_83 (11) = happyGoto action_13
action_83 (13) = happyGoto action_14
action_83 (14) = happyGoto action_15
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (15) = happyShift action_16
action_84 (18) = happyShift action_17
action_84 (20) = happyShift action_18
action_84 (21) = happyShift action_19
action_84 (28) = happyShift action_20
action_84 (40) = happyShift action_21
action_84 (41) = happyShift action_22
action_84 (45) = happyShift action_23
action_84 (46) = happyShift action_24
action_84 (47) = happyShift action_25
action_84 (49) = happyShift action_26
action_84 (52) = happyShift action_27
action_84 (53) = happyShift action_28
action_84 (54) = happyShift action_29
action_84 (55) = happyShift action_30
action_84 (56) = happyShift action_31
action_84 (57) = happyShift action_32
action_84 (58) = happyShift action_33
action_84 (6) = happyGoto action_95
action_84 (7) = happyGoto action_10
action_84 (9) = happyGoto action_11
action_84 (10) = happyGoto action_12
action_84 (11) = happyGoto action_13
action_84 (13) = happyGoto action_14
action_84 (14) = happyGoto action_15
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (15) = happyShift action_16
action_85 (18) = happyShift action_17
action_85 (20) = happyShift action_18
action_85 (21) = happyShift action_19
action_85 (28) = happyShift action_20
action_85 (40) = happyShift action_21
action_85 (41) = happyShift action_22
action_85 (45) = happyShift action_23
action_85 (46) = happyShift action_24
action_85 (47) = happyShift action_25
action_85 (49) = happyShift action_26
action_85 (52) = happyShift action_27
action_85 (53) = happyShift action_28
action_85 (54) = happyShift action_29
action_85 (55) = happyShift action_30
action_85 (56) = happyShift action_31
action_85 (57) = happyShift action_32
action_85 (58) = happyShift action_33
action_85 (6) = happyGoto action_94
action_85 (7) = happyGoto action_10
action_85 (9) = happyGoto action_11
action_85 (10) = happyGoto action_12
action_85 (11) = happyGoto action_13
action_85 (13) = happyGoto action_14
action_85 (14) = happyGoto action_15
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_42

action_87 _ = happyReduce_12

action_88 (15) = happyShift action_16
action_88 (18) = happyShift action_17
action_88 (20) = happyShift action_18
action_88 (21) = happyShift action_19
action_88 (28) = happyShift action_20
action_88 (40) = happyShift action_21
action_88 (41) = happyShift action_22
action_88 (45) = happyShift action_23
action_88 (46) = happyShift action_24
action_88 (47) = happyShift action_25
action_88 (49) = happyShift action_26
action_88 (52) = happyShift action_27
action_88 (53) = happyShift action_28
action_88 (54) = happyShift action_29
action_88 (55) = happyShift action_30
action_88 (56) = happyShift action_31
action_88 (57) = happyShift action_32
action_88 (58) = happyShift action_33
action_88 (6) = happyGoto action_35
action_88 (7) = happyGoto action_10
action_88 (9) = happyGoto action_11
action_88 (10) = happyGoto action_12
action_88 (11) = happyGoto action_13
action_88 (12) = happyGoto action_93
action_88 (13) = happyGoto action_14
action_88 (14) = happyGoto action_15
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_40

action_90 (44) = happyShift action_92
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_46

action_92 (32) = happyShift action_98
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_44

action_94 (27) = happyShift action_52
action_94 (28) = happyShift action_53
action_94 (29) = happyShift action_54
action_94 (30) = happyShift action_55
action_94 (33) = happyShift action_56
action_94 (34) = happyShift action_57
action_94 (35) = happyShift action_58
action_94 (36) = happyShift action_59
action_94 (37) = happyShift action_60
action_94 (38) = happyShift action_61
action_94 (39) = happyShift action_62
action_94 (42) = happyShift action_63
action_94 (43) = happyShift action_64
action_94 (51) = happyShift action_65
action_94 _ = happyReduce_14

action_95 (16) = happyShift action_97
action_95 (17) = happyReduce_36
action_95 (27) = happyShift action_52
action_95 (28) = happyShift action_53
action_95 (29) = happyShift action_54
action_95 (30) = happyShift action_55
action_95 (33) = happyShift action_56
action_95 (34) = happyShift action_57
action_95 (35) = happyShift action_58
action_95 (36) = happyShift action_59
action_95 (37) = happyShift action_60
action_95 (38) = happyShift action_61
action_95 (39) = happyShift action_62
action_95 (42) = happyShift action_63
action_95 (43) = happyShift action_64
action_95 (51) = happyShift action_65
action_95 _ = happyReduce_33

action_96 (27) = happyShift action_52
action_96 (28) = happyShift action_53
action_96 (29) = happyShift action_54
action_96 (30) = happyShift action_55
action_96 (33) = happyShift action_56
action_96 (34) = happyShift action_57
action_96 (35) = happyShift action_58
action_96 (36) = happyShift action_59
action_96 (37) = happyShift action_60
action_96 (38) = happyShift action_61
action_96 (39) = happyShift action_62
action_96 (42) = happyShift action_63
action_96 (43) = happyShift action_64
action_96 (51) = happyShift action_65
action_96 _ = happyReduce_34

action_97 (15) = happyShift action_16
action_97 (18) = happyShift action_17
action_97 (20) = happyShift action_18
action_97 (21) = happyShift action_19
action_97 (28) = happyShift action_20
action_97 (40) = happyShift action_21
action_97 (41) = happyShift action_22
action_97 (45) = happyShift action_23
action_97 (46) = happyShift action_24
action_97 (47) = happyShift action_25
action_97 (49) = happyShift action_26
action_97 (52) = happyShift action_27
action_97 (53) = happyShift action_28
action_97 (54) = happyShift action_29
action_97 (55) = happyShift action_30
action_97 (56) = happyShift action_31
action_97 (57) = happyShift action_32
action_97 (58) = happyShift action_33
action_97 (6) = happyGoto action_100
action_97 (7) = happyGoto action_10
action_97 (8) = happyGoto action_101
action_97 (9) = happyGoto action_11
action_97 (10) = happyGoto action_12
action_97 (11) = happyGoto action_13
action_97 (13) = happyGoto action_14
action_97 (14) = happyGoto action_15
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (15) = happyShift action_16
action_98 (18) = happyShift action_17
action_98 (20) = happyShift action_18
action_98 (21) = happyShift action_19
action_98 (28) = happyShift action_20
action_98 (40) = happyShift action_21
action_98 (41) = happyShift action_22
action_98 (45) = happyShift action_23
action_98 (46) = happyShift action_24
action_98 (47) = happyShift action_25
action_98 (49) = happyShift action_26
action_98 (52) = happyShift action_27
action_98 (53) = happyShift action_28
action_98 (54) = happyShift action_29
action_98 (55) = happyShift action_30
action_98 (56) = happyShift action_31
action_98 (57) = happyShift action_32
action_98 (58) = happyShift action_33
action_98 (6) = happyGoto action_99
action_98 (7) = happyGoto action_10
action_98 (9) = happyGoto action_11
action_98 (10) = happyGoto action_12
action_98 (11) = happyGoto action_13
action_98 (13) = happyGoto action_14
action_98 (14) = happyGoto action_15
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (27) = happyShift action_52
action_99 (28) = happyShift action_53
action_99 (29) = happyShift action_54
action_99 (30) = happyShift action_55
action_99 (33) = happyShift action_56
action_99 (34) = happyShift action_57
action_99 (35) = happyShift action_58
action_99 (36) = happyShift action_59
action_99 (37) = happyShift action_60
action_99 (38) = happyShift action_61
action_99 (39) = happyShift action_62
action_99 (42) = happyShift action_63
action_99 (43) = happyShift action_64
action_99 (51) = happyShift action_65
action_99 _ = happyReduce_13

action_100 (19) = happyShift action_102
action_100 (27) = happyShift action_52
action_100 (28) = happyShift action_53
action_100 (29) = happyShift action_54
action_100 (30) = happyShift action_55
action_100 (33) = happyShift action_56
action_100 (34) = happyShift action_57
action_100 (35) = happyShift action_58
action_100 (36) = happyShift action_59
action_100 (37) = happyShift action_60
action_100 (38) = happyShift action_61
action_100 (39) = happyShift action_62
action_100 (42) = happyShift action_63
action_100 (43) = happyShift action_64
action_100 (51) = happyShift action_65
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_35

action_102 (15) = happyShift action_16
action_102 (18) = happyShift action_17
action_102 (20) = happyShift action_18
action_102 (21) = happyShift action_19
action_102 (28) = happyShift action_20
action_102 (40) = happyShift action_21
action_102 (41) = happyShift action_22
action_102 (45) = happyShift action_23
action_102 (46) = happyShift action_24
action_102 (47) = happyShift action_25
action_102 (49) = happyShift action_26
action_102 (52) = happyShift action_27
action_102 (53) = happyShift action_28
action_102 (54) = happyShift action_29
action_102 (55) = happyShift action_30
action_102 (56) = happyShift action_31
action_102 (57) = happyShift action_32
action_102 (58) = happyShift action_33
action_102 (6) = happyGoto action_103
action_102 (7) = happyGoto action_10
action_102 (9) = happyGoto action_11
action_102 (10) = happyGoto action_12
action_102 (11) = happyGoto action_13
action_102 (13) = happyGoto action_14
action_102 (14) = happyGoto action_15
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (16) = happyShift action_97
action_103 (27) = happyShift action_52
action_103 (28) = happyShift action_53
action_103 (29) = happyShift action_54
action_103 (30) = happyShift action_55
action_103 (33) = happyShift action_56
action_103 (34) = happyShift action_57
action_103 (35) = happyShift action_58
action_103 (36) = happyShift action_59
action_103 (37) = happyShift action_60
action_103 (38) = happyShift action_61
action_103 (39) = happyShift action_62
action_103 (42) = happyShift action_63
action_103 (43) = happyShift action_64
action_103 (51) = happyShift action_65
action_103 _ = happyReduce_36

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Drct happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (Stmt happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn5
		 (Exit
	)

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn5
		 (Help
	)

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn5
		 (Clear
	)

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyTerminal (TStr happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Load happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyTerminal (TSym happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Rm happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 6 6 happyReduction_13
happyReduction_13 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Assign happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 6 happyReduction_14
happyReduction_14 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  6 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn6
		 (Read
	)

happyReduce_16 = happySpecReduce_2  6 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Print happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary ((+) :: Double -> Double -> Double) happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary ((-) :: Double -> Double -> Double) happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary ((*) :: Double -> Double -> Double) happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary ((/) :: Double -> Double -> Double) happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  6 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary ((**) :: Double -> Double -> Double) happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  6 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Unary opposite happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary (&&) happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  6 happyReduction_24
happyReduction_24 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary (||) happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  6 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary (==) happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  6 happyReduction_26
happyReduction_26 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary (/=) happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  6 happyReduction_27
happyReduction_27 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary (<) happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  6 happyReduction_28
happyReduction_28 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary (>) happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  6 happyReduction_29
happyReduction_29 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Unary (-<) happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  6 happyReduction_30
happyReduction_30 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Unary (>-) happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  6 happyReduction_31
happyReduction_31 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary ((<>) :: Seq Val -> Seq Val -> Seq Val) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  6 happyReduction_32
happyReduction_32 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Binary ((<>) :: String -> String -> String) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 7 happyReduction_33
happyReduction_33 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (If [(happy_var_2,happy_var_4)] (Val Null)
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 4 7 happyReduction_34
happyReduction_34 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (If happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 5 8 happyReduction_35
happyReduction_35 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_1,happy_var_3):happy_var_5
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  8 happyReduction_36
happyReduction_36 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn8
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  9 happyReduction_37
happyReduction_37 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Seq happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  9 happyReduction_38
happyReduction_38 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1:happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  9 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn9
		 ([]
	)

happyReduce_40 = happySpecReduce_3  10 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (ListLiteral happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  10 happyReduction_41
happyReduction_41 _
	_
	 =  HappyAbsSyn10
		 (ListLiteral []
	)

happyReduce_42 = happySpecReduce_3  11 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  11 happyReduction_43
happyReduction_43 _
	_
	 =  HappyAbsSyn11
		 ([]
	)

happyReduce_44 = happySpecReduce_3  12 happyReduction_44
happyReduction_44 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1:happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  12 happyReduction_45
happyReduction_45 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  13 happyReduction_46
happyReduction_46 (HappyTerminal (TSym happy_var_1))
	 =  HappyAbsSyn13
		 (Var happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  13 happyReduction_47
happyReduction_47 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (Val happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  14 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn14
		 (BoolVal True
	)

happyReduce_49 = happySpecReduce_1  14 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn14
		 (BoolVal False
	)

happyReduce_50 = happySpecReduce_1  14 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn14
		 (Null
	)

happyReduce_51 = happySpecReduce_1  14 happyReduction_51
happyReduction_51 (HappyTerminal (TNum happy_var_1))
	 =  HappyAbsSyn14
		 (AlgVal happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  14 happyReduction_52
happyReduction_52 (HappyTerminal (TChar happy_var_1))
	 =  HappyAbsSyn14
		 (CharVal happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  14 happyReduction_53
happyReduction_53 (HappyTerminal (TStr happy_var_1))
	 =  HappyAbsSyn14
		 (StrVal happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  14 happyReduction_54
happyReduction_54 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn14
		 (FunVal happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= L.lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 59 59 tk (HappyState action) sts stk;
	TIf -> cont 15;
	TElIf -> cont 16;
	TElse -> cont 17;
	TWhile -> cont 18;
	TDo -> cont 19;
	TPrint -> cont 20;
	TRead -> cont 21;
	TExit -> cont 22;
	THelp -> cont 23;
	TRM -> cont 24;
	TClear -> cont 25;
	TLoad -> cont 26;
	TPlus -> cont 27;
	TMinus -> cont 28;
	TStar -> cont 29;
	TSlash -> cont 30;
	TFatArr -> cont 31;
	TAssign -> cont 32;
	TDash -> cont 33;
	TLt -> cont 34;
	TGt -> cont 35;
	TEq -> cont 36;
	TNeq -> cont 37;
	TLtGt -> cont 38;
	TGtLt -> cont 39;
	TRFork -> cont 40;
	TLFork -> cont 41;
	TAnd -> cont 42;
	TOr -> cont 43;
	TRParen -> cont 44;
	TRParen -> cont 45;
	TRBrace -> cont 46;
	TLBrace -> cont 47;
	TRBracket -> cont 48;
	TLBracket -> cont 49;
	TComma -> cont 50;
	TSemi -> cont 51;
	TTrue -> cont 52;
	TFalse -> cont 53;
	TNull -> cont 54;
	TNum happy_dollar_dollar -> cont 55;
	TChar happy_dollar_dollar -> cont 56;
	TStr happy_dollar_dollar -> cont 57;
	TSym happy_dollar_dollar -> cont 58;
	_ -> happyError' (tk, [])
	})

happyError_ explist 59 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Except String a -> (a -> Except String b) -> Except String b
happyThen = ((>>=))
happyReturn :: () => a -> Except String a
happyReturn = (return)
happyThen1 :: () => Except String a -> (a -> Except String b) -> Except String b
happyThen1 = happyThen
happyReturn1 :: () => a -> Except String a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> Except String a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
pig = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError _ = throwError '!Parse Error'
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
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
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

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
