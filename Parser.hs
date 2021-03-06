{-# OPTIONS_GHC -w #-}
module Parser where
import Syntax
import qualified Token as T

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (T.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

action_0 (10) = happyShift action_3
action_0 (24) = happyShift action_4
action_0 (25) = happyShift action_5
action_0 (26) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (5) = happyGoto action_8
action_0 _ = happyFail

action_1 (10) = happyShift action_3
action_1 (24) = happyShift action_4
action_1 (25) = happyShift action_5
action_1 (26) = happyShift action_6
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (11) = happyShift action_19
action_3 _ = happyFail

action_4 (10) = happyShift action_14
action_4 (12) = happyShift action_15
action_4 (19) = happyShift action_16
action_4 (6) = happyGoto action_18
action_4 (7) = happyGoto action_11
action_4 (8) = happyGoto action_12
action_4 (9) = happyGoto action_13
action_4 _ = happyFail

action_5 (10) = happyShift action_14
action_5 (12) = happyShift action_15
action_5 (19) = happyShift action_16
action_5 (6) = happyGoto action_17
action_5 (7) = happyGoto action_11
action_5 (8) = happyGoto action_12
action_5 (9) = happyGoto action_13
action_5 _ = happyFail

action_6 (10) = happyShift action_14
action_6 (12) = happyShift action_15
action_6 (19) = happyShift action_16
action_6 (6) = happyGoto action_10
action_6 (7) = happyGoto action_11
action_6 (8) = happyGoto action_12
action_6 (9) = happyGoto action_13
action_6 _ = happyFail

action_7 (27) = happyAccept
action_7 _ = happyFail

action_8 (10) = happyShift action_3
action_8 (24) = happyShift action_4
action_8 (25) = happyShift action_5
action_8 (26) = happyShift action_6
action_8 (4) = happyGoto action_9
action_8 (5) = happyGoto action_8
action_8 _ = happyReduce_1

action_9 _ = happyReduce_2

action_10 (23) = happyShift action_30
action_10 _ = happyFail

action_11 (17) = happyShift action_28
action_11 (18) = happyShift action_29
action_11 _ = happyReduce_9

action_12 (13) = happyShift action_26
action_12 (14) = happyShift action_27
action_12 _ = happyReduce_12

action_13 (15) = happyShift action_24
action_13 (16) = happyShift action_25
action_13 _ = happyReduce_15

action_14 _ = happyReduce_16

action_15 _ = happyReduce_17

action_16 (10) = happyShift action_14
action_16 (12) = happyShift action_15
action_16 (19) = happyShift action_16
action_16 (6) = happyGoto action_23
action_16 (7) = happyGoto action_11
action_16 (8) = happyGoto action_12
action_16 (9) = happyGoto action_13
action_16 _ = happyFail

action_17 (21) = happyShift action_22
action_17 _ = happyFail

action_18 (21) = happyShift action_21
action_18 _ = happyFail

action_19 (10) = happyShift action_14
action_19 (12) = happyShift action_15
action_19 (19) = happyShift action_16
action_19 (6) = happyGoto action_20
action_19 (7) = happyGoto action_11
action_19 (8) = happyGoto action_12
action_19 (9) = happyGoto action_13
action_19 _ = happyFail

action_20 (23) = happyShift action_40
action_20 _ = happyFail

action_21 (10) = happyShift action_3
action_21 (24) = happyShift action_4
action_21 (25) = happyShift action_5
action_21 (26) = happyShift action_6
action_21 (4) = happyGoto action_39
action_21 (5) = happyGoto action_8
action_21 _ = happyFail

action_22 (10) = happyShift action_3
action_22 (24) = happyShift action_4
action_22 (25) = happyShift action_5
action_22 (26) = happyShift action_6
action_22 (4) = happyGoto action_38
action_22 (5) = happyGoto action_8
action_22 _ = happyFail

action_23 (20) = happyShift action_37
action_23 _ = happyFail

action_24 (10) = happyShift action_14
action_24 (12) = happyShift action_15
action_24 (19) = happyShift action_16
action_24 (8) = happyGoto action_36
action_24 (9) = happyGoto action_13
action_24 _ = happyFail

action_25 (10) = happyShift action_14
action_25 (12) = happyShift action_15
action_25 (19) = happyShift action_16
action_25 (8) = happyGoto action_35
action_25 (9) = happyGoto action_13
action_25 _ = happyFail

action_26 (10) = happyShift action_14
action_26 (12) = happyShift action_15
action_26 (19) = happyShift action_16
action_26 (7) = happyGoto action_34
action_26 (8) = happyGoto action_12
action_26 (9) = happyGoto action_13
action_26 _ = happyFail

action_27 (10) = happyShift action_14
action_27 (12) = happyShift action_15
action_27 (19) = happyShift action_16
action_27 (7) = happyGoto action_33
action_27 (8) = happyGoto action_12
action_27 (9) = happyGoto action_13
action_27 _ = happyFail

action_28 (10) = happyShift action_14
action_28 (12) = happyShift action_15
action_28 (19) = happyShift action_16
action_28 (6) = happyGoto action_32
action_28 (7) = happyGoto action_11
action_28 (8) = happyGoto action_12
action_28 (9) = happyGoto action_13
action_28 _ = happyFail

action_29 (10) = happyShift action_14
action_29 (12) = happyShift action_15
action_29 (19) = happyShift action_16
action_29 (6) = happyGoto action_31
action_29 (7) = happyGoto action_11
action_29 (8) = happyGoto action_12
action_29 (9) = happyGoto action_13
action_29 _ = happyFail

action_30 _ = happyReduce_4

action_31 _ = happyReduce_7

action_32 _ = happyReduce_8

action_33 _ = happyReduce_11

action_34 _ = happyReduce_10

action_35 _ = happyReduce_14

action_36 _ = happyReduce_13

action_37 _ = happyReduce_18

action_38 (22) = happyShift action_42
action_38 _ = happyFail

action_39 (22) = happyShift action_41
action_39 _ = happyFail

action_40 _ = happyReduce_3

action_41 (21) = happyShift action_43
action_41 _ = happyFail

action_42 _ = happyReduce_6

action_43 (10) = happyShift action_3
action_43 (24) = happyShift action_4
action_43 (25) = happyShift action_5
action_43 (26) = happyShift action_6
action_43 (4) = happyGoto action_44
action_43 (5) = happyGoto action_8
action_43 _ = happyFail

action_44 (22) = happyShift action_45
action_44 _ = happyFail

action_45 _ = happyReduce_5

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T.Variable happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Assign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Print happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 8 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ((BinOp EqualTo happy_var_1 happy_var_3)
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ((BinOp LessThan happy_var_1 happy_var_3)
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ((BinOp Add happy_var_1 happy_var_3)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ((BinOp Sub happy_var_1 happy_var_3)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ((BinOp Multi happy_var_1 happy_var_3)
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ((BinOp Div happy_var_1 happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyTerminal (T.Variable happy_var_1))
	 =  HappyAbsSyn9
		 (Var happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  9 happyReduction_17
happyReduction_17 (HappyTerminal (T.Number happy_var_1))
	 =  HappyAbsSyn9
		 (Constant happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 27 27 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T.Variable happy_dollar_dollar -> cont 10;
	T.Assignment -> cont 11;
	T.Number happy_dollar_dollar -> cont 12;
	T.Plus -> cont 13;
	T.Minus -> cont 14;
	T.Multi -> cont 15;
	T.Div -> cont 16;
	T.LessThan -> cont 17;
	T.EqualTo -> cont 18;
	T.LeftParen -> cont 19;
	T.RightParen -> cont 20;
	T.LeftBrace -> cont 21;
	T.RightBrace -> cont 22;
	T.Semi -> cont 23;
	T.If -> cont 24;
	T.While -> cont 25;
	T.Print -> cont 26;
	_ -> happyError' (tk:tks)
	}

happyError_ 27 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(T.Token)] -> HappyIdentity a
happyError' = HappyIdentity . error . show

atticParser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
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
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
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
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
