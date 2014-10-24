{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.GraphPatterns.Types (

    Unique
  , NotUnique

  , In
  , Out
  , Both
  , IfIn
  , IfOut
  , IfBoth
  , FixDirection

  , Multi
  , Single
  , IfMulti
  , IfSingle

  ) where

data Unique
data NotUnique

data In
data Out
data Both

-- | Here we define the type lambda which gives us a partial order on
--   {In, Out, Both} as follows:
--
--     In    Out
--       \  /
--       Both
--
--   It's useful because we will want to say that some EdgeDirection is at
--   least Out (i.e. either Both or Out) and the same for In.
--
--   TBD more suggestive name? I can't really call it a partial order because
--   it actually computes the least upper bound (and is rightly undefined for
--   <In, Out>, <Out, In>.
type family FixDirection a b :: * where
  FixDirection In In = In
  FixDirection Out Out = Out
  FixDirection Both In = In
  FixDirection Both Out = Out

type family IfIn d a b :: * where
  IfIn In a b = a
  IfIn Out a b = b
  IfIn Both a b = b

type family IfOut d a b :: * where
  IfOut Out a b = a
  IfOut In a b = b
  IfOut Both a b = b

type family IfBoth d a b :: * where
  IfBoth Both a b = a
  IfBoth In a b = b
  IfBoth Out a b = b

data Multi
data Single

type family IfMulti d a b :: * where
  IfMulti Multi a b = a
  IfMulti Single a b = b

type family IfSingle d a b :: * where
  IfSingle Single a b = a
  IfSingle Multi a b = b
