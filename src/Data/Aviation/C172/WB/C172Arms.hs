{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.C172.WB.C172Arms(
  C172Arms(..)
, HasC172Arms(..)
, c172ArmsPOH
) where

import Control.Applicative(Applicative((<*>), pure))
import Control.Lens(makeClassy, (^.))
import Data.Aviation.Units(inches)
import Data.Aviation.WB.Arm(Arm, (.->.), rangeArm, staticArm)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldr))
import Data.Functor(Functor(fmap), (<$>))
import Data.Ord(Ord)
import Data.Traversable(Traversable(traverse))
import Prelude(Show)

data C172Arms a =
  C172Arms {
    _frontseat ::
      a
  , _rearseat ::
      a
  , _fuel ::
      a
  , _baggagea ::
      a
  , _baggageb ::
      a
  }
  deriving (Eq, Ord, Show)

makeClassy ''C172Arms

instance Functor C172Arms where
  fmap k (C172Arms t r f a b) =
    C172Arms (k t) (k r) (k f) (k a) (k b)

instance Applicative C172Arms where
  pure a =
    C172Arms a a a a a
  C172Arms f1 f2 f3 f4 f5 <*> C172Arms a1 a2 a3 a4 a5 =
    C172Arms (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5)

instance Foldable C172Arms where
  foldr k z (C172Arms t r f a b) =
    foldr k z [t,r,f,a,b]

instance Traversable C172Arms where
  traverse k (C172Arms t r f a b) =
    C172Arms <$> k t <*> k r <*> k f <*> k a <*> k b

c172ArmsPOH ::
  C172Arms Arm
c172ArmsPOH =
  C172Arms
    (rangeArm (37 ^. inches) (34 ^. inches .->. 46 ^. inches))
    (staticArm (73 ^. inches))
    (staticArm (48 ^. inches))
    (rangeArm (95 ^. inches) (82 ^. inches .->. 108 ^. inches))
    (rangeArm (123 ^. inches) (108 ^. inches .->. 142 ^. inches))
