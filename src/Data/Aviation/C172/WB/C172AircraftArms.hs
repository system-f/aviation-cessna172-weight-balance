{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.C172.WB.C172AircraftArms(
  C172AircraftArms(..)
, HasC172AircraftArms(..)
, bewC172AircraftArms
, zfwMoment
, ffwMoment
, ufwMoment
, totalC172Moment
) where

import Control.Applicative(Applicative((<*>), pure))
import Control.Category((.))
import Control.Lens(makeClassy, lens, (^.), (.~), (&))
import Data.Aviation.C172.WB.C172Arms(C172Arms, HasC172Arms(c172Arms), fuel, c172ArmsPOH)
import Data.Aviation.Units(Pounds(pounds))
import Data.Aviation.WB(ArmStatic)
import Data.Aviation.WB.Arm(Arm, HasArmStatic, staticArm)
import Data.Aviation.WB.Weight(HasWeight(weight))
import Data.Aviation.WB.Moment(Moment, momentX)
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldr))
import Data.Functor(Functor(fmap), (<$>))
import Data.Ord(Ord)
import Data.Traversable(Traversable(traverse))
import Prelude(Show)

data C172AircraftArms a =
  C172AircraftArms {
    _basicEmptyWeight ::
      a
  , c172Arms_ ::
      C172Arms a
  }
  deriving (Eq, Ord, Show)

makeClassy ''C172AircraftArms

instance HasC172Arms (C172AircraftArms a) a where
  c172Arms =
    lens
      (\(C172AircraftArms _ c) -> c)
      (\(C172AircraftArms c _) a -> C172AircraftArms c a)

instance Functor C172AircraftArms where
  fmap k (C172AircraftArms c x) =
    C172AircraftArms (k c) (fmap k x)

instance Applicative C172AircraftArms where
  pure a =
    C172AircraftArms a (pure a)
  C172AircraftArms c1 c2 <*> C172AircraftArms x1 x2 =
    C172AircraftArms (c1 x1) (c2 <*> x2)

instance Foldable C172AircraftArms where
  foldr k z (C172AircraftArms c x) =
    k c (foldr k z x)

instance Traversable C172AircraftArms where
  traverse k (C172AircraftArms c x) =
    C172AircraftArms <$> k c <*> traverse k x

bewC172AircraftArms ::
  ArmStatic
  -> C172AircraftArms Arm
bewC172AircraftArms a =
  C172AircraftArms
    (staticArm a)
    c172ArmsPOH

-- zero fuel
zfwMoment ::
  C172AircraftArms Moment
  -> C172AircraftArms Moment
zfwMoment x =
  x & fuel . weight .~ 0 ^. pounds

-- full fuel
ffwMoment ::
  C172AircraftArms Moment
  -> C172AircraftArms Moment
ffwMoment x =
  x & fuel . weight .~ 336 ^. pounds

-- unusable fuel
ufwMoment ::
  C172AircraftArms Moment
  -> C172AircraftArms Moment
ufwMoment x =
  x & fuel . weight .~ 18 ^. pounds


totalC172Moment ::
  (HasWeight w, HasArmStatic s) =>
  w
  -> C172Arms w
  -> C172AircraftArms s
  -> C172AircraftArms Moment
totalC172Moment bew wt am =
  momentX (C172AircraftArms bew wt) am
