{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Optics.Examples where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Optics
import Optics.SOP

_1 :: Lens' (a, b) a
_1 = mkLens $ \ f (a, b) -> (, b) <$> f a

_2 :: Lens' (a, b) b
_2 = mkLens $ \ f (a, b) -> (a, ) <$> f b

data Meetup = Meetup
  { _mDate :: Date
  , _mTalk :: Talk
  }
  deriving (Show, GHC.Generic)

data Talk = Talk
  { _tTitle   :: String
  , _tSpeaker :: String
  }
  deriving (Show, GHC.Generic)

data Date = Date
  { _dYear  :: Int
  , _dMonth :: Int
  , _dDate  :: Int
  }
  deriving (Show, GHC.Generic)

deriving instance Generic Meetup
deriving instance Generic Talk
deriving instance Generic Date

_traverse :: Traversable t => Traversal (t a) (t b) a b
_traverse = mkTraversal traverse

-- Lenses (_1, _2) = mkLenses :: LensesFor (a, b)
Prisms (_Nothing, _Just) = mkPrisms :: PrismsFor (Maybe a)
Prisms (_Left, _Right) = mkPrisms :: PrismsFor (Either a b)

Lenses (mDate, mTalk) = mkLenses :: LensesFor Meetup
Lenses (tTitle, tSpeaker) = mkLenses :: LensesFor Talk
Lenses (dYear, dMonth, dDate) = mkLenses :: LensesFor Date

Prisms _Meetup = mkPrisms :: PrismsFor Meetup
Prisms _Talk = mkPrisms :: PrismsFor Talk
Prisms _Date = mkPrisms :: PrismsFor Date

ix :: Int -> Traversal' [a] a
ix j = mkTraversal $ go j
  where
    go :: Applicative f => Int -> (a -> f a) -> ([a] -> f [a])
    go _ _ [] = pure []
    go i f (x : xs)
      | i <= 0 = (: xs) <$> f x
      | otherwise = (x :) <$> go (i - 1) f xs

exampleMeetup :: Meetup
exampleMeetup =
  Meetup (Date 2017 03 16) (Talk "Simulating subtyping in Haskell" "Andres Loeh")
