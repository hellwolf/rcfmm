{-# LANGUAGE TypeFamilies #-}

module Money.Theory.TestMonetaryTypes where

import           Test.QuickCheck

import           Money.Theory.SemanticMoney


newtype TestTime = TestTime Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
instance Arbitrary TestTime where
    arbitrary = TestTime <$> arbitrary

newtype TestMValue = TestMValue Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
instance Arbitrary TestMValue where
    arbitrary = TestMValue <$> arbitrary

newtype TestMUnit = TestMUnit Integer deriving (Enum, Eq, Ord, Num, Real, Integral, Show)
instance Arbitrary TestMUnit where
    arbitrary = TestMUnit . getNonNegative <$> arbitrary

data TestMonetaryTypes
instance MonetaryTypes TestMonetaryTypes where
    type MT_TIME  TestMonetaryTypes = TestTime
    type MT_VALUE TestMonetaryTypes = TestMValue
    type MT_UNIT  TestMonetaryTypes = TestMUnit

deriving instance Show (RTBParticle TestMonetaryTypes)

type TesRTBParticle = RTBParticle TestMonetaryTypes

type TestUniversalIndex = UniversalIndex TestMonetaryTypes TesRTBParticle
deriving instance Show TestUniversalIndex

type TestPDPoolIndex = PDPoolIndex TestMonetaryTypes TesRTBParticle
deriving instance Show TestPDPoolIndex

type TestPDPoolMember = PDPoolMember TestMonetaryTypes TesRTBParticle
deriving instance Show TestPDPoolMember

type TestPDPoolMemberMU = PDPoolMemberMU TestMonetaryTypes TesRTBParticle

-- type TestPDPoolStore = M.Map String TestPDPoolMember
