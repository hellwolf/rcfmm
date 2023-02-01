{-# LANGUAGE TypeFamilies #-}

module Money.Theory.TestMonetaryTypes where

import           Money.Theory.SemanticMoney


data TestMonetaryTypes
instance MonetaryTypes TestMonetaryTypes where
    type MT_TIME  TestMonetaryTypes = Integer
    type MT_VALUE TestMonetaryTypes = Integer
    type MT_UNIT  TestMonetaryTypes = Integer

deriving instance Show (RTBParticle TestMonetaryTypes)

type TestUniversalIndex = UniversalIndex TestMonetaryTypes
deriving instance Show TestUniversalIndex

type TestPDPoolIndex = PDIndex TestMonetaryTypes
deriving instance Show TestPDPoolIndex

type TestPDPoolMember = PDPoolMember TestMonetaryTypes
deriving instance Show TestPDPoolMember

-- type TestPDPoolStore = M.Map String TestPDPoolMember
