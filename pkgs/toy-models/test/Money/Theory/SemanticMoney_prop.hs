{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Theory.SemanticMoney_prop (tests) where

import           Data.Default
import           Test.Hspec
import           Test.QuickCheck

import           Money.Theory.SemanticMoney
import           Money.Theory.TestMonetaryTypes


uidx_uidx_shift2 x t = rtb a 0 + rtb b 0 == rtb a' t + rtb b' t
    where a = def :: TestUniversalIndex
          b = def :: TestUniversalIndex
          (a', b') = shift2 (a, b) x 0

uidx_uidx_twice_shift2 x t x' t' = rtb a 0 + rtb b 0 == rtb a' t' + rtb b' t'
    where a = def :: TestUniversalIndex
          b = def :: TestUniversalIndex
          (a', b') = shift2 (shift2 (a, b) x 0) x' t

uidx_uidx_flow2 r t = rtb a 0 + rtb b 0 == rtb a' t + rtb b' t
    where a = def :: TestUniversalIndex
          b = def :: TestUniversalIndex
          (a', b') = flow2 (a, b) r 0

uidx_uidx_twice_flow2 r t r' t' = rtb a 0 + rtb b 0 == rtb a' t' + rtb b' t'
    where a = def :: TestUniversalIndex
          b = def :: TestUniversalIndex
          (a', b') = flow2 (flow2 (a, b) r 0) r' t

uidx_uidx_shift2_flow2 x t r' t' = rtb a 0 + rtb b 0 == rtb a' t' + rtb b' t'
    where a = def :: TestUniversalIndex
          b = def :: TestUniversalIndex
          (a', b') = flow2 (shift2 (a, b) x 0) r' t

uidx_uidx_flow2_shift2 x t r' t' = rtb a 0 + rtb b 0 == rtb a' t' + rtb b' t'
    where a = def :: TestUniversalIndex
          b = def :: TestUniversalIndex
          (a', b') = shift2 (flow2 (a, b) x 0) r' t

uidx_pdidx_1member_shift2 u1 x t = rtb a 0 + rtb (b, b1) 0 == rtb a' t + rtb (b', b1) t
    where a  = def :: TestUniversalIndex
          (b, b1) = pdp_update_member u1 0 (def :: (TestPDPoolIndex, TestPDPoolMember))
          (a', b') = shift2 (a, b) x t

uidx_pdidx_1member_twice_shift2 u1 x t x' t' = rtb a 0 + rtb (b, b1) 0 == rtb a' t' + rtb (b', b1) t'
    where a  = def :: TestUniversalIndex
          (b, b1) = pdp_update_member u1 0 (def :: (TestPDPoolIndex, TestPDPoolMember))
          (a', b') = shift2 (shift2 (a, b) x 0) x' t

uidx_pdidx_1member_flow2 u1 r t = rtb a 0 + rtb (b, b1) 0 == rtb a' t + rtb (b', b1) t
    where a = def :: TestUniversalIndex
          (b, b1) = pdp_update_member u1 0 (def :: (TestPDPoolIndex, TestPDPoolMember))
          (a', b') = flow2 (a, b) r 0

uidx_pdidx_1member_twice_flow2 u1 r t r' t' = rtb a 0 + rtb (b, b1) 0 == rtb a' t' + rtb (b', b1) t'
    where a = def :: TestUniversalIndex
          (b, b1) = pdp_update_member u1 0 (def :: (TestPDPoolIndex, TestPDPoolMember))
          (a', b') = flow2 (flow2 (a, b) r 0) r' t

uidx_pdidx_1member_shift2_flow2 u1 x t r t' = rtb a 0 + rtb (b, b1) 0 == rtb a' t' + rtb (b', b1) t'
    where a = def :: TestUniversalIndex
          (b, b1) = pdp_update_member u1 0 (def :: (TestPDPoolIndex, TestPDPoolMember))
          (a', b') = flow2 (shift2 (a, b) x 0) r t

uidx_pdidx_1member_flow2_shift2 u1 r t x t' = rtb a 0 + rtb (b, b1) 0 == rtb a' t' + rtb (b', b1) t'
    where a = def :: TestUniversalIndex
          (b, b1) = pdp_update_member u1 0 (def :: (TestPDPoolIndex, TestPDPoolMember))
          (a', b') = shift2 (flow2 (a, b) r 0) x t

tests = describe "Semantic money properties" $ do
    -- uidx -> uidx
    it "uidx:uidx shift2" $ property uidx_uidx_shift2
    it "uidx:uidx twice shift2" $ property uidx_uidx_twice_shift2
    it "uidx:uidx flow2" $ property uidx_uidx_flow2
    it "uidx:uidx twice flow2" $ property uidx_uidx_twice_flow2
    it "uidx:uidx shift2 >>> flow2" $ property uidx_uidx_shift2_flow2
    it "uidx:uidx flow2 >>> shift2" $ property uidx_uidx_flow2_shift2
    -- uidx -> pdidx with 1 member
    it "uidx:pdidx_1member shift2" $ property uidx_pdidx_1member_shift2
    it "uidx:pdidx_1member twice shift2" $ property uidx_pdidx_1member_twice_shift2
    it "uidx:pdidx_1member flow2" $ property uidx_pdidx_1member_flow2
    it "uidx:pdidx_1member twice flow2" $ property uidx_pdidx_1member_twice_flow2
    it "uidx:pdidx 1member shift2 >>> flow2" $ property uidx_pdidx_1member_shift2_flow2
    it "uidx:pdidx 1member flow2 >>> shift2" $ property uidx_pdidx_1member_flow2_shift2
