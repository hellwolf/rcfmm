{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}

module Money.Theory.SemanticMoney_prop (tests) where

import           Data.Default
import qualified Data.Map.Lazy              as M
import           Data.Maybe                 (fromMaybe)
import           Test.Hspec
import           Test.QuickCheck

import           Money.Theory.SemanticMoney


type TestUniversalIndex = UniversalIndex Integer Integer

type TestPDPoolIndex = PDIndex Integer Integer Integer
type TestPDPoolMember = PDPoolMember Integer Integer Integer
type TestPDPoolStore = M.Map String TestPDPoolMember
instance PDPoolStore TestPDPoolStore Integer Integer Integer String where
    pdps_lookup_member lbl ps = fromMaybe def (M.lookup lbl ps)
    pdps_insert_member = M.insert

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

uidx_pdidx_1member_shift2 u1 x t = rtb a 0 + rtb b1 0 == rtb a' t + rtb b1' t
    where a = def :: TestUniversalIndex
          (bi, bs) = pdp_update_member "alice" u1 0 (def :: (TestPDPoolIndex, TestPDPoolStore))
          b1 = (bi, pdps_lookup_member "alice" bs)
          (a', bi') = shift2 (a, bi) x t
          b1' = (bi', pdps_lookup_member "alice" bs)

uidx_pdidx_1member_flow2 u1 r t = rtb a 0 + rtb b1 0 == rtb a' t + rtb b1' t
    where a = def :: TestUniversalIndex
          (bi, bs) = pdp_update_member "alice" u1 0 (def :: (TestPDPoolIndex, TestPDPoolStore))
          b1 = (bi, pdps_lookup_member "alice" bs)
          (a', bi') = flow2 (a, bi) r 0
          b1' = (bi', pdps_lookup_member "alice" bs)

tests = describe "Semantic money properties" $ do
    -- uidx -> uidx
    it "uidx:uidx shift2" $ property uidx_uidx_shift2
    it "uidx:uidx twice shift2" $ property uidx_uidx_twice_shift2
    it "uidx:uidx flow2" $ property uidx_uidx_flow2
    it "uidx:uidx twice flow2" $ property uidx_uidx_twice_flow2
    it "uidx:uidx shift2 then flow2" $ property uidx_uidx_shift2_flow2
    it "uidx:uidx flow2 then shift2" $ property uidx_uidx_flow2_shift2
    -- uidx -> pdidx with 1 member
    it "uidx:pdidx_1member shift2" $ property uidx_pdidx_1member_shift2
    it "uidx:pdidx_1member flow2" $ property uidx_pdidx_1member_flow2
