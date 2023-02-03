{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Money.Theory.SemanticMoney where

import           Data.Default
import           Data.Kind    (Type)


-- | Type system trite: types used in semantic money
--
-- Note:
--   * Index related types through associated type families.
--   * Use type family dependencies to make these types to the index type injective.
class ( Integral (MT_TIME mt)
      , Integral (MT_VALUE mt)
      , Integral (MT_UNIT  mt)
      ) => MonetaryTypes mt where
    mt_v_mul_t :: MT_VALUE mt -> MT_TIME mt -> MT_VALUE mt
    mt_v_mul_t v t = v * (fromInteger . toInteger) t
    mt_v_mul_u :: MT_VALUE mt -> MT_UNIT mt -> MT_VALUE mt
    mt_v_mul_u v u = v * (fromInteger . toInteger) u
    mt_v_qr_u :: MT_VALUE mt -> MT_UNIT mt -> (MT_VALUE mt, MT_VALUE mt)
    mt_v_qr_u v u = let u' = (fromInteger . toInteger) u in v `quotRem` u'
    mt_v_mul_u_qr_u :: MT_VALUE mt -> (MT_UNIT mt, MT_UNIT mt) -> (MT_VALUE mt, MT_VALUE mt)
    mt_v_mul_u_qr_u v (u1, u2) = (v * (fromInteger . toInteger) u1) `quotRem` (fromInteger . toInteger) u2

    type family MT_TIME  mt = (t :: Type) | t -> mt
    type family MT_VALUE mt = (v :: Type) | v -> mt
    type family MT_UNIT  mt = (u :: Type) | u -> mt

--
-- General Payment Primitives
--

class ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
      ) => MonetaryUnit mt t v mu | mu -> mt where
    rtb :: mu -> t -> v

class ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
      ) => Index mt t v a | a -> mt where
    settle      :: t -> a -> a
    shift1      :: v -> a -> (a, v)
    getFlow1    :: a -> v
    setFlow1    :: v -> a -> (a, v)
    shiftFlow1  :: v -> a -> (a, v)
    shiftFlow1 v a = setFlow1 (v + getFlow1 a) a

indexOp2 :: (Index mt t v a, Index mt t v b)
         => ((a, b) -> (a, b)) -> t -> (a, b) -> (a, b)
indexOp2 op t (a, b) = op (settle t a, settle t b)

shift2 :: (Index mt t v a, Index mt t v b)
       => v -> t -> (a, b) -> (a, b)
shift2 amount = indexOp2 op
    where op (a, b) = let (b', amount') = shift1 amount b
                          (a', _) = shift1 (-amount') a
                      in (a', b')

flow2 :: (Index mt t v a, Index mt t v b)
      => v -> t -> (a, b) -> (a, b)
flow2 flowRate = indexOp2 op
    where op (a, b) = let (b', flowRate') = setFlow1 flowRate b
                          (a', _) = setFlow1 (-flowRate') a
                      in (a', b')

--- TODO particle independent

data RTBParticle mt = RTBParticle { rtb_settled_at    :: MT_TIME  mt
                                  , rtb_settled_value :: MT_VALUE mt
                                  , rtb_flow_rate     :: MT_VALUE mt
                                  }

rtbp_align_index :: ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
                    , Index mt t v a
                    ) => u -> u -> (RTBParticle mt, a) -> (RTBParticle mt, a)
rtbp_align_index old new (rpa, a) = ( rpa { rtb_flow_rate = r'} , a')
    where r = rtb_flow_rate rpa
          (r', er') = if new == 0 then (0, r `mt_v_mul_u` old) else r `mt_v_mul_u_qr_u` (old, new)
          a' = fst . shiftFlow1 er' $ a

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => Default (RTBParticle mt) where def = RTBParticle 0 0 0

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => MonetaryUnit mt t v (RTBParticle mt) where
    rtb (RTBParticle t s r) t' = r `mt_v_mul_t` (t' - t) + s

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => Index mt t v (RTBParticle mt) where
    settle t' a = a { rtb_settled_at = t'
                    , rtb_settled_value = rtb a t'
                    }

    shift1 x a = (a { rtb_settled_value = rtb_settled_value a + x }, x)

    getFlow1 = rtb_flow_rate

    setFlow1 r' a = (a { rtb_flow_rate = r' }, r')

--
-- Univeral Index
--

newtype UniversalIndex mt = UniversalIndex (RTBParticle mt)
deriving newtype instance MonetaryTypes mt => Default (UniversalIndex mt)
deriving newtype instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
                          ) => MonetaryUnit mt t v (UniversalIndex mt)
deriving newtype instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
                          ) => Index mt t v (UniversalIndex mt)

-- TODO make it monoid

--
-- Proportional Distribution Index
--

data PDIndex mt = PDIndex
                  { pdidx_total_units :: MT_UNIT mt
                  , pdidx_rtbp        :: RTBParticle mt
                  }
instance (MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
         ) => Default (PDIndex mt) where def = PDIndex 0 def

data PDPoolMember mt = PDPoolMember { pdpm_owned_unit    :: MT_UNIT mt
                                    , pdpm_settled_value :: MT_VALUE mt
                                    , pdpm_synced_rtbp   :: RTBParticle mt
                                    }
instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
         ) => Default (PDPoolMember mt) where def = PDPoolMember 0 0 def

type PDPoolMemberMU mt = (PDIndex mt, PDPoolMember mt)

pdpUpdateMember2 :: ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
                    , Index mt t v a
                    , mu ~ PDPoolMemberMU mt
                    ) => u -> t -> (a, mu) -> (a, mu)
pdpUpdateMember2 u' t' (a, (b, pm)) = (a', (b', pm'))
    where sb = settle t' b
          tu  = pdidx_total_units sb
          rpi = pdidx_rtbp sb
          rpm = pdpm_synced_rtbp pm
          u = pdpm_owned_unit pm
          sv = (rtb rpi t' - rtb rpm t') `mt_v_mul_u` u
          tu' = tu + u' - u
          (rpi', a') = rtbp_align_index tu tu' (rpi, settle t' a)
          b' = sb { pdidx_total_units = tu'
                  , pdidx_rtbp = rpi'
                  }
          pm' = pm { pdpm_owned_unit = u'
                   , pdpm_settled_value = sv
                   , pdpm_synced_rtbp = rpi'
                   }

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => MonetaryUnit mt t v (PDPoolMemberMU mt) where
    rtb (PDIndex _ rpi, PDPoolMember u sv rpm) t' =
        -- let ti = rtb_settled_at rpi
        --     ts = rtb_settled_at rpm
        -- in (rtb rpi t' - rtb rpm ti) -- include index's current accruals for the member
        -- +  (rtb rpm ti - rtb rpm ts) -- cancel out-of-sync member's rtb between [ts:ti]
        -- =>
        (rtb rpi t' - rtb rpm (rtb_settled_at rpm)) `mt_v_mul_u` u
        + sv

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => Index mt t v (PDIndex mt) where
    settle t' a@(PDIndex _ rpi) = a { pdidx_rtbp = settle t' rpi }

    shift1 x a@(PDIndex tu rpi) = (a { pdidx_rtbp = rpi' }, x' `mt_v_mul_u` tu)
        where (rpi', x') = if tu == 0 then (rpi, 0) else shift1 (fst (x `mt_v_qr_u` tu)) rpi

    getFlow1 (PDIndex _ rpi) = getFlow1 rpi

    setFlow1 r' a@(PDIndex tu rpi) = (a { pdidx_rtbp = rpi' }, r'' `mt_v_mul_u` tu)
        where (rpi', r'') = if tu == 0 then (rpi, 0) else setFlow1 (fst (r' `mt_v_qr_u` tu)) rpi
