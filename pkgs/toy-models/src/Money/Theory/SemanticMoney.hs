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
    settledAt :: mu -> t
    rtb       :: mu -> t -> v

 -- * On right side biased operations:
 --   1) Right side produces error term with which left side is adjusted accordingly.
 --   2) The adjustment must not produce new error term, or otherwise it would require recursive adjustments.

class ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
      ) => Index mt t v u ix | ix -> mt where
    -- settle 1-primitive
    settle1 :: t -> ix -> ix
    -- shift 1-primitive
    shift1      :: v -> ix -> (ix, v)
    -- (constant) flow 1-primitive
    getFlowRate :: ix -> v
    setFlow1    :: v -> ix -> (ix, v)
    shiftFlow1  :: v -> ix -> (ix, v)
    shiftFlow1 v a = setFlow1 (v + getFlowRate a) a
    -- align 2-primitive, right side biased
    align2      :: forall a. Index mt t v u a => u -> u -> (a, ix) -> (a, ix)

-- polymorphic 2-primitives
--

-- 2-primitive pattern
mu_op2 :: (Index mt t v u a, Index mt t v u b)
       => ((a, b) -> (a, b)) -> t -> (a, b) -> (a, b)
mu_op2 op t (a, b) = op (settle1 t a, settle1 t b)

-- shift2, right side biased
shift2 :: (Index mt t v u a, Index mt t v u b)
       => v -> t -> (a, b) -> (a, b)
shift2 amount = mu_op2 op
    where op (a, b) = let (b', amount') = shift1 amount b
                          (a', _) = shift1 (-amount') a
                      in (a', b')

-- flow2, right side biased
flow2 :: (Index mt t v u a, Index mt t v u b)
      => v -> t -> (a, b) -> (a, b)
flow2 flowRate = mu_op2 op
    where op (a, b) = let (b', flowRate') = setFlow1 flowRate b
                          (a', _) = setFlow1 (-flowRate') a
                      in (a', b')

--
-- Univeral Index
--

newtype UniversalIndex mt wi = UniversalIndex wi
deriving newtype instance ( MonetaryTypes mt
                          , Default wi) => Default (UniversalIndex mt wi)
deriving newtype instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
                          , MonetaryUnit mt t v wi) => MonetaryUnit mt t v (UniversalIndex mt wi)
deriving newtype instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
                          , Index mt t v u wi) => Index mt t v u (UniversalIndex mt wi)

-- TODO make it monoid

--
-- Proportional Distribution Pool
--

data PDPoolIndex mt wi = PDPoolIndex { pdidx_total_units :: MT_UNIT mt
                                     , pdidx_rtbp        :: wi
                                     }
instance (MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
         , Default wi) => Default (PDPoolIndex mt wi) where def = PDPoolIndex 0 def

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         , MonetaryUnit mt t v wi) => MonetaryUnit mt t v (PDPoolIndex mt wi) where
    settledAt (PDPoolIndex _ rpi) = settledAt rpi
    rtb (PDPoolIndex tu rpi) t' = rtb rpi t' `mt_v_mul_u` tu

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
         , Index mt t v u wi) => Index mt t v u (PDPoolIndex mt wi) where
    settle1 t' a@(PDPoolIndex _ rpi) = a { pdidx_rtbp = settle1 t' rpi }

    align2 = error "PDPoolIndex does not support right side biased alignment"

    shift1 x a@(PDPoolIndex tu rpi) = (a { pdidx_rtbp = rpi' }, x' `mt_v_mul_u` tu)
        where (rpi', x') = if tu == 0 then (rpi, 0) else shift1 (fst (x `mt_v_qr_u` tu)) rpi

    getFlowRate (PDPoolIndex _ rpi) = getFlowRate rpi

    setFlow1 r' a@(PDPoolIndex tu rpi) = (a { pdidx_rtbp = rpi' }, r'' `mt_v_mul_u` tu)
        where (rpi', r'') = if tu == 0 then (rpi, 0) else setFlow1 (fst (r' `mt_v_qr_u` tu)) rpi

data PDPoolMember mt wi = PDPoolMember { pdpm_owned_unit    :: MT_UNIT mt
                                       , pdpm_settled_value :: MT_VALUE mt
                                       , pdpm_synced_rtbp   :: wi
                                       }
instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
         , Default wi) => Default (PDPoolMember mt wi) where def = PDPoolMember 0 0 def

type PDPoolMemberMU mt wi = (PDPoolIndex mt wi, PDPoolMember mt wi)

pdpUpdateMember2 :: ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
                    , Index mt t v u a
                    , MonetaryUnit mt t v wi
                    , Index mt t v u wi
                    , mu ~ PDPoolMemberMU mt wi
                    ) => u -> t -> (a, mu) -> (a, mu)
pdpUpdateMember2 u' t' (a, (b, pm)) = (a', (b', pm'))
    where (PDPoolIndex tu rpi) = settle1 t' b
          (PDPoolMember u _ rpm) = pm
          tu' = tu + u' - u
          sv' = (rtb rpi t' - rtb rpm t') `mt_v_mul_u` u
          (rpi', a') = align2 tu tu' (rpi, settle1 t' a)
          b'  = PDPoolIndex tu' rpi'
          pm' = PDPoolMember u' sv' rpi'

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         , MonetaryUnit mt t v wi) => MonetaryUnit mt t v (PDPoolMemberMU mt wi) where
    settledAt (_, PDPoolMember _ _ rpm)= settledAt rpm
    rtb (PDPoolIndex _ rpi, PDPoolMember u sv rpm) t' = sv +
        -- let ti = rtb_settled_at rpi
        --     ts = rtb_settled_at rpm
        -- in (rtb rpi t' - rtb rpm ti) -- include index's current accruals for the member
        -- +  (rtb rpm ti - rtb rpm ts) -- cancel out-of-sync member's rtb between [ts:ti]
        -- =>
        (rtb rpi t' - rtb rpm (settledAt rpm)) `mt_v_mul_u` u

--
-- Particles: final index as building block for other indexes
--

data RTBParticle mt = RTBParticle { rtb_settled_at    :: MT_TIME  mt
                                  , rtb_settled_value :: MT_VALUE mt
                                  , rtb_flow_rate     :: MT_VALUE mt
                                  }
instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => Default (RTBParticle mt) where def = RTBParticle 0 0 0

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => MonetaryUnit mt t v (RTBParticle mt) where
    settledAt = rtb_settled_at
    rtb (RTBParticle t s r) t' = r `mt_v_mul_t` (t' - t) + s

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
         ) => Index mt t v u (RTBParticle mt) where
    settle1 t' a = a { rtb_settled_at = t'
                    , rtb_settled_value = rtb a t'
                    }

    align2 u u' (a, b) = (a', b')
        where r = getFlowRate a
              (r', er') = if u' == 0 then (0, r `mt_v_mul_u` u) else r `mt_v_mul_u_qr_u` (u, u')
              a' = (fst . setFlow1 r') $ a
              b' = (fst . shiftFlow1 er') $ b

    shift1 x a = (a { rtb_settled_value = rtb_settled_value a + x }, x)

    getFlowRate = rtb_flow_rate

    setFlow1 r' a = (a { rtb_flow_rate = r' }, r')
