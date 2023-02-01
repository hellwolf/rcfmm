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
    settle :: a -> t -> a
    shift1 :: a -> v -> (a, v)
    flow1  :: a -> v -> (a, v)

shift2 :: (Index mt t v a, Index mt t v b)
       => (a, b) -> v -> t -> (a, b)
shift2 (a, b) amount t = (a', b')
    where (b', amount') = shift1 (settle b t) amount
          (a', _) = shift1 (settle a t) (-amount')

flow2 :: (Index mt t v a, Index mt t v b)
      => (a, b) -> v -> t -> (a, b)
flow2 (a, b) flowRate t = (a', b')
    where (b', flowRate') = flow1 (settle b t) flowRate
          (a', _) = flow1 (settle a t) (-flowRate')

data RTBParticle mt = RTBParticle { rtb_settled_at    :: MT_TIME  mt
                                  , rtb_settled_value :: MT_VALUE mt
                                  , rtb_flow_rate     :: MT_VALUE mt
                                  }

rtbp_scale :: ( MonetaryTypes mt, u ~ MT_UNIT mt
              ) => RTBParticle mt -> u -> u -> RTBParticle mt
rtbp_scale rp old new = rp
    { rtb_settled_value = if old' == 0 then 0 else rtb_settled_value rp * old' `div` new'
    , rtb_flow_rate = if old' == 0 then 0 else rtb_flow_rate rp * old' `div` new'
    }
    where new' = (fromInteger . toInteger) new
          old' = (fromInteger . toInteger) old

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => Default (RTBParticle mt) where def = RTBParticle 0 0 0

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => MonetaryUnit mt t v (RTBParticle mt) where
    rtb (RTBParticle t s r) t' = r * (fromInteger . toInteger)(t' - t) + s

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => Index mt t v (RTBParticle mt) where
    settle idx t' = idx { rtb_settled_at = t'
                        , rtb_settled_value = rtb idx t'
                        }

    shift1 idx x = (idx { rtb_settled_value = rtb_settled_value idx + x}, x)

    flow1 idx r' = (idx { rtb_flow_rate = r' }, r')

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

data PDPoolMember mt = PDPoolMember { pdpm_owned_unit :: MT_UNIT mt
                                    , pdpm_rtbp       :: RTBParticle mt
                                    }
instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
         ) => Default (PDPoolMember mt) where def = PDPoolMember 0 def

type PDPoolMemberMU mt t v u = (PDIndex mt, PDPoolMember mt)

pdp_update_member :: ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt, u ~ MT_UNIT mt
                     , mu ~ PDPoolMemberMU mt t v u
                     ) => u -> t -> mu -> mu
pdp_update_member unit' t' (idx, pm) = (idx', pm')
    where unit = pdpm_owned_unit pm
          tu  = pdidx_total_units idx
          tu' = tu + unit' - unit
          idx' = (settle idx t')
                 { pdidx_total_units = tu'
                 , pdidx_rtbp = rtbp_scale (pdidx_rtbp idx) tu tu'
                 }
          pm'  = pm { pdpm_owned_unit = unit'
                    , pdpm_rtbp = pdidx_rtbp idx'
                    }

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => MonetaryUnit mt t v (PDPoolMemberMU mt t v u) where
    rtb (PDIndex _ rpi, PDPoolMember u rps) t' = (fromInteger . toInteger) u * (
        rtb rpi t' +      -- include index's rtb
        rtb rps (ts - ti) -- cancel out-of-sync member's rtb between [ts:ti]
        ) where ti = rtb_settled_at rpi
                ts = rtb_settled_at rps

instance ( MonetaryTypes mt, t ~ MT_TIME mt, v ~ MT_VALUE mt
         ) => Index mt t v (PDIndex mt) where
    settle idx@(PDIndex _ rpi) t' = idx { pdidx_rtbp = settle rpi t' }

    shift1 idx@(PDIndex tu rpi) x = (idx { pdidx_rtbp = rpi' }, ux'' * tu')
        where tu' = (fromInteger . toInteger) tu
              ux' = if tu == 0 then 0 else x `div` tu'
              (rpi', ux'') = shift1 rpi ux'

    flow1 idx@(PDIndex tu rpi) r' = (idx { pdidx_rtbp = rpi' }, ur'' * tu')
        where tu' = (fromInteger . toInteger) tu
              ur' = if tu == 0 then 0 else r' `div` tu'
              (rpi', ur'') = flow1 rpi ur'
