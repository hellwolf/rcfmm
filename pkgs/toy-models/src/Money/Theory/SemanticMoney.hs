{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Theory.SemanticMoney where

import           Data.Default

--
-- General Payment Primitives
--

class ( Integral t
      , Integral v
      ) => MonetaryUnit t v m | m -> t, m -> v where
    rtb :: m -> t -> v

class ( Integral t
      , Integral v
      ) => Index t v a | a -> t, a -> v where
    settle :: (Integral t, Integral v)
           => a -> t -> a
    shift1 :: Integral v
           => a -> v -> (a, v)
    flow1  :: Integral v
           => a -> v -> (a, v)

shift2 :: (Index t v a, Index t v b)
       => (a, b) -> v -> t -> (a, b)
shift2 (a, b) amount t = (a', b')
    where (b', amount') = shift1 (settle b t) amount
          (a', _) = shift1 (settle a t) (-amount')

flow2 :: (Index t v a, Index t v b)
      => (a, b) -> v -> t -> (a, b)
flow2 (a, b) flowRate t = (a', b')
    where (b', flowRate') = flow1 (settle b t) flowRate
          (a', _) = flow1 (settle a t) (-flowRate')

data RTBParticle t v = RTBParticle
    { rtb_settled_at    :: t
    , rtb_settled_value :: v
    , rtb_flow_rate     :: v
    }
instance (Integral t, Integral v) => Default (RTBParticle t v) where def = RTBParticle 0 0 0

instance ( Integral t
         , Integral v
         ) => MonetaryUnit t v (RTBParticle t v) where
    rtb (RTBParticle t s r) t' = r * (fromInteger . toInteger)(t' - t) + s

instance ( Integral t
         , Integral v
         ) => Index t v (RTBParticle t v) where
    settle idx t' = idx { rtb_settled_at = t'
                        , rtb_settled_value = rtb idx t'
                        }

    shift1 idx x = (idx { rtb_settled_value = rtb_settled_value idx + x}, x)

    flow1 idx r' = (idx { rtb_flow_rate = r' }, r')

--
-- Univeral Index
--
newtype UniversalIndex t v = UniversalIndex (RTBParticle t v)
deriving newtype instance (Integral t, Integral v) => Default (UniversalIndex t v)
deriving newtype instance (Integral t, Integral v) => MonetaryUnit t v (UniversalIndex t v)
deriving newtype instance (Integral t, Integral v) => Index t v (UniversalIndex t v)

-- TODO make it monoid

--
-- Proportional Distribution Index
--

data PDIndex t v u = Integral u => PDIndex
    { pdidx_total_units :: u
    , pdidx_rtbp        :: RTBParticle t v
    }
instance (Integral t, Integral v, Integral u) => Default (PDIndex t v u) where def = PDIndex 0 def

data PDPoolMember t v u = Integral u => PDPoolMember
    { pdpm_owned_unit :: u
    , pdpm_rtbp       :: RTBParticle t v
    -- , pdpm_synced_at             :: t
    -- , pdpm_synced_unit_value     :: v
    -- , pdpm_synced_unit_flow_rate :: v
    }
instance (Integral t, Integral v, Integral u) => Default (PDPoolMember t v u) where def = PDPoolMember 0 def

type PDPoolMemberMU t v u = (PDIndex t v u, PDPoolMember t v u)

class PDPoolStore ps t v u l | ps -> t, ps -> v, ps -> u, ps -> l where
    pdps_lookup_member :: l -> ps -> PDPoolMember t v u
    pdps_insert_member :: l -> PDPoolMember t v u -> ps -> ps

type PDPool t v u ps l = (PDIndex t v u, ps)

pdp_update_member :: ( Integral t
                     , Integral v
                     , Integral u
                     , PDPoolStore ps t v u l
                     , p ~ PDPool t v u ps l
                     )
                  => l -> u -> t -> p -> p
pdp_update_member lbl unit' t' (idx, ps) =
    (idx', pdps_insert_member lbl pm' ps)
    where pm = pdps_lookup_member lbl ps
          unit = pdpm_owned_unit pm
          idx' = (settle idx t') { pdidx_total_units = pdidx_total_units idx + unit' - unit }
          pm'  = pm { pdpm_owned_unit = unit'
                    , pdpm_rtbp = pdidx_rtbp idx'
                    }

instance ( Integral t
         , Integral v
         ) => MonetaryUnit t v (PDPoolMemberMU t v u) where
    rtb (PDIndex _ rpi, PDPoolMember u rps) t' = (fromInteger . toInteger) u * (
        - rtb rps (ti - ts) -- cancel out of sync real time balance between [ts:ti]
        + rtb rpi (t' - ti) -- inclusion of real time balance between [ti:ts]
        ) where ti = rtb_settled_at rpi
                ts = rtb_settled_at rps

instance ( Integral t
         , Integral v
         ) => Index t v (PDIndex t v u) where
    settle idx@(PDIndex _ rpi) t' = idx { pdidx_rtbp = settle rpi t' }

    shift1 idx@(PDIndex tu rpi) x = (idx { pdidx_rtbp = rpi' }, ux'' * tu')
        where tu' = (fromInteger . toInteger) tu
              ux' = if tu == 0 then 0 else x `div` tu'
              (rpi', ux'') = shift1 rpi ux'

    flow1 idx@(PDIndex tu rpi) r' = (idx { pdidx_rtbp = rpi' }, ur'' * tu')
        where tu' = (fromInteger . toInteger) tu
              ur' = if tu == 0 then 0 else r' `div` tu'
              (rpi', ur'') = flow1 rpi ur'
