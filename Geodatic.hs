{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, Rank2Types, FlexibleContexts, TypeSynonymInstances, LiberalTypeSynonyms #-}

module Geodatic ( GeodaticModelT(..)
                , ans, grs80, wgs84, ans', grs80', wgs84' 
                , toEcef, toGeodatic ) where

import qualified Prelude
import Numeric.Units.Dimensional.Prelude

data GeodaticModelT where
  ANS :: GeodaticModelT
  GRS80 :: GeodaticModelT
  WGS84 :: GeodaticModelT
  deriving (Eq, Show)

instance GeodaticModel GeodaticModelT where
  semiMajorAxis ANS = (6378160.0 *~ meter)
  semiMajorAxis GRS80 = (6378137.0 *~ meter)
  semiMajorAxis WGS84 = (6378137.0 *~ meter)
  recProcFlattening ANS = (298.25 *~ one)
  recProcFlattening GRS80 = (298.257222101 *~ one)
  recProcFlattening WGS84 = (298.257223563 *~ one)

class (Eq m, Show m) => GeodaticModel m where
  semiMajorAxis :: (Fractional t) => m -> Length t
  recProcFlattening :: (Fractional t) => m -> Dimensionless t
  flattening :: (Fractional t) => m -> Dimensionless t
  flattening m = _1 / (recProcFlattening m)
  semiMinorAxis :: (Fractional t) => m -> Length t
  semiMinorAxis m = (semiMajorAxis m) * (_1 - flattening m)
  fstEccentricity :: (Floating t, Fractional t) => m -> Dimensionless t
  fstEccentricity m =
    let f = flattening m
    in (_2 * f) - (f ** _2)
  sndEccentricity :: (Floating t, Fractional t) => m -> Dimensionless t
  sndEccentricity m =     
    let f = flattening m
        a = f * (_2 - f) 
        b = ((_1 - f) ** _2)
    in  a / b
       
data Coordinate t where
  GeodaticCoordinate :: (Eq t, Show t, Num t, RealFloat t) => 
                        GeodaticModelT -> 
                        PlaneAngle t-> 
                        PlaneAngle t -> 
                        Length t -> 
                        Coordinate t
  ECEFCoordinate :: (Eq t, Show t, Num t, RealFloat t) =>Length t -> Length t -> Length t -> Coordinate t
  



instance Eq (Coordinate t) where
  (ECEFCoordinate x y z) == (ECEFCoordinate x' y' z') = (x == x') &&
                                                        (y == y') && 
                                                        (z == z')
  a@(GeodaticCoordinate m φ λ h) == b@(GeodaticCoordinate m' φ' λ' h') =
    if (m == m') then (φ == φ') && (λ == λ') && (h == h) else
      toGeodatic m b == a
  a == b = toEcef a == toEcef b

--instance Num (Coordinate t) where
  

instance Show (Coordinate t) where
  show (GeodaticCoordinate m φ λ h) = show (m,φ,λ,h)
  show (ECEFCoordinate x y z) = show (x,y,z)
  


ans' φ λ h= GeodaticCoordinate ANS φ λ h
ans φ λ h = ans' (φ *~ degree) (λ *~ degree) (h *~ meter)

grs80' φ λ h = GeodaticCoordinate GRS80 φ λ h
grs80 φ λ h = grs80' (φ *~ degree) (λ *~ degree) (h *~ meter)

wgs84' φ λ h = GeodaticCoordinate WGS84 φ λ h
wgs84 φ λ h = wgs84'(φ *~ degree) (λ *~ degree) (h *~ meter)

toEcef :: Coordinate t -> Coordinate t
toEcef c@(ECEFCoordinate _ _ _) = c
toEcef (GeodaticCoordinate m φ λ h) =
  let e2 = fstEccentricity m
      x = sqrt (_1 - (e2 * ((sin φ) ** _2)))
      a = semiMajorAxis m
      normal = a / x
      normalh = normal + h
      rx = normalh * (cos φ) * (cos λ)
      ry = normalh * (cos φ) * (sin λ)
      rz = (((a * ( _1 - e2)) / x) + h) * (sin φ)
  in ECEFCoordinate rx ry rz
     
toGeodatic :: GeodaticModelT -> Coordinate t -> Coordinate t
toGeodatic tm c@(GeodaticCoordinate m φ λ h) =
  if (tm == m) then c else toGeodatic tm (toEcef c)
toGeodatic tm c@(ECEFCoordinate x y z) = 
  let a = semiMajorAxis tm
      b = semiMinorAxis tm
      e2 = fstEccentricity tm
      e'2 = sndEccentricity tm
      r = sqrt ((x * x) + (y * y))
      ee2 = (a * a) - (b * b)
      f = (54 *~ one) * (b * b) * (z * z)
      g = (r * r) + ((_1 - e2) * z * z) - (e2 * e2 * ee2)
      c = (e2 * e2 * f * r * r ) / ( g * g * g )
      s = cbrt (_1 + c + sqrt ((c*c) + (_2 * c)))  
      p = f / (_3 * ((s + (_1 / s) + _1) ** _2) * (g * g))
      q = sqrt (_1 + (_2 * e2 * e2 * p))
      r0a = ((_0 - _1) * (p * e2 * r)) / (_1 + q) 
      r0b = sqrt ((((_1 / _2) * a * a) * (_1 + (_1 / q)))- 
                  ((p * (_1 - e2) * z * z)/(q * (_1 + q))) - 
                  ((_1 / _2) * p * r * r))
      r0 = r0a + r0b
      ub = z * z
      ua = (r  - (e2 * r0)) * (r  - (e2 * r0))
      u = sqrt (ua + ub) 
      v = sqrt (ua + ((_1 - e2) * ub))
      z0 = (b * b * z) / (a * v)
      h = u * (_1 - ((b * b)/(a * v)))
      φ = atan ((z + (e'2 * z0)) / r)
      λ = atan2 y x
  in GeodaticCoordinate tm φ λ h


gcDist :: Coordinate t -> Coordinate t -> Length t
gcDist a@(ECEFCoordinate _ _ _) b@(ECEFCoordinate _ _ _) =
  gcDist (toGeodatic WGS84 a) (toGeodatic WGS84 b)
gcDist a@(GeodaticCoordinate m _ _ _) b@(ECEFCoordinate _ _ _) =
  gcDist a (toGeodatic m b)
gcDist a@(ECEFCoordinate _ _ _) b@(GeodaticCoordinate m _ _ _) =
  gcDist (toGeodatic m a) b
gcDist c1@(GeodaticCoordinate m1 φ1 λ1 __) c2@(GeodaticCoordinate m2 φ2 λ2 _)
  | (m1 /= m2) = gcDist c1 (toGeodatic m1 c2)
  | otherwise = 
      let dλ = abs $ λ1 - λ2
          a1 = (cos φ1 * sin dλ)
          a2 = (cos φ2 * sin φ1) - (sin φ2 * cos φ1 * cos dλ)
          a = sqrt ((a1 * a1) + (a2 *a2))     
          b = (sin φ2 * sin φ1) + (cos φ2 * cos φ1 * cos dλ)      
          dσ = atan (a / b)
          r = 6371.01 *~ kilo meter
      in r * dσ
  


ms = wgs84 51.969659 7.605286 0
hh = wgs84 53.543572 10.02502 0
d1 = wgs84 36.12 (-86.67) 0
d1a = ans 36.12 (-86.67) 0
d2 = wgs84 33.94 (-118.40) 0

