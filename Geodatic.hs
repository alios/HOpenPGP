{-# LANGUAGE GADTs, EmptyDataDecls, FlexibleInstances, Rank2Types, FlexibleContexts, TypeSynonymInstances #-}

module ObjectDB where

import qualified Prelude
import Numeric.Units.Dimensional.Prelude

data GeodaticModelT where
  ANS :: GeodaticModelT
  GRS80 :: GeodaticModelT
  WGS84 :: GeodaticModelT
  deriving (Eq, Show)

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
       
instance GeodaticModel GeodaticModelT where
  semiMajorAxis ANS = (6378160.0 *~ meter)
  semiMajorAxis GRS80 = (6378137.0 *~ meter)
  semiMajorAxis WGS84 = (6378137.0 *~ meter)
  recProcFlattening ANS = (298.25 *~ one)
  recProcFlattening GRS80 = (298.257222101 *~ one)
  recProcFlattening WGS84 = (298.257223563 *~ one)

data Geodatic
data ECEF

data Coordinate t where
  GeodaticCoordinate :: (RealFloat t) => GeodaticModelT -> 
                        PlaneAngle t -> PlaneAngle t -> Length t -> Coordinate Geodatic
  ECEFCoordinate :: (RealFloat t, Show t) => Length t -> Length t -> Length t -> Coordinate ECEF
  
  
instance Show (Coordinate t) where
  show (GeodaticCoordinate m phi lambda h) = show (m,phi,lambda,h)
  show (ECEFCoordinate x y z) = show (x,y,z)
  
toEcef :: Coordinate t -> Coordinate ECEF
toEcef c@(ECEFCoordinate _ _ _) = c
toEcef (GeodaticCoordinate m phi lambda h) =
  let e2 = fstEccentricity m
      x = sqrt (_1 - (e2 * ((sin phi) ** _2)))
      a = semiMajorAxis m
      normal = a / x
      normalh = normal + h
      rx = normalh * (cos phi) * (cos lambda)
      ry = normalh * (cos phi) * (sin lambda)
      rz = (((a * ( _1 - e2)) / x) + h) * (sin phi)
  in ECEFCoordinate rx ry rz
     
toGeodatic :: Coordinate t -> GeodaticModelT -> Coordinate Geodatic
toGeodatic c@(GeodaticCoordinate m phi lambda h) tm =
  if (tm == m) then c else toGeodatic (toEcef c) tm
toGeodatic c@(ECEFCoordinate x y z) m = 
  let a = semiMajorAxis m
      b = semiMinorAxis m
      e2 = fstEccentricity m
      e'2 = sndEccentricity m
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
      phi = atan ((z + (e'2 * z0)) / r)
      lambda = atan2 y x
  in GeodaticCoordinate m phi lambda h
     
ms = GeodaticCoordinate WGS84 (57.6 *~ degree) (7.42 *~ degree) (35 *~ meter)
mse = toEcef ms
msa1 = toGeodatic ms ANS
msa2 = toGeodatic mse ANS
msb = toGeodatic mse WGS84


