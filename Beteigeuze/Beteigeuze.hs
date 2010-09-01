{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, 
             TypeOperators, FlexibleInstances, MultiParamTypeClasses, 
             FlexibleContexts, FunctionalDependencies #-}

module Beteigeuze where

import Data.List (find)
import Data.Generics (Data)
import Data.Typeable (Typeable)
import Control.Monad.State
import Control.Monad.Reader
import Happstack.Data
import Happstack.State


-- ***** Players *****
type PlayerId = Integer           

data PlayerT = Player { 
  playerTId :: PlayerId,
  playerTName :: String
  } deriving (Show, Eq, Typeable, Data)
instance Version PlayerT
$(deriveSerialize ''PlayerT)

class Player p where
  
instance Player PlayerT

type SystemId = Integer

type Position = (Integer, Integer, Integer)

newtype Players = Players [PlayerT]
  deriving (Show, Eq, Data, Typeable)

instance Version Players
$(deriveSerialize ''Players)

instance Component Players where
  type Dependencies Players = End
  initialValue = Players []

playerById :: PlayerId -> Query Players (Maybe PlayerT)
playerById id = do
  (Players ps) <- ask
  return $ find (\p -> playerTId p == id) ps

$(mkMethods ''Players ['playerById])
 
-- ***** Systems *****
data SystemT = System {
  systemTId :: SystemId,
  systemTName :: String,
  systemTCoordinate :: Position
  } deriving (Show, Eq, Typeable, Data)
instance Version SystemT
$(deriveSerialize ''SystemT)

class System s where
  systemName :: s -> String
  systemCoordinate :: s -> Position
  
instance System SystemT where  
  systemName = systemTName
  systemCoordinate = systemTCoordinate

newtype Systems = Systems [SystemT]
  deriving (Show, Eq, Data, Typeable)

instance Version Systems
$(deriveSerialize ''Systems)

instance Component Systems where
  type Dependencies Systems = End
  initialValue = Systems []

type Capacity = Float

systemById :: SystemId -> Query Systems (Maybe SystemT)
systemById id = do
  (Systems ps) <- ask
  return $ find (\p -> systemTId p == id) ps

$(mkMethods ''Systems ['systemById])

-- ***** Planets *****
data AreaT =
  IridiumArea Capacity |
  FoodArea Capacity |
  MilitaryArea Capacity |
  LivingArea Capacity
  deriving (Show, Eq, Typeable, Data)
instance Version AreaT 
$(deriveSerialize ''AreaT)
  
class Area a where
  
instance Area AreaT where

type PlanetId = Integer

data PlanetT = Planet {
  planetTId :: PlanetId,
  planetTName :: String,
  planetTOwner :: Maybe PlayerId,
  planetTSystemId :: SystemId,
  planetTAreas :: [AreaT]
  } deriving (Show, Eq, Typeable, Data)
instance Version PlanetT  
$(deriveSerialize ''PlanetT)

class (Area a) => Planet p a | p -> a where
  planetName :: p -> String
  planetOwner :: p -> Maybe PlayerById 
  planetSystem :: p -> SystemById
  planetAreas :: (Area a) => p -> [a]
  
instance Planet PlanetT AreaT where
  planetName = planetTName 
  planetOwner p = case (planetTOwner p) of 
    Nothing -> Nothing
    Just ownerId -> Just $ PlayerById ownerId
  planetSystem p = SystemById $ planetTSystemId p
  planetAreas = planetTAreas 
 
newtype Planets = Planets [PlanetT]
  deriving (Show, Eq, Data, Typeable)

instance Version Planets
$(deriveSerialize ''Planets)

instance Component Planets where
  type Dependencies Planets = Systems :+: Players :+: End
  initialValue = Planets []
  
planetById :: PlanetId -> Query Planets (Maybe PlanetT)
planetById id = do
  (Planets ps) <- ask
  return $ find (\p -> planetTId p == id) ps

$(mkMethods ''Planets ['planetById])

-- ***** Fleets *****                  
type FleetId = Integer

-- ***** Ships *****                  
type ShipId = Integer

data ShipT = Ship {
  shipId :: ShipId,
  shipOwner :: PlayerId,
  shipPos :: Either SystemId FleetId
  } deriving (Show, Eq, Typeable, Data)
instance Version ShipT
$(deriveSerialize ''ShipT)
