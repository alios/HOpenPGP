
\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module MilSymb where

import Data.Text (Text)
\end{code}

\subsection{Composition of tactical symbols} 
A fully displayed tactical symbol is composed of a \emph{frame}, \emph{fill}, and \empfh{icon} 
and may include text and/or graphic modifiers that provide additional information (see figure 1). 
The frame attributes (i.e., standard identity, battle dimension, and FRAME COLOR FILL
status) determine the type of frame for a given symbol. Fill color is a redundant indication of
ICON the symbol’s standard identity.


  
  
class (HasColor d, HasCharAbbrev d) => Descriptor d 

class HasColor c where
  color :: c -> Color

class HasCharAbbrev a where
  charAbbrev :: a -> Char

class ModifierField f where
  fieldId :: f -> Text
  fieldTitle :: f -> Text
  fieldDescription :: f -> Text
  fieldApplications :: f -> [FieldApplication]
  
\begin{code}

class BattleDimension d

data UnknownDim = UnkownDim
data SpaceDim = SpaceDim
data AirDim = AirDim
data UnitsDim = UnitsDim
data EquipmentDim = EquipmentDim
data InstallationsDim = InstallationsDim
data SeaSurfaceDim = SeaSurfaceDim
data SubsurfaceDim = SubsurfaceDim
data SOFDim = SOFDim
                     
data PendingIdentity
data UnknownIdentity
data FriendIdentity
data NeutralIdentity
data HostileIdentity
data AssumedFriendIdentity
data SuspectIdentity
                                                 
data ExercisePending
data ExerciseUnknown
data ExerciseFriend
data ExerciseNeutral
data ExerciseAssumedFriend
data Joker
data Faker

class StandardIdentity i
instance StandardIdentity PendingIdentity
instance StandardIdentity UnknownIdentity
instance StandardIdentity FriendIdentity
instance StandardIdentity NeutralIdentity
instance StandardIdentity HostileIdentity
instance StandardIdentity AssumedFriendIdentity
instance StandardIdentity SuspectIdentity

class ExerciseAmplifyingDescriptor d
instance ExerciseAmplifyingDescriptor ExercisePending
instance ExerciseAmplifyingDescriptor ExerciseUnknown
instance ExerciseAmplifyingDescriptor ExerciseFriend
instance ExerciseAmplifyingDescriptor ExerciseNeutral
instance ExerciseAmplifyingDescriptor ExerciseAssumedFriend
instance ExerciseAmplifyingDescriptor Joker
instance ExerciseAmplifyingDescriptor Faker

class Descriptor d
instance (StandardIdentity d) => Descriptor d

data Symbol dim desc where  
  Symbol :: (BattleDimension dim, Descriptor desc) => dim -> desc -> [FieldApplication] -> Symbol dim desc

class HasColor t where
  color :: t -> Color
  
instance HasColor PendingIdentity where color _ = Yellow
instance HasColor UnknownIdentity where color _ = Yellow  
instance HasColor FriendIdentity where color _  = Cyan
instance HasColor NeutralIdentity where color _ = Green
instance HasColor HostileIdentity where color _ = Red
instance HasColor AssumedFriendIdentity where color _ = Cyan
instance HasColor SuspectIdentity where color _ = Red

instance HasColor ExercisePending where color _  = Yellow
instance HasColor ExerciseUnknown where color _ = Yellow  
instance HasColor ExerciseFriend where color _ = Cyan
instance HasColor ExerciseNeutral where color _ = Green
instance HasColor ExerciseAssumedFriend where color _ = Cyan
instance HasColor Joker where color _ = Red
instance HasColor Faker where color _ = Red
   
instance (HasColor desc) => HasColor (Symbol dim desc) where
  color (Symbol _ desc _) = color desc


data FieldApplication = GraphicModifier
                      | TextModifier Integer
                      | GraphicTextModifier Integer

data Color = Yellow
           | Cyan
           | Green
           | Red
           deriving (Show, Read, Eq, Enum)
\end{code}  

instance (BattleDimension d) => BattleDimensionClass d where
  aboveSurface d = d `elem` [SpaceDim, AirDim]
  ground d = d `elem` [UnitsDim, EquipmentDim, InstallationsDim] 
  surface d = or [ d == SeaSurfaceDim , ground d ]

 
  
instance HasCharAbbrev BattleDimension where
  charAbbrev UnknownDim = 'Z'
  charAbbrev SpaceDim = 'P'
  charAbbrev AirDim = 'A'
  charAbbrev UnitsDim = 'G'
  charAbbrev EquipmentDim = 'G'
  charAbbrev InstallationsDim = 'G'
  charAbbrev SeaSurfaceDim = 'S'
  charAbbrev SubsurfaceDim = 'U'
  charAbbrev SOFDim = 'F'

instance Descriptor StdIdentity
instance Descriptor ExerciseAmplifyingDescriptor


instance HasCharAbbrev StdIdentity where
  charAbbrev PendingIdentity = 'P'
  charAbbrev UnknownIdentity = 'U'  
  charAbbrev FriendIdentity = 'F'
  charAbbrev NeutralIdentity = 'N'
  charAbbrev HostileIdentity = 'H'
  charAbbrev AssumedFriendIdentity = 'A'
  charAbbrev SuspectIdentity = 'S'
                                        
instance HasCharAbbrev ExerciseAmplifyingDescriptor where
  charAbbrev ExercisePending = 'G'
  charAbbrev ExerciseUnknown = 'W'
  charAbbrev ExerciseFriend = 'D'
  charAbbrev ExerciseNeutral = 'L'
  charAbbrev ExerciseAssumedFriend = 'M'
  charAbbrev Joker = 'J'
  charAbbrev Faker = 'K'




symbolDim = (1000, 710)
