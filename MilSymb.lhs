
\begin{code}
{-# LANGUAGE GADTs  #-}

module MilSymb where

import Data.Text (Text)
\end{code}

\subsection{Composition of tactical symbols} 
A fully displayed tactical symbol is composed of a \emph{frame}, \emph{fill}, and \empfh{icon} 
and may include text and/or graphic modifiers that provide additional information (see figure 1). 
The frame attributes (i.e., standard identity, battle dimension, and FRAME COLOR FILL
status) determine the type of frame for a given symbol. Fill color is a redundant indication of
ICON the symbol’s standard identity.


\begin{code}
  
class BattleDimensionClass d where
  aboveSurface :: d -> Bool
  ground :: d -> Bool
  surface :: d -> Bool
  
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
  
data Symbol dim desc where
  Symbol :: (BattleDimensionClass dim) => dim -> desc -> Symbol dim desc

data BattleDimension = UnknownDim
                     | SpaceDim
                     | AirDim
                     | UnitsDim
                     | EquipmentDim
                     | InstallationsDim
                     | SeaSurfaceDim
                     | SubsurfaceDim
                     | SOFDim
                     deriving (Show, Read, Eq, Enum)

data StdIdentity = PendingIdentity
                 | UnknownIdentity
                 | FriendIdentity
                 | NeutralIdentity
                 | HostileIdentity
                 | AssumedFriendIdentity
                 | SuspectIdentity
                 deriving (Show, Read, Eq, Enum)
                                                 
data ExerciseAmplifyingDescriptor = ExercisePending
                                  | ExerciseUnknown
                                  | ExerciseFriend
                                  | ExerciseNeutral
                                  | ExerciseAssumedFriend
                                  | Joker
                                  | Faker
                                  deriving (Show, Read, Eq, Enum)


data FieldApplication = GraphicModifier
                      | TextModifier Integer
                      | GraphicTextModifier Integer

data Color = Yellow
           | Cyan
           | Green
           | Red
           deriving (Show, Read, Eq, Enum)

instance BattleDimensionClass BattleDimension where
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

instance HasColor StdIdentity where
  color PendingIdentity = Yellow
  color UnknownIdentity = Yellow  
  color FriendIdentity = Cyan
  color NeutralIdentity = Green
  color HostileIdentity = Red
  color AssumedFriendIdentity = Cyan
  color SuspectIdentity = Red
   
instance HasColor ExerciseAmplifyingDescriptor where
  color ExercisePending = Yellow
  color ExerciseUnknown = Yellow  
  color ExerciseFriend = Cyan
  color ExerciseNeutral = Green
  color ExerciseAssumedFriend = Cyan
  color Joker = Red
  color Faker = Red

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

\end{code}  
