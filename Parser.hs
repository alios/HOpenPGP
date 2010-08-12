{-# LANGUAGE DeriveDataTypeable #-}
module Parser () where

import Data.Generics(Data)
import Data.Typeable    
import Data.Binary
import Data.Binary.Get
import Control.Applicative

import Nmea0183 

getTransmittableChar :: Get Word8
getTransmittableChar = 
    do c <- getWord8
       if (isTransmittableChar c) then 
           return c else
           fail $ show c ++ " is not a transmittable char."
                  


data AddressField = 
    AAF { 
      talkerId :: String,
      format :: String 
    } | QAF
    {
      
    } 
    deriving (Read, Show, Eq, Data, Typeable)
                  
