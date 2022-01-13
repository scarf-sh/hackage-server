{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.TrackingPixels.State 
    ( TrackingPixel(..)
    , TrackingPixelsState(..)
    , initialTrackingPixelsState

    -- * State queries and updates
    , TrackingPixelsForPackage(..)
    , AddPackageTrackingPixel(..)
    , RemovePackageTrackingPixel(..)
    , GetTrackingPixelsState(..)
    , ReplaceTrackingPixelsState(..)
    ) where

import Distribution.Package (PackageName)

import Distribution.Server.Framework.MemSize (MemSize)
import Distribution.Server.Users.State ()

import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import qualified Data.Set as Set

import Control.DeepSeq (NFData)
import qualified Control.Monad.State as State
import Control.Monad.Reader.Class (ask, asks)

newtype TrackingPixel = TrackingPixel
    {
        trackingPixelUrl :: Text
    }
    deriving (Show, Eq, Ord, NFData, Typeable, MemSize)

newtype TrackingPixelsState = TrackingPixelsState
    {
        trackingPixels :: Map PackageName (Set TrackingPixel)
    }
  deriving (Show, Eq, NFData, Typeable, MemSize)

-- SafeCopy instances
$(deriveSafeCopy 0 'base ''TrackingPixel)
$(deriveSafeCopy 0 'base ''TrackingPixelsState)

--

initialTrackingPixelsState :: TrackingPixelsState
initialTrackingPixelsState = TrackingPixelsState
    {
        trackingPixels = Map.empty
    }

trackingPixelsForPackage :: PackageName -> Query TrackingPixelsState (Set TrackingPixel)
trackingPixelsForPackage name = asks $ Map.findWithDefault Set.empty name . trackingPixels

-- | Adds a 'TrackingPixel' to a 'Package'. Returns 'True' if the pixel was inserted, and 'False' if
-- the 'TrackingPixel' was already present.
addPackageTrackingPixel :: PackageName -> TrackingPixel -> Update TrackingPixelsState Bool
addPackageTrackingPixel name trackingPixel = do
    state <- State.get
    let (successfullyInserted, pixels) = Map.alterF insertTrackingPixel name (trackingPixels state)
    State.put (state { trackingPixels = pixels })
    pure successfullyInserted
    where
        insertTrackingPixel :: Maybe (Set TrackingPixel) -> (Bool, Maybe (Set TrackingPixel))
        insertTrackingPixel Nothing = 
            (True, Just (Set.singleton trackingPixel))
        insertTrackingPixel existingPixels@(Just pixels)
            | trackingPixel `Set.member` pixels = 
                (False, existingPixels)
            | otherwise = 
                (True, Just (Set.insert trackingPixel pixels))

-- | Removes a 'TrackingPixel' from a 'Package'.
removePackageTrackingPixel :: PackageName -> TrackingPixel -> Update TrackingPixelsState ()
removePackageTrackingPixel name trackingPixel = do
    state <- State.get
    let pixels = Map.alter removeTrackingPixel name (trackingPixels state)
    State.put (state { trackingPixels = pixels })
    pure ()
    where
        removeTrackingPixel Nothing =
            Nothing
        removeTrackingPixel (Just pixels) =
            let pixels' = trackingPixel `Set.delete` pixels in
                if Set.null pixels' then Nothing else Just pixels'

-- get and replace the entire state, for backups

getTrackingPixelsState :: Query TrackingPixelsState TrackingPixelsState
getTrackingPixelsState = ask

replaceTrackingPixelsState :: TrackingPixelsState -> Update TrackingPixelsState ()
replaceTrackingPixelsState = State.put

makeAcidic
  ''TrackingPixelsState
  [ 'getTrackingPixelsState
  , 'trackingPixelsForPackage
  , 'replaceTrackingPixelsState
  , 'addPackageTrackingPixel
  , 'removePackageTrackingPixel
  ]
