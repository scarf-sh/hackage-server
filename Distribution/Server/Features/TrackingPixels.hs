{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

-- | Implements a system to allow users to upvote packages.
--
module Distribution.Server.Features.TrackingPixels
  ( TrackingPixelsFeature(..)
  , initTrackingPixelsFeature
  ) where

import Distribution.Server.Features.TrackingPixels.State

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Package

-- | Define the prototype for this feature
data TrackingPixelsFeature = TrackingPixelsFeature {
    trackingPixelsFeatureInterface :: HackageFeature,
    trackingPixelAdded             :: Hook (PackageName, TrackingPixel) (),
    trackingPixelRemoved           :: Hook (PackageName, TrackingPixel) ()
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature TrackingPixelsFeature where
  getFeatureInterface = trackingPixelsFeatureInterface

-- | Called from Features.hs to initialize this feature
initTrackingPixelsFeature :: ServerEnv
                          -> IO ( CoreFeature
                            -> UserFeature
                            -> IO TrackingPixelsFeature)
initTrackingPixelsFeature env@ServerEnv{serverStateDir} = do
  dbTrackingPixelsState <- trackingPixelsStateComponent serverStateDir
  trackingPixelAdded    <- newHook
  trackingPixelRemoved  <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = trackingPixelsFeature env
                  dbTrackingPixelsState
                  coref userf trackingPixelAdded trackingPixelRemoved

    return feature

-- | Define the backing store (i.e. database component)
trackingPixelsStateComponent :: FilePath -> IO (StateComponent AcidState TrackingPixelsState)
trackingPixelsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "TrackingPixels") initialTrackingPixelsState
  return StateComponent {
      stateDesc    = "Backing store for TrackingPixels feature"
    , stateHandle  = st
    , getState     = query st GetTrackingPixelsState
    , putState     = update st . ReplaceTrackingPixelsState
    , resetState   = trackingPixelsStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return initialTrackingPixelsState
                       }
   }


-- | Default constructor for building this feature.
trackingPixelsFeature :: ServerEnv
                      -> StateComponent AcidState TrackingPixelsState
                      -> CoreFeature                          -- To get site package list
                      -> UserFeature                          -- To authenticate users
                      -> Hook (PackageName, TrackingPixel) () -- Signals addition of a new TrackingPixel
                      -> Hook (PackageName, TrackingPixel) () -- Signals removeal of a TrackingPixel
                      -> TrackingPixelsFeature

trackingPixelsFeature  ServerEnv{..}
              trackingPixelsState
              CoreFeature { coreResource = CoreResource{..} }
              UserFeature{..}
              trackingPixelAdded
              trackingPixelRemoved
  = TrackingPixelsFeature {..}
  where
    trackingPixelsFeatureInterface  = (emptyHackageFeature "trackingPixels") {
        featureDesc      = "Allow users to attache tracking pixels to their packages",
        featureResources = []
      , featureState     = [abstractAcidStateComponent trackingPixelsState]
      }
