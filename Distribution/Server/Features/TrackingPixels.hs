{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

-- | Implements a system to allow users to upvote packages.
--
module Distribution.Server.Features.TrackingPixels
  ( TrackingPixelsFeature(..)
  , TrackingPixel(..)
  , initTrackingPixelsFeature
  ) where

import Data.Set (Set)

import Distribution.Server.Features.TrackingPixels.State

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users

import Distribution.Package

-- | Define the prototype for this feature
data TrackingPixelsFeature = TrackingPixelsFeature {
    trackingPixelsFeatureInterface :: HackageFeature,
    trackingPixelsResource         :: Resource,
    userTrackingPixelsResource     :: Resource,

    trackingPixelAdded             :: Hook (PackageName, TrackingPixel) (),
    trackingPixelRemoved           :: Hook (PackageName, TrackingPixel) (),

    -- | Returns all 'TrackingPixel's associated with a 'Package'.
    getPackageTrackingPixels       :: forall m. MonadIO m => PackageName -> m (Set TrackingPixel),

    -- | Adds a new 'TrackingPixel' to a 'Package'. Returns True in case it was added. False in case
    -- it's already existing.
    addPackageTrackingPixel        :: forall m. MonadIO m => PackageName -> TrackingPixel -> m Bool,

    -- | Remove a 'TrackingPixel' from a 'Package'.
    removePackageTrackingPixel     :: forall m. MonadIO m => PackageName -> TrackingPixel -> m ()
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature TrackingPixelsFeature where
  getFeatureInterface = trackingPixelsFeatureInterface

-- | Called from Features.hs to initialize this feature
initTrackingPixelsFeature :: ServerEnv
                          -> IO ( CoreFeature
                            -> UserFeature
                            -> UploadFeature
                            -> IO TrackingPixelsFeature)
initTrackingPixelsFeature env@ServerEnv{serverStateDir} = do
  dbTrackingPixelsState <- trackingPixelsStateComponent serverStateDir
  trackingPixelAdded    <- newHook
  trackingPixelRemoved  <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} uploadf -> do
    let feature = trackingPixelsFeature env
                  dbTrackingPixelsState
                  coref userf uploadf trackingPixelAdded trackingPixelRemoved

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
                      -> UploadFeature                        -- For accessing package maintainers and trustees
                      -> Hook (PackageName, TrackingPixel) () -- Signals addition of a new TrackingPixel
                      -> Hook (PackageName, TrackingPixel) () -- Signals removeal of a TrackingPixel
                      -> TrackingPixelsFeature

trackingPixelsFeature  ServerEnv{..}
              trackingPixelsState
              CoreFeature { coreResource = CoreResource{..} }
              UserFeature{..}
              UploadFeature{..}
              trackingPixelAdded
              trackingPixelRemoved
  = TrackingPixelsFeature {..}
  where
    trackingPixelsFeatureInterface  = (emptyHackageFeature "trackingPixels") {
        featureDesc      = "Allow users to attach tracking pixels to their packages",
        featureResources = [trackingPixelsResource, userTrackingPixelsResource]
      , featureState     = [abstractAcidStateComponent trackingPixelsState]
      }

    trackingPixelsResource :: Resource
    trackingPixelsResource = (resourceAt "/packages/:package/tracking-pixels.:format") {
        resourceDesc   = [ (GET, "Returns the installed tracking pixels for a package")
                         , (PUT, "Adds a tracking pixel to this package")
                         , (DELETE, "Remove a tracking pixel from this package")
                         ]
      , resourceGet    = [("json", servePackageTrackingPixelsGet)]
      , resourcePost   = [("",     servePackageTrackingPixelsPut)]
      , resourceDelete = [("",     servePackageTrackingPixelsDelete)]
    }

    userTrackingPixelsResource :: Resource
    userTrackingPixelsResource = resourceAt "/user/:username/tracking-pixels.:format"

    servePackageTrackingPixelsGet :: DynamicPath -> ServerPartE Response
    servePackageTrackingPixelsGet dpath = do
        guardAuthorised_ [AnyKnownUser]
        pkgname <- packageInPath dpath
        guardValidPackageName pkgname
        error "TODO"

    servePackageTrackingPixelsPut :: DynamicPath -> ServerPartE Response
    servePackageTrackingPixelsPut dpath = do
        pkgname <- packageInPath dpath
        guardValidPackageName pkgname
        guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]
        error "TODO"

    servePackageTrackingPixelsDelete :: DynamicPath -> ServerPartE Response
    servePackageTrackingPixelsDelete dpath = do
        pkgname <- packageInPath dpath
        guardValidPackageName pkgname
        guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]
        error "TODO" 

    getPackageTrackingPixels :: MonadIO m => PackageName -> m (Set TrackingPixel)
    getPackageTrackingPixels name = 
        queryState trackingPixelsState (TrackingPixelsForPackage name)

    addPackageTrackingPixel :: MonadIO m => PackageName -> TrackingPixel -> m Bool
    addPackageTrackingPixel name pixel = do
        added <- updateState trackingPixelsState (AddPackageTrackingPixel name pixel)
        when added $ runHook_ trackingPixelAdded (name, pixel)
        pure added

    removePackageTrackingPixel :: MonadIO m => PackageName -> TrackingPixel -> m ()
    removePackageTrackingPixel name pixel = do
        updateState trackingPixelsState (RemovePackageTrackingPixel name pixel)
        runHook_ trackingPixelRemoved (name, pixel)
