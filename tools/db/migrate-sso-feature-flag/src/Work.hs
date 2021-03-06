{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Work where

import Brig.Types hiding (Client)
import Cassandra
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Data.Misc
import Galley.Data.Instances ()
import Galley.Types.Teams.SSO
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async (pooledMapConcurrentlyN)

deriving instance Cql Name

runCommand :: Logger -> ClientState -> ClientState -> IO ()
runCommand l spar galley = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient spar) getSsoTeams)
      .| C.mapM
        ( \(i, tids) -> do
            Log.info l (Log.field "number of idps processed: " (show (i * pageSize)))
            pure (runIdentity <$> tids)
        )
      .| C.mapM_ (\tids -> runClient galley (writeSsoFlags tids))

pageSize :: Int32
pageSize = 1000

getSsoTeams :: ConduitM () [Identity TeamId] Client ()
getSsoTeams = paginateC cql (paramsP Quorum () pageSize) x5
  where
    cql :: PrepQuery R () (Identity TeamId)
    cql = "select team from idp"

writeSsoFlags :: [TeamId] -> Client ()
writeSsoFlags = mapM_ (`setSSOTeamConfig` (SSOTeamConfig SSOEnabled))
  where
    setSSOTeamConfig :: MonadClient m => TeamId -> SSOTeamConfig -> m ()
    setSSOTeamConfig tid SSOTeamConfig {ssoTeamConfigStatus} = do
      retry x5 $ write updateSSOTeamConfig (params Quorum (ssoTeamConfigStatus, tid))
    updateSSOTeamConfig :: PrepQuery W (SSOStatus, TeamId) ()
    updateSSOTeamConfig = "update team_features set sso_status = ? where team_id = ?"
