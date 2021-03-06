{-# LANGUAGE RecordWildCards #-}

module Galley.Data.CustomBackend
  ( getCustomBackend,
    setCustomBackend,
    deleteCustomBackend,
  )
where

import Cassandra
import Data.Domain (Domain)
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Types
import Imports

getCustomBackend :: MonadClient m => Domain -> m (Maybe CustomBackend)
getCustomBackend domain = fmap toCustomBackend <$> do
  retry x1 $ query1 Cql.selectCustomBackend (params Quorum (Identity domain))
  where
    toCustomBackend (backendConfigJsonUrl, backendWebappWelcomeUrl) =
      CustomBackend {..}

setCustomBackend :: MonadClient m => Domain -> CustomBackend -> m ()
setCustomBackend domain CustomBackend {..} = do
  retry x5 $ write Cql.updateCustomBackend (params Quorum (backendConfigJsonUrl, backendWebappWelcomeUrl, domain))

deleteCustomBackend :: MonadClient m => Domain -> m ()
deleteCustomBackend domain = do
  retry x5 $ write Cql.deleteCustomBackend (params Quorum (Identity domain))
