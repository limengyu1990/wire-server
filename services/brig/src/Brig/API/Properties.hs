module Brig.API.Properties
  ( PropertiesDataError (..),
    setProperty,
    deleteProperty,
    clearProperties,
    Data.lookupProperty,
    Data.lookupPropertyKeys,
    Data.lookupPropertyKeysAndValues,
  )
where

import Brig.App
import Brig.Data.Properties (PropertiesDataError)
import qualified Brig.Data.Properties as Data
import qualified Brig.IO.Intra as Intra
import Brig.Types
import Brig.User.Event
import Control.Error
import Data.Id
import Imports

setProperty :: () -> ConnId -> PropertyKey -> PropertyValue -> ExceptT PropertiesDataError AppIO ()
setProperty (undefined -> u) c k v = do
  Data.insertProperty u k v
  lift $ Intra.onPropertyEvent u c (undefined PropertySet u k v)

deleteProperty :: () -> ConnId -> PropertyKey -> AppIO ()
deleteProperty (undefined -> u) c k = do
  Data.deleteProperty u k
  Intra.onPropertyEvent u c (undefined PropertyDeleted u k)

clearProperties :: () -> ConnId -> AppIO ()
clearProperties (undefined -> u) c = do
  Data.clearProperties u
  Intra.onPropertyEvent u c (undefined PropertiesCleared u)
