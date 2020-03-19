{-# LANGUAGE RecordWildCards #-}

-- TODO: Move to Brig.User.RPC or similar.
module Brig.IO.Intra
  ( -- * Pushing & Journaling Events
    onUserEvent,
    onConnectionEvent,
    onPropertyEvent,
    onClientEvent,

    -- * Conversations
    createSelfConv,
    createConnectConv,
    acceptConnectConv,
    blockConv,
    unblockConv,
    getConv,

    -- * Clients
    Brig.IO.Intra.newClient,
    rmClient,

    -- * Account Deletion
    rmUser,

    -- * Teams
    addTeamMember,
    createTeam,
    getTeamMember,
    getTeamMembers,
    getTeam,
    getTeamConv,
    getTeamName,
    getTeamId,
    getTeamContacts,
    getTeamOwners,
    getTeamOwnersWithEmail,
    getTeamLegalHoldStatus,
    changeTeamStatus,
    getTruncatedTeamSize,
  )
where

import Bilge hiding (head, options, requestId)
import Bilge.RPC
import Bilge.Retry
import Brig.API.Types
import Brig.App
import Brig.Data.Connection (lookupContactList)
import Brig.Data.User (lookupUsers)
import qualified Brig.IO.Journal as Journal
import Brig.RPC
import Brig.Types
import Brig.Types.Intra
import Brig.Types.Team.LegalHold (LegalHoldTeamConfig)
import Brig.User.Event
import qualified Brig.User.Event.Log as Log
import qualified Brig.User.Search.Index as Search
import Control.Lens ((.~), (?~), (^.), view)
import Control.Lens.Prism (_Just)
import Control.Retry
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
import qualified Data.Currency as Currency
import qualified Data.HashMap.Strict as M
import Data.Id
import Data.Json.Util ((#), UTCTimeMillis)
import Data.Json.Util ()
import Data.List.Split (chunksOf)
import Data.List1 (List1, list1, singleton)
import qualified Data.Map as Map
import Data.Range
import qualified Data.Set as Set
import Galley.Types (Connect (..), Conversation)
import qualified Galley.Types.Teams as Team
import qualified Galley.Types.Teams.Intra as Team
import Gundeck.Types.Push.V2
import qualified Gundeck.Types.Push.V2 as Push
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import System.Logger.Class as Log hiding ((.=), name)

-----------------------------------------------------------------------------
-- Event Handlers

onUserEvent :: E -> UserId -> Maybe ConnId -> UserEvent -> AppIO ()
onUserEvent E orig conn e =
  updateSearchIndex orig e
    *> dispatchNotifications E orig conn e
    *> journalEvent orig e

onConnectionEvent ::
  E ->
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID, if any.
  Maybe ConnId ->
  -- | The event.
  ConnectionEvent ->
  AppIO ()
onConnectionEvent E orig conn evt = do
  let from = ucFrom (ucConn evt)
  notify
    E
    (singleton $ ConnectionEvent evt)
    orig
    Push.RouteAny
    conn
    (return $ list1 from [])

onPropertyEvent ::
  E ->
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  ConnId ->
  PropertyEvent ->
  AppIO ()
onPropertyEvent E orig conn e =
  notify
    E
    (singleton $ PropertyEvent e)
    orig
    Push.RouteDirect
    (Just conn)
    (return $ list1 orig [])

onClientEvent ::
  E ->
  -- | Originator of the event.
  UserId ->
  -- | Client connection ID.
  Maybe ConnId ->
  -- | The event.
  ClientEvent ->
  AppIO ()
onClientEvent E orig conn e = do
  let events = singleton (ClientEvent e)
  let rcps = list1 orig []
  -- Synchronous push for better delivery guarantees of these
  -- events and to make sure new clients have a first notification
  -- in the stream.
  push E events rcps orig Push.RouteAny conn

updateSearchIndex :: UserId -> UserEvent -> AppIO ()
updateSearchIndex orig e = case e of
  -- no-ops
  UserCreated {} -> return ()
  UserIdentityUpdated {} -> return ()
  UserIdentityRemoved {} -> return ()
  UserLegalHoldDisabled {} -> return ()
  UserLegalHoldEnabled {} -> return ()
  LegalHoldClientRequested {} -> return ()
  UserSuspended {} -> Search.reindex orig
  UserResumed {} -> Search.reindex orig
  UserActivated {} -> Search.reindex orig
  UserDeleted {} -> Search.reindex orig
  UserUpdated {..} -> do
    let interesting =
          or
            [ isJust eupName,
              isJust eupAccentId,
              isJust eupHandle,
              isJust eupSearchable
            ]
    when (interesting) $ Search.reindex orig

journalEvent :: UserId -> UserEvent -> AppIO ()
journalEvent orig e = case e of
  UserActivated acc -> Journal.userActivate (accountUser acc)
  UserUpdated {eupName = Just name} -> Journal.userUpdate orig Nothing Nothing (Just name)
  UserUpdated {eupLocale = Just loc} -> Journal.userUpdate orig Nothing (Just loc) Nothing
  UserIdentityUpdated _ (Just em) _ -> Journal.userUpdate orig (Just em) Nothing Nothing
  UserIdentityRemoved _ (Just em) _ -> Journal.userEmailRemove orig em
  UserDeleted {} -> Journal.userDelete orig
  _ -> return ()

-------------------------------------------------------------------------------
-- Low-Level Event Notification

-- | Notify the origin user's contact list (first-level contacts),
-- as well as his other clients about a change to his user account
-- or profile.
dispatchNotifications :: E -> UserId -> Maybe ConnId -> UserEvent -> AppIO ()
dispatchNotifications E orig conn e = case e of
  UserCreated {} -> return ()
  UserSuspended {} -> return ()
  UserResumed {} -> return ()
  LegalHoldClientRequested {} -> notifyContacts E event orig Push.RouteAny conn
  UserLegalHoldDisabled {} -> notifyContacts E event orig Push.RouteAny conn
  UserLegalHoldEnabled {} -> notifyContacts E event orig Push.RouteAny conn
  UserUpdated {..}
    -- bug hiding?
    | isJust eupLocale -> notifySelf E event orig Push.RouteDirect conn
    | otherwise -> notifyContacts E event orig Push.RouteDirect conn
  UserActivated {} -> notifySelf E event orig Push.RouteAny conn
  UserIdentityUpdated {} -> notifySelf E event orig Push.RouteDirect conn
  UserIdentityRemoved {} -> notifySelf E event orig Push.RouteDirect conn
  UserDeleted {} -> do
    -- n.b. Synchronously fetch the contact list on the current thread.
    -- If done asynchronously, the connections may already have been deleted.
    recipients <- list1 orig <$> lookupContactList orig
    notify E event orig Push.RouteDirect conn (pure recipients)
  where
    event = singleton $ UserEvent e

-- | Push events to other users.
push ::
  E ->
  -- | The events to push.
  List1 Event ->
  -- | The users to push to.
  List1 UserId ->
  -- | The originator of the events.
  UserId ->
  -- | The push routing strategy.
  Push.Route ->
  -- | The originating device connection.
  Maybe ConnId ->
  AppIO ()
push E (toList -> events) usrs orig route conn =
  case mapMaybe toPushData events of
    [] -> pure ()
    x : xs -> rawPush E (list1 x xs) usrs orig route conn
  where
    toPushData :: Event -> Maybe (Builder, (Object, Maybe ApsData))
    toPushData e = case toPushFormat e of
      Just o -> Just (Log.bytes e, (o, toApsData e))
      Nothing -> Nothing

-- | Push encoded events to other users. Useful if you want to push
-- something that's not defined in Brig.
rawPush ::
  E ->
  -- | The events to push.
  List1 (Builder, (Object, Maybe ApsData)) ->
  -- | The users to push to.
  List1 UserId ->
  -- | The originator of the events.
  UserId ->
  -- | The push routing strategy.
  Push.Route ->
  -- | The originating device connection.
  Maybe ConnId ->
  AppIO ()
-- TODO: if we decide to have service whitelist events in Brig instead of
-- Galley, let's merge 'push' and 'rawPush' back. See Note [whitelist events].
rawPush E (toList -> events) usrs orig route conn = do
  for_ events $ \e -> debug $ remote "gundeck" . msg (fst e)
  g <- view gundeck
  forM_ recipients $ \rcps ->
    void . recovering x3 rpcHandlers $ const $
      rpc'
        "gundeck"
        g
        ( method POST
            . path "/i/push/v2"
            . zUser orig
            . json (map (mkPush rcps . snd) events)
            . expect2xx
        )
  where
    recipients :: [Range 1 1024 (Set.Set Recipient)]
    recipients =
      map (unsafeRange . Set.fromList)
        $ chunksOf 512
        $ map (`recipient` route)
        $ toList usrs
    mkPush :: Range 1 1024 (Set.Set Recipient) -> (Object, Maybe ApsData) -> Push
    mkPush rcps (o, aps) =
      newPush
        orig
        rcps
        (singletonPayload o)
        & pushOriginConnection .~ conn
        & pushNativeAps .~ aps

-- | (Asynchronously) notifies other users of events.
notify ::
  E ->
  List1 Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  -- | Users to notify.
  IO (List1 UserId) ->
  AppIO ()
notify E events orig route conn recipients = forkAppIO (Just orig) $ do
  rs <- liftIO recipients
  push E events rs orig route conn

notifySelf ::
  E ->
  List1 Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  AppIO ()
notifySelf E events orig route conn =
  notify E events orig route conn (pure (singleton orig))

notifyContacts ::
  E ->
  List1 Event ->
  -- | Origin user.
  UserId ->
  -- | Push routing strategy.
  Push.Route ->
  -- | Origin device connection, if any.
  Maybe ConnId ->
  AppIO ()
notifyContacts E events orig route conn = do
  env <- ask
  notify E events orig route conn
    $ runAppT env
    $ list1 orig <$> liftA2 (++) contacts teamContacts
  where
    contacts :: AppIO [UserId]
    contacts = lookupContactList orig
    teamContacts :: AppIO [UserId]
    teamContacts = getUids <$> getTeamContacts orig
    getUids :: Maybe Team.TeamMemberList -> [UserId]
    getUids = fmap (view Team.userId) . view (_Just . Team.teamMembers)

-- Event Serialisation:

toPushFormat :: Event -> Maybe Object
toPushFormat (UserEvent (UserCreated (UserAccount u _))) =
  Just $
    M.fromList
      [ "type" .= ("user.new" :: Text),
        "user" .= SelfProfile (u {userIdentity = Nothing})
      ]
toPushFormat (UserEvent (UserActivated (UserAccount u _))) =
  Just $
    M.fromList
      [ "type" .= ("user.activate" :: Text),
        "user" .= SelfProfile u
      ]
toPushFormat (UserEvent (UserUpdated i n pic acc ass hdl loc mb _)) =
  Just $
    M.fromList
      [ "type" .= ("user.update" :: Text),
        "user"
          .= object
            ( "id" .= i
                # "name" .= n
                # "picture" .= pic -- DEPRECATED
                # "accent_id" .= acc
                # "assets" .= ass
                # "handle" .= hdl
                # "locale" .= loc
                # "managed_by" .= mb
                # []
            )
      ]
toPushFormat (UserEvent UserIdentityUpdated {..}) =
  Just $
    M.fromList
      [ "type" .= ("user.update" :: Text),
        "user"
          .= object
            ( "id" .= eiuId
                # "email" .= eiuEmail
                # "phone" .= eiuPhone
                # []
            )
      ]
toPushFormat (UserEvent (UserIdentityRemoved i e p)) =
  Just $
    M.fromList
      [ "type" .= ("user.identity-remove" :: Text),
        "user"
          .= object
            ( "id" .= i
                # "email" .= e
                # "phone" .= p
                # []
            )
      ]
toPushFormat (ConnectionEvent (ConnectionUpdated uc _ name)) =
  Just $ M.fromList $
    "type" .= ("user.connection" :: Text)
      # "connection" .= uc
      # "user" .= case name of
        Just n -> Just $ object ["name" .= n]
        Nothing -> Nothing
      # []
toPushFormat (UserEvent (UserSuspended i)) =
  Just $
    M.fromList
      [ "type" .= ("user.suspend" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserResumed i)) =
  Just $
    M.fromList
      [ "type" .= ("user.resume" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserDeleted i)) =
  Just $
    M.fromList
      [ "type" .= ("user.delete" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserLegalHoldDisabled i)) =
  Just $
    M.fromList
      [ "type" .= ("user.legalhold-disable" :: Text),
        "id" .= i
      ]
toPushFormat (UserEvent (UserLegalHoldEnabled i)) =
  Just $
    M.fromList
      [ "type" .= ("user.legalhold-enable" :: Text),
        "id" .= i
      ]
toPushFormat (PropertyEvent (PropertySet _ k v)) =
  Just $
    M.fromList
      [ "type" .= ("user.properties-set" :: Text),
        "key" .= k,
        "value" .= v
      ]
toPushFormat (PropertyEvent (PropertyDeleted _ k)) =
  Just $
    M.fromList
      [ "type" .= ("user.properties-delete" :: Text),
        "key" .= k
      ]
toPushFormat (PropertyEvent (PropertiesCleared _)) =
  Just $
    M.fromList
      [ "type" .= ("user.properties-clear" :: Text)
      ]
toPushFormat (ClientEvent (ClientAdded _ c)) =
  Just $
    M.fromList
      [ "type" .= ("user.client-add" :: Text),
        "client" .= c
      ]
toPushFormat (ClientEvent (ClientRemoved _ c)) =
  Just $
    M.fromList
      [ "type" .= ("user.client-remove" :: Text),
        "client" .= IdObject (clientId c)
      ]
toPushFormat (UserEvent (LegalHoldClientRequested payload)) =
  let LegalHoldClientRequestedData targetUser lastPrekey' clientId = payload
   in Just $
        M.fromList
          [ "type" .= ("user.legalhold-request" :: Text),
            "id" .= targetUser,
            "last_prekey" .= lastPrekey',
            "client" .= IdObject clientId
          ]

toApsData :: Event -> Maybe ApsData
toApsData (ConnectionEvent (ConnectionUpdated uc _ name)) =
  case (ucStatus uc, name) of
    (Pending, Just n) -> Just $ apsConnRequest n
    (Accepted, Just n) -> Just $ apsConnAccept n
    (_, _) -> Nothing
  where
    apsConnRequest n =
      apsData (ApsLocKey "push.notification.connection.request") [fromName n]
        & apsSound ?~ ApsSound "new_message_apns.caf"
    apsConnAccept n =
      apsData (ApsLocKey "push.notification.connection.accepted") [fromName n]
        & apsSound ?~ ApsSound "new_message_apns.caf"
toApsData _ = Nothing

-------------------------------------------------------------------------------
-- Conversation Management

-- | calls 'Galley.API.createSelfConversationH'
createSelfConv :: UserId -> AppIO ()
createSelfConv u = do
  debug $
    remote "galley"
      . msg (val "Creating self conversation")
  void $ galleyRequest POST req
  where
    req =
      path "/conversations/self"
        . zUser u
        . expect2xx

-- | calls 'Galley.API.createConnectConversationH'
--
-- if conversation did not exist before:
--   ConvCreate EdConversation event to self
-- if only the other already was member before:
--   MemberJoin EdMembersJoin event to you and other
-- if conversation already existed with two members (more precisely: not a connect conversation):
--   ConvConnect EdConnect event to self
createConnectConv :: N -> UserId -> UserId -> Maybe Text -> Maybe Message -> Maybe ConnId -> AppIO ConvId
createConnectConv N from to cname mess conn = do
  debug $
    Log.connection from to
      . remote "galley"
      . msg (val "Creating connect conversation")
  r <- galleyRequest POST req
  maybe (error "invalid conv id") return
    $ fromByteString
    $ getHeader' "Location" r
  where
    req =
      path "/i/conversations/connect"
        . zUser from
        . maybe id (header "Z-Connection" . fromConnId) conn
        . contentJson
        . lbytes (encode $ Connect to (messageText <$> mess) cname Nothing)
        . expect2xx

-- | calls 'Galley.API.acceptConvH'
--
-- MemberJoin EdMembersJoin event to you, if the conversation existed and had < 2 members before
-- MemberJoin EdMembersJoin event to other, if the conversation existed and only the other already was member before
acceptConnectConv :: N -> UserId -> Maybe ConnId -> ConvId -> AppIO Conversation
acceptConnectConv N from conn cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Accepting connect conversation")
  galleyRequest PUT req >>= decodeBody "galley"
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "accept", "v2"]
        . zUser from
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

-- | calls 'Galley.API.blockConvH'
blockConv :: UserId -> Maybe ConnId -> ConvId -> AppIO ()
blockConv usr conn cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Blocking conversation")
  void $ galleyRequest PUT req
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "block"]
        . zUser usr
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

-- | calls 'Galley.API.unblockConvH'
--
-- MemberJoin EdMembersJoin event to you, if the conversation had < 2 members before
-- MemberJoin EdMembersJoin event to other, if only the other already was member before
unblockConv :: N -> UserId -> Maybe ConnId -> ConvId -> AppIO Conversation
unblockConv N usr conn cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Unblocking conversation")
  galleyRequest PUT req >>= decodeBody "galley"
  where
    req =
      paths ["/i/conversations", toByteString' cnv, "unblock"]
        . zUser usr
        . maybe id (header "Z-Connection" . fromConnId) conn
        . expect2xx

-- | calls 'Galley.API.getConversationH'
getConv :: UserId -> ConvId -> AppIO (Maybe Conversation)
getConv usr cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting conversation")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

-- | calls 'Galley.API.getTeamConversationH'
getTeamConv :: UserId -> TeamId -> ConvId -> AppIO (Maybe Team.TeamConversation)
getTeamConv usr tid cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting team conversation")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["teams", toByteString' tid, "conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

-------------------------------------------------------------------------------
-- User management

-- | calls 'Galley.API.rmUserH', as well as gundeck and cargohold
--
-- MemberLeave EdMembersLeave event to members for all conversations the user was in
rmUser :: N -> UserId -> [Asset] -> AppIO ()
rmUser N usr asts = do
  debug $
    remote "gundeck"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  void $ gundeckRequest DELETE (path "/i/user" . zUser usr . expect2xx)
  debug $
    remote "galley"
      . field "user" (toByteString usr)
      . msg (val "remove user")
  void $ galleyRequest DELETE (path "/i/user" . zUser usr . expect2xx)
  debug $
    remote "cargohold"
      . field "user" (toByteString usr)
      . msg (val "remove profile assets")
  -- Note that we _may_ not get a 2xx response code from cargohold (e.g., client has
  -- deleted the asset "directly" with cargohold; on our side, we just do our best to
  -- delete it in case it is still there
  forM_ asts $ \ast ->
    cargoholdRequest DELETE (paths ["assets/v3", toByteString' $ assetKey ast] . zUser usr)

-------------------------------------------------------------------------------
-- Client management

-- | calls 'Galley.API.addClientH'
newClient :: UserId -> ClientId -> AppIO ()
newClient u c = do
  debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (toByteString c)
      . msg (val "new client")
  let p = paths ["i", "clients", toByteString' c]
  void $ galleyRequest POST (p . zUser u . expect2xx)

-- | calls 'Galley.API.rmClientH', as well as gundeck
rmClient :: UserId -> ClientId -> AppIO ()
rmClient u c = do
  let cid = toByteString' c
  debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "remove client")
  let p = paths ["i", "clients", cid]
  void $ galleyRequest DELETE (p . zUser u . expect expected)
  -- for_ clabel rmClientCookie
  debug $
    remote "gundeck"
      . field "user" (toByteString u)
      . field "client" (BL.fromStrict cid)
      . msg (val "unregister push client")
  g <- view gundeck
  void . recovering x3 rpcHandlers $ const $
    rpc'
      "gundeck"
      g
      ( method DELETE
          . paths ["i", "clients", cid]
          . zUser u
          . expect expected
      )
  where
    expected = [status200, status204, status404]

-------------------------------------------------------------------------------
-- Team Management

-- | calls 'Galley.API.uncheckedAddTeamMemberH'
addTeamMember :: UserId -> TeamId -> (Maybe (UserId, UTCTimeMillis), Team.Role) -> AppIO Bool
addTeamMember u tid (minvmeta, role) = do
  debug $
    remote "galley"
      . msg (val "Adding member to team")
  rs <- galleyRequest POST req
  return $ case Bilge.statusCode rs of
    200 -> True
    _ -> False
  where
    prm = Team.rolePermissions role
    bdy = Team.newNewTeamMember $ Team.newTeamMember u prm minvmeta
    req =
      paths ["i", "teams", toByteString' tid, "members"]
        . header "Content-Type" "application/json"
        . zUser u
        . expect [status200, status403]
        . lbytes (encode bdy)

-- | calls 'Galley.API.createBindingTeamH'
createTeam :: UserId -> Team.BindingNewTeam -> TeamId -> AppIO CreateUserTeam
createTeam u t@(Team.BindingNewTeam bt) teamid = do
  debug $
    remote "galley"
      . msg (val "Creating Team")
  r <- galleyRequest PUT $ req teamid
  tid <-
    maybe (error "invalid team id") return
      $ fromByteString
      $ getHeader' "Location" r
  return (CreateUserTeam tid $ fromRange (bt ^. Team.newTeamName))
  where
    req tid =
      paths ["i", "teams", toByteString' tid]
        . header "Content-Type" "application/json"
        . zUser u
        . expect2xx
        . lbytes (encode t)

-- | calls 'Galley.API.uncheckedGetTeamMemberH'
getTeamMember :: UserId -> TeamId -> AppIO (Maybe Team.TeamMember)
getTeamMember u tid = do
  debug $
    remote "galley"
      . msg (val "Get team member")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["i", "teams", toByteString' tid, "members", toByteString' u]
        . zUser u
        . expect [status200, status404]

-- | calls 'Galley.API.uncheckedGetTeamMembersH'
getTeamMembers :: TeamId -> AppIO Team.TeamMemberList
getTeamMembers tid = do
  debug $ remote "galley" . msg (val "Get team members")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "members"]
        . expect2xx

-- | calls 'Galley.API.getTruncatedTeamSizeH'
getTruncatedTeamSize :: TeamId -> Range 1 Team.HardTruncationLimit Int32 -> AppIO Team.TruncatedTeamSize
getTruncatedTeamSize tid limit = do
  debug $ remote "galley" . msg (val "Get limited team size")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "truncated-size", toByteString' limit]
        . expect2xx

-- | calls 'Galley.API.getBindingTeamMembersH'
-- | Only works on 'BindingTeam's!
getTeamContacts :: UserId -> AppIO (Maybe Team.TeamMemberList)
getTeamContacts u = do
  debug $ remote "galley" . msg (val "Get team contacts")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team", "members"]
        . expect [status200, status404]

-- | 'Nothing' means no team could be found.  'Just' contains all members of the team that have full
-- permissions.
--
-- TODO: This could arguably also live in galley, since it is about teams.  But it also needs emails
-- of users, so no matter whether it lives in galley or brig, one has to call the other for this to
-- be decided.  A small refactoring to improve on this: split up 'getTeamOwners' into the part that
-- fetches the team members from galley, and the part that filters them for permissions and email
-- addresses.  When galley wants to know, the second part can be called from
-- /i/users/:uid/can-be-deleted directly with a list of team members passed to that end-point in the
-- body.  When brig wants to know, it can call both parts.  These thoughts may all become obsolete
-- if we introduce a deletion service in the future.
getTeamOwners :: TeamId -> AppIO [Team.TeamMember]
getTeamOwners tid = filter Team.isTeamOwner . view Team.teamMembers <$> getTeamMembers tid

-- | Like 'getTeamOwners', but only returns owners with an flag indicating whether they have
-- an email address.
getTeamOwnersWithEmail :: TeamId -> AppIO [(Team.TeamMember, Bool)]
getTeamOwnersWithEmail tid = do
  mems <- getTeamOwners tid
  usrList :: [User] <- lookupUsers ((^. Team.userId) <$> mems)
  let usrMap :: Map.Map UserId Bool
      usrMap = Map.fromList $ mkListItem <$> usrList
      mkListItem :: User -> (UserId, Bool)
      mkListItem usr = (userId usr, isJust $ userEmail usr)
      hasEmail :: Team.TeamMember -> Bool
      hasEmail mem = maybe False id $ Map.lookup (mem ^. Team.userId) usrMap
  pure $ (\mem -> (mem, hasEmail mem)) <$> mems

-- | calls 'Galley.API.getBindingTeamIdH'
getTeamId :: UserId -> AppIO (Maybe TeamId)
getTeamId u = do
  debug $ remote "galley" . msg (val "Get team from user")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> return Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team"]
        . expect [status200, status404]

-- | calls 'Galley.API.getTeamInternalH'
getTeam :: TeamId -> AppIO Team.TeamData
getTeam tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid]
        . expect2xx

-- | calls 'Galley.API.getTeamInternalH'
getTeamName :: TeamId -> AppIO Team.TeamName
getTeamName tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "name"]
        . expect2xx

-- | calls 'Galley.API.getLegalholdStatusInternalH'
getTeamLegalHoldStatus :: TeamId -> AppIO LegalHoldTeamConfig
getTeamLegalHoldStatus tid = do
  debug $ remote "galley" . msg (val "Get legalhold settings")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "features", "legalhold"]
        . expect2xx

-- | calls 'Galley.API.updateTeamStatusH'
changeTeamStatus :: TeamId -> Team.TeamStatus -> Maybe Currency.Alpha -> AppIO ()
changeTeamStatus tid s cur = do
  debug $ remote "galley" . msg (val "Change Team status")
  void $ galleyRequest PUT req
  where
    req =
      paths ["i", "teams", toByteString' tid, "status"]
        . header "Content-Type" "application/json"
        . expect2xx
        . lbytes (encode $ Team.TeamStatusUpdate s cur)
