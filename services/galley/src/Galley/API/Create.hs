module Galley.API.Create
  ( createGroupConversationH,
    internalCreateManagedConversationH,
    createSelfConversationH,
    createOne2OneConversationH,
    createConnectConversationH,
  )
where

import Control.Lens hiding ((??))
import Control.Monad.Catch
import Data.Id
import Data.IdMapping (MappedOrLocalId (Local, Mapped))
import Data.List.NonEmpty (nonEmpty)
import Data.List1 (list1)
import Data.Range
import qualified Data.Set as Set
import Data.Time
import qualified Data.UUID.Tagged as U
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Intra.Push
import Galley.Types
import Galley.Types.Teams hiding (EventType (..))
import Galley.Validation
import Imports hiding ((\\))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities

----------------------------------------------------------------------------
-- Group conversations

-- | The public-facing endpoint for creating group conversations.
--
-- See Note [managed conversations].
--
-- notifies members
-- ConvCreate
createGroupConversationH :: E -> UserId ::: ConnId ::: JsonRequest NewConvUnmanaged -> Galley Response
createGroupConversationH E (zusr ::: zcon ::: req) = do
  newConv <- fromJsonBody req
  handleConversationResponse <$> createGroupConversation E zusr zcon newConv

-- ConvCreate EdConversation event to members
createGroupConversation :: E -> UserId -> ConnId -> NewConvUnmanaged -> Galley ConversationResponse
createGroupConversation E zusr zcon wrapped@(NewConvUnmanaged body) = do
  case newConvTeam body of
    Nothing ->
      -- ConvCreate EdConversation event to members
      createRegularGroupConv E zusr zcon wrapped
    Just tinfo ->
      -- ConvCreate EdConversation event to members
      createTeamGroupConv E zusr zcon tinfo body

-- | An internal endpoint for creating managed group conversations. Will
-- throw an error for everything else.
--
-- ConvCreate EdConversation event to members
internalCreateManagedConversationH :: E -> UserId ::: ConnId ::: JsonRequest NewConvManaged -> Galley Response
internalCreateManagedConversationH E (zusr ::: zcon ::: req) = do
  newConv <- fromJsonBody req
  handleConversationResponse <$> internalCreateManagedConversation E zusr zcon newConv

-- ConvCreate EdConversation event to members
internalCreateManagedConversation :: E -> UserId -> ConnId -> NewConvManaged -> Galley ConversationResponse
internalCreateManagedConversation E zusr zcon (NewConvManaged body) = do
  case newConvTeam body of
    Nothing -> throwM internalError
    Just tinfo ->
      -- ConvCreate EdConversation event to members
      createTeamGroupConv E zusr zcon tinfo body

-- | A helper for creating a regular (non-team) group conversation.
--
-- ConvCreate EdConversation event to members
createRegularGroupConv :: E -> UserId -> ConnId -> NewConvUnmanaged -> Galley ConversationResponse
createRegularGroupConv E zusr zcon (NewConvUnmanaged body) = do
  name <- rangeCheckedMaybe (newConvName body)
  _uids <- checkedConvSize (newConvUsers body) -- currently not needed, as we only consider local IDs
  mappedOrLocalUserIds <- traverse resolveOpaqueUserId (newConvUsers body)
  let (localUserIds, remoteUserIds) = partitionMappedOrLocalIds mappedOrLocalUserIds
  ensureConnected zusr mappedOrLocalUserIds
  -- FUTUREWORK(federation): notify remote users' backends about new conversation
  for_ (nonEmpty remoteUserIds) $
    throwM . federationNotImplemented
  localCheckedUsers <- checkedConvSize localUserIds
  c <-
    Data.createConversation
      zusr
      name
      (access body)
      (accessRole body)
      localCheckedUsers
      (newConvTeam body)
      (newConvMessageTimer body)
      (newConvReceiptMode body)
      (newConvUsersRole body)
  -- ConvCreate EdConversation event to members
  notifyCreatedConversation E Nothing zusr (Just zcon) c
  conversationCreated zusr c

-- | A helper for creating a team group conversation, used by the endpoint
-- handlers above. Allows both unmanaged and managed conversations.
--
-- ConvCreate EdConversation event to members
createTeamGroupConv :: E -> UserId -> ConnId -> ConvTeamInfo -> NewConv -> Galley ConversationResponse
createTeamGroupConv E zusr zcon tinfo body = do
  (localUserIds, remoteUserIds) <-
    partitionMappedOrLocalIds <$> traverse resolveOpaqueUserId (newConvUsers body)
  -- for now, teams don't support conversations with remote members
  for_ (nonEmpty remoteUserIds) $
    throwM . federationNotImplemented
  name <- rangeCheckedMaybe (newConvName body)
  let convTeam = (cnvTeamId tinfo)
  zusrMembership <- Data.teamMember convTeam zusr
  convMemberships <- mapM (Data.teamMember convTeam) localUserIds
  ensureAccessRole (accessRole body) (zip localUserIds convMemberships)
  void $ permissionCheck CreateConversation zusrMembership
  otherConvMems <-
    if cnvManaged tinfo
      then do
        allMembers <- Data.teamMembersUnsafeForLargeTeams convTeam
        let otherConvMems = filter (/= zusr) $ map (view userId) $ allMembers
        checkedConvSize otherConvMems
      else do
        otherConvMems <- checkedConvSize localUserIds
        -- In teams we don't have 1:1 conversations, only regular conversations. We want
        -- users without the 'AddRemoveConvMember' permission to still be able to create
        -- regular conversations, therefore we check for 'AddRemoveConvMember' only if
        -- there are going to be more than two users in the conversation.
        -- FUTUREWORK: We keep this permission around because not doing so will break backwards
        -- compatibility in the sense that the team role 'partners' would be able to create group
        -- conversations (which they should not be able to).
        -- Not sure at the moment how to best solve this but it is unlikely
        -- we can ever get rid of the team permission model anyway - the only thing I can
        -- think of is that 'partners' can create convs but not be admins...
        when (length (fromConvSize otherConvMems) > 1) $ do
          void $ permissionCheck DoNotUseDeprecatedAddRemoveConvMember zusrMembership
        -- Team members are always considered to be connected, so we only check
        -- 'ensureConnected' for non-team-members.
        ensureConnectedToLocals zusr (notTeamMember (fromConvSize otherConvMems) (catMaybes convMemberships))
        pure otherConvMems
  conv <- Data.createConversation zusr name (access body) (accessRole body) otherConvMems (newConvTeam body) (newConvMessageTimer body) (newConvReceiptMode body) (newConvUsersRole body)
  now <- liftIO getCurrentTime
  -- NOTE: We only send (conversation) events to members of the conversation
  --
  -- ConvCreate EdConversation event to members
  notifyCreatedConversation E (Just now) zusr (Just zcon) conv
  conversationCreated zusr conv

----------------------------------------------------------------------------
-- Other kinds of conversations

createSelfConversationH :: UserId -> Galley Response
createSelfConversationH zusr = do
  handleConversationResponse <$> createSelfConversation zusr

createSelfConversation :: UserId -> Galley ConversationResponse
createSelfConversation zusr = do
  c <- Data.conversation (Id . toUUID $ zusr)
  maybe create (conversationExisted zusr) c
  where
    create = do
      c <- Data.createSelfConversation zusr Nothing
      conversationCreated zusr c

-- ConvCreate EdConversation event to members
createOne2OneConversationH :: E -> UserId ::: ConnId ::: JsonRequest NewConvUnmanaged -> Galley Response
createOne2OneConversationH E (zusr ::: zcon ::: req) = do
  newConv <- fromJsonBody req
  -- ConvCreate EdConversation event to members
  handleConversationResponse <$> createOne2OneConversation E zusr zcon newConv

-- ConvCreate EdConversation event to members
createOne2OneConversation :: E -> UserId -> ConnId -> NewConvUnmanaged -> Galley ConversationResponse
createOne2OneConversation E zusr zcon (NewConvUnmanaged j) = do
  other <- head . fromRange <$> (rangeChecked (newConvUsers j) :: Galley (Range 1 1 [OpaqueUserId]))
  (x, y) <- toUUIDs (makeIdOpaque zusr) other
  when (x == y)
    $ throwM
    $ invalidOp "Cannot create a 1-1 with yourself"
  otherUserId <- resolveOpaqueUserId other
  case newConvTeam j of
    Just ti
      | cnvManaged ti -> throwM noManagedTeamConv
      | otherwise -> case otherUserId of
        Local localOther -> checkBindingTeamPermissions zusr localOther (cnvTeamId ti)
        Mapped _ -> throwM noBindingTeamMembers -- remote user can't be in local team
    Nothing -> do
      ensureConnected zusr [otherUserId]
  n <- rangeCheckedMaybe (newConvName j)
  c <- Data.conversation (Data.one2OneConvId x y)
  maybe (create x y n $ newConvTeam j) (conversationExisted zusr) c
  where
    verifyMembership tid u = do
      membership <- Data.teamMember tid u
      when (isNothing membership) $
        throwM noBindingTeamMembers
    checkBindingTeamPermissions x y tid = do
      zusrMembership <- Data.teamMember tid zusr
      void $ permissionCheck CreateConversation zusrMembership
      Data.teamBinding tid >>= \case
        Just Binding -> do
          verifyMembership tid x
          verifyMembership tid y
        Just _ -> throwM nonBindingTeam
        Nothing -> throwM teamNotFound
    create x y n tinfo = do
      c <- Data.createOne2OneConversation x y n (cnvTeamId <$> tinfo)
      -- ConvCreate EdConversation event to members
      notifyCreatedConversation E Nothing zusr (Just zcon) c
      conversationCreated zusr c

createConnectConversationH :: E -> UserId ::: Maybe ConnId ::: JsonRequest Connect -> Galley Response
createConnectConversationH E (usr ::: conn ::: req) = do
  j <- fromJsonBody req
  handleConversationResponse <$> createConnectConversation E usr conn j

-- TODO: figure out the logic
-- TODO: add event types
-- notifies self (and other, if they already created the connect conversation)
-- ConvCreate EdConversation event to members, if conversation gets created (did not already exist)
-- EdConnect event to self, if conversation gets created (did not already exist)
--
-- if conversation existed, but we are not member
createConnectConversation :: E -> UserId -> Maybe ConnId -> Connect -> Galley ConversationResponse
createConnectConversation () usr conn j = do
  (x, y) <- toUUIDs (makeIdOpaque usr) (makeIdOpaque (cRecipient j))
  n <- rangeCheckedMaybe (cName j)
  conv <- Data.conversation (Data.one2OneConvId x y)
  maybe
    -- ConvCreate EdConversation event to members, if conversation did not exist before
    -- ConvConnect event to self, if conversation did not exist before
    (create x y n)
    -- ConvConnect EdConnect event to self and other, if conversation existed and type is connect
    -- EdMembersJoin event to you and other, if conversation existed and type is connect and only the other is already part of it
    (update E n)
    conv
  where
    -- ConvCreate EdConversation event to members
    create x y n = do
      (c, e) <- Data.createConnectConversation x y n j
      -- ConvCreate EdConversation event to members
      notifyCreatedConversation E Nothing usr conn c
      for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> Data.convMembers c)) $ \p ->
        -- EdConnect event to self TODO
        push1 E $
          p
            & pushRoute .~ RouteDirect
            & pushConn .~ conn
      conversationCreated usr c
    -- ConvConnect EdConnect event to self and other, if conversation type is connect
    -- EdMembersJoin event to you and other, if conversation type is connect and only the other is already part of it
    update E n conv =
      let mems = Data.convMembers conv
       in conversationExisted usr
            =<< if | makeIdOpaque usr `isMember` mems ->
                     -- ConvConnect EdConnect event to self and other, if conversation type is connect
                     connect E n conv
                   | otherwise -> do
                     now <- liftIO getCurrentTime
                     mm <- snd <$> Data.addMember now (Data.convId conv) usr
                     let conv' =
                           conv
                             { Data.convMembers = Data.convMembers conv <> toList mm
                             }
                     if null mems
                       then--
                       -- ConvConnect EdConnect event to self and other, if conversation type is connect
                         connect E n conv'
                       else do
                         -- EdMembersJoin event to you and other, if it's a ConnectConv that had < 2 members
                         conv'' <- acceptOne2One E usr conv' conn
                         if Data.convType conv'' == ConnectConv
                           then--
                           -- ConvConnect EdConnect event to self and other, if conversation type is connect
                             connect E n conv''
                           else return conv''
    -- ConvConnect EdConnect event to self and other, if conversation type is connect
    connect E n conv
      | Data.convType conv == ConnectConv = do
        n' <- case n of
          Just x -> do
            Data.updateConversation (Data.convId conv) x
            return . Just $ fromRange x
          Nothing -> return $ Data.convName conv
        t <- liftIO getCurrentTime
        let e = Event ConvConnect (Data.convId conv) usr t (Just $ EdConnect j)
        for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> Data.convMembers conv)) $ \p ->
          -- ConvConnect EdConnect event to self and other
          push1 E $
            p
              & pushRoute .~ RouteDirect
              & pushConn .~ conn
        return $ conv {Data.convName = n'}
      | otherwise = return conv

-------------------------------------------------------------------------------
-- Helpers

data ConversationResponse
  = ConversationCreated !Conversation
  | ConversationExisted !Conversation

conversationCreated :: UserId -> Data.Conversation -> Galley ConversationResponse
conversationCreated usr cnv = ConversationCreated <$> conversationView usr cnv

conversationExisted :: UserId -> Data.Conversation -> Galley ConversationResponse
conversationExisted usr cnv = ConversationExisted <$> conversationView usr cnv

handleConversationResponse :: ConversationResponse -> Response
handleConversationResponse = \case
  ConversationCreated cnv -> json cnv & setStatus status201 . location (cnvId cnv)
  ConversationExisted cnv -> json cnv & setStatus status200 . location (cnvId cnv)

-- ConvCreate EdConversation event to members
notifyCreatedConversation :: E -> Maybe UTCTime -> UserId -> Maybe ConnId -> Data.Conversation -> Galley ()
notifyCreatedConversation E dtime usr conn c = do
  now <- maybe (liftIO getCurrentTime) pure dtime
  -- ConvCreate EdConversation event to members
  pushSome E =<< mapM (toPush now) (Data.convMembers c)
  where
    route
      | Data.convType c == RegularConv = RouteAny
      | otherwise = RouteDirect
    toPush t m = do
      c' <- conversationView (memId m) c
      let e = Event ConvCreate (Data.convId c) usr t (Just $ EdConversation c')
      return $
        newPush1 (evtFrom e) (ConvEvent e) (list1 (recipient m) [])
          & pushConn .~ conn
          & pushRoute .~ route

toUUIDs :: OpaqueUserId -> OpaqueUserId -> Galley (U.UUID U.V4, U.UUID U.V4)
toUUIDs a b = do
  a' <- U.fromUUID (toUUID a) & ifNothing invalidUUID4
  b' <- U.fromUUID (toUUID b) & ifNothing invalidUUID4
  return (a', b')

accessRole :: NewConv -> AccessRole
accessRole b = fromMaybe Data.defRole (newConvAccessRole b)

access :: NewConv -> [Access]
access a = case Set.toList (newConvAccess a) of
  [] -> Data.defRegularConvAccess
  (x : xs) -> x : xs
