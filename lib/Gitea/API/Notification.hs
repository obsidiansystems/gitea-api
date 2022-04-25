{-
   Gitea API.

   This documentation describes the Gitea API.

   OpenAPI Version: 3.0.1
   Gitea API. API version: 1.15.9
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Gitea.API.Notification
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Gitea.API.Notification where

import Gitea.Core
import Gitea.MimeTypes
import Gitea.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Notification

-- *** notifyGetList

-- | @GET \/notifications@
-- 
-- List users's notification threads
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyTOTPHeader', 'AuthApiKeyToken'
-- 
notifyGetList 
  :: GiteaRequest NotifyGetList MimeNoContent [NotificationThread] MimeJSON
notifyGetList =
  _mkRequest "GET" ["/notifications"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data NotifyGetList  

-- | /Optional Param/ "all" - If true, show notifications marked as read. Default value is false
instance HasOptionalParam NotifyGetList All where
  applyOptionalParam req (All xs) =
    req `setQuery` toQuery ("all", Just xs)

-- | /Optional Param/ "status-types" - Show notifications with the provided status types. Options are: unread, read and/or pinned. Defaults to unread & pinned.
instance HasOptionalParam NotifyGetList StatusTypes where
  applyOptionalParam req (StatusTypes xs) =
    req `setQuery` toQueryColl MultiParamArray ("status-types", Just xs)

-- | /Optional Param/ "subject-type" - filter notifications by subject type
instance HasOptionalParam NotifyGetList SubjectType where
  applyOptionalParam req (SubjectType xs) =
    req `setQuery` toQueryColl MultiParamArray ("subject-type", Just xs)

-- | /Optional Param/ "since" - Only show notifications updated after the given time. This is a timestamp in RFC 3339 format
instance HasOptionalParam NotifyGetList Since where
  applyOptionalParam req (Since xs) =
    req `setQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "before" - Only show notifications updated before the given time. This is a timestamp in RFC 3339 format
instance HasOptionalParam NotifyGetList Before where
  applyOptionalParam req (Before xs) =
    req `setQuery` toQuery ("before", Just xs)

-- | /Optional Param/ "page" - page number of results to return (1-based)
instance HasOptionalParam NotifyGetList Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "limit" - page size of results
instance HasOptionalParam NotifyGetList Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces NotifyGetList MimeJSON


-- *** notifyGetRepoList

-- | @GET \/repos\/{owner}\/{repo}\/notifications@
-- 
-- List users's notification threads on a specific repo
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyTOTPHeader', 'AuthApiKeyToken'
-- 
notifyGetRepoList 
  :: Owner -- ^ "owner" -  owner of the repo
  -> Repo -- ^ "repo" -  name of the repo
  -> GiteaRequest NotifyGetRepoList MimeNoContent [NotificationThread] MimeJSON
notifyGetRepoList (Owner owner) (Repo repo) =
  _mkRequest "GET" ["/repos/",toPath owner,"/",toPath repo,"/notifications"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data NotifyGetRepoList  

-- | /Optional Param/ "all" - If true, show notifications marked as read. Default value is false
instance HasOptionalParam NotifyGetRepoList All where
  applyOptionalParam req (All xs) =
    req `setQuery` toQuery ("all", Just xs)

-- | /Optional Param/ "status-types" - Show notifications with the provided status types. Options are: unread, read and/or pinned. Defaults to unread & pinned
instance HasOptionalParam NotifyGetRepoList StatusTypes where
  applyOptionalParam req (StatusTypes xs) =
    req `setQuery` toQueryColl MultiParamArray ("status-types", Just xs)

-- | /Optional Param/ "subject-type" - filter notifications by subject type
instance HasOptionalParam NotifyGetRepoList SubjectType where
  applyOptionalParam req (SubjectType xs) =
    req `setQuery` toQueryColl MultiParamArray ("subject-type", Just xs)

-- | /Optional Param/ "since" - Only show notifications updated after the given time. This is a timestamp in RFC 3339 format
instance HasOptionalParam NotifyGetRepoList Since where
  applyOptionalParam req (Since xs) =
    req `setQuery` toQuery ("since", Just xs)

-- | /Optional Param/ "before" - Only show notifications updated before the given time. This is a timestamp in RFC 3339 format
instance HasOptionalParam NotifyGetRepoList Before where
  applyOptionalParam req (Before xs) =
    req `setQuery` toQuery ("before", Just xs)

-- | /Optional Param/ "page" - page number of results to return (1-based)
instance HasOptionalParam NotifyGetRepoList Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "limit" - page size of results
instance HasOptionalParam NotifyGetRepoList Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces NotifyGetRepoList MimeJSON


-- *** notifyGetThread

-- | @GET \/notifications\/threads\/{id}@
-- 
-- Get notification thread by ID
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyTOTPHeader', 'AuthApiKeyToken'
-- 
notifyGetThread 
  :: IdText -- ^ "id" -  id of notification thread
  -> GiteaRequest NotifyGetThread MimeNoContent NotificationThread MimeJSON
notifyGetThread (IdText id) =
  _mkRequest "GET" ["/notifications/threads/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data NotifyGetThread  
-- | @application/json@
instance Produces NotifyGetThread MimeJSON


-- *** notifyNewAvailable

-- | @GET \/notifications\/new@
-- 
-- Check if unread notifications exist
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyTOTPHeader', 'AuthApiKeyToken'
-- 
notifyNewAvailable 
  :: Accept accept -- ^ request accept ('MimeType')
  -> GiteaRequest NotifyNewAvailable MimeNoContent NotificationCount accept
notifyNewAvailable  _ =
  _mkRequest "GET" ["/notifications/new"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data NotifyNewAvailable  
-- | @text/html@
instance Produces NotifyNewAvailable MimeTextHtml
-- | @application/json@
instance Produces NotifyNewAvailable MimeJSON


-- *** notifyReadList

-- | @PUT \/notifications@
-- 
-- Mark notification threads as read, pinned or unread
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyTOTPHeader', 'AuthApiKeyToken'
-- 
notifyReadList 
  :: GiteaRequest NotifyReadList MimeNoContent NoContent MimeNoContent
notifyReadList =
  _mkRequest "PUT" ["/notifications"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data NotifyReadList  

-- | /Optional Param/ "last_read_at" - Describes the last point that notifications were checked. Anything updated since this time will not be updated.
instance HasOptionalParam NotifyReadList LastReadAt where
  applyOptionalParam req (LastReadAt xs) =
    req `setQuery` toQuery ("last_read_at", Just xs)

-- | /Optional Param/ "all" - If true, mark all notifications on this repo. Default value is false
instance HasOptionalParam NotifyReadList AllText where
  applyOptionalParam req (AllText xs) =
    req `setQuery` toQuery ("all", Just xs)

-- | /Optional Param/ "status-types" - Mark notifications with the provided status types. Options are: unread, read and/or pinned. Defaults to unread.
instance HasOptionalParam NotifyReadList StatusTypes where
  applyOptionalParam req (StatusTypes xs) =
    req `setQuery` toQueryColl MultiParamArray ("status-types", Just xs)

-- | /Optional Param/ "to-status" - Status to mark notifications as, Defaults to read.
instance HasOptionalParam NotifyReadList ToStatus where
  applyOptionalParam req (ToStatus xs) =
    req `setQuery` toQuery ("to-status", Just xs)
instance Produces NotifyReadList MimeNoContent


-- *** notifyReadRepoList

-- | @PUT \/repos\/{owner}\/{repo}\/notifications@
-- 
-- Mark notification threads as read, pinned or unread on a specific repo
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyTOTPHeader', 'AuthApiKeyToken'
-- 
notifyReadRepoList 
  :: Owner -- ^ "owner" -  owner of the repo
  -> Repo -- ^ "repo" -  name of the repo
  -> GiteaRequest NotifyReadRepoList MimeNoContent NoContent MimeNoContent
notifyReadRepoList (Owner owner) (Repo repo) =
  _mkRequest "PUT" ["/repos/",toPath owner,"/",toPath repo,"/notifications"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data NotifyReadRepoList  

-- | /Optional Param/ "all" - If true, mark all notifications on this repo. Default value is false
instance HasOptionalParam NotifyReadRepoList AllText where
  applyOptionalParam req (AllText xs) =
    req `setQuery` toQuery ("all", Just xs)

-- | /Optional Param/ "status-types" - Mark notifications with the provided status types. Options are: unread, read and/or pinned. Defaults to unread.
instance HasOptionalParam NotifyReadRepoList StatusTypes where
  applyOptionalParam req (StatusTypes xs) =
    req `setQuery` toQueryColl MultiParamArray ("status-types", Just xs)

-- | /Optional Param/ "to-status" - Status to mark notifications as. Defaults to read.
instance HasOptionalParam NotifyReadRepoList ToStatus where
  applyOptionalParam req (ToStatus xs) =
    req `setQuery` toQuery ("to-status", Just xs)

-- | /Optional Param/ "last_read_at" - Describes the last point that notifications were checked. Anything updated since this time will not be updated.
instance HasOptionalParam NotifyReadRepoList LastReadAt where
  applyOptionalParam req (LastReadAt xs) =
    req `setQuery` toQuery ("last_read_at", Just xs)
instance Produces NotifyReadRepoList MimeNoContent


-- *** notifyReadThread

-- | @PATCH \/notifications\/threads\/{id}@
-- 
-- Mark notification thread as read by ID
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyTOTPHeader', 'AuthApiKeyToken'
-- 
notifyReadThread 
  :: IdText -- ^ "id" -  id of notification thread
  -> GiteaRequest NotifyReadThread MimeNoContent NoContent MimeNoContent
notifyReadThread (IdText id) =
  _mkRequest "PATCH" ["/notifications/threads/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data NotifyReadThread  

-- | /Optional Param/ "to-status" - Status to mark notifications as
instance HasOptionalParam NotifyReadThread ToStatus where
  applyOptionalParam req (ToStatus xs) =
    req `setQuery` toQuery ("to-status", Just xs)
instance Produces NotifyReadThread MimeNoContent

