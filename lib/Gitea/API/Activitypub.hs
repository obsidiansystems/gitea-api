{-
   Gitea API.

   This documentation describes the Gitea API.

   OpenAPI Version: 3.0.1
   Gitea API. API version: 1.19.4
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Gitea.API.Activitypub
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Gitea.API.Activitypub where

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


-- ** Activitypub

-- *** activitypubPerson

-- | @GET \/activitypub\/user-id\/{user-id}@
-- 
-- Returns the Person actor for a user
-- 
-- AuthMethod: 'AuthApiKeyTOTPHeader', 'AuthApiKeyAuthorizationHeaderToken', 'AuthApiKeySudoHeader', 'AuthBasicBasicAuth', 'AuthApiKeyAccessToken', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
activitypubPerson
  :: UserId -- ^ "userId" -  user ID of the user
  -> GiteaRequest ActivitypubPerson MimeNoContent ActivityPub MimeJSON
activitypubPerson (UserId userId) =
  _mkRequest "GET" ["/activitypub/user-id/",toPath userId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data ActivitypubPerson  
-- | @application/json@
instance Produces ActivitypubPerson MimeJSON


-- *** activitypubPersonInbox

-- | @POST \/activitypub\/user-id\/{user-id}\/inbox@
-- 
-- Send to the inbox
-- 
-- AuthMethod: 'AuthApiKeyTOTPHeader', 'AuthApiKeyAuthorizationHeaderToken', 'AuthApiKeySudoHeader', 'AuthBasicBasicAuth', 'AuthApiKeyAccessToken', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
activitypubPersonInbox
  :: UserId -- ^ "userId" -  user ID of the user
  -> GiteaRequest ActivitypubPersonInbox MimeNoContent NoContent MimeNoContent
activitypubPersonInbox (UserId userId) =
  _mkRequest "POST" ["/activitypub/user-id/",toPath userId,"/inbox"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data ActivitypubPersonInbox  
instance Produces ActivitypubPersonInbox MimeNoContent

