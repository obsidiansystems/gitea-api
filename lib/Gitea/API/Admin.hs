{-
   Gitea API.

   This documentation describes the Gitea API.

   OpenAPI Version: 3.0.1
   Gitea API. API version: 1.1.1
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Gitea.API.Admin
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Gitea.API.Admin where

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


-- ** Admin

-- *** adminCreateOrg

-- | @POST \/admin\/users\/{username}\/orgs@
-- 
-- Create an organization
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminCreateOrg 
  :: (Consumes AdminCreateOrg MimeJSON, MimeRender MimeJSON CreateOrgOption)
  => CreateOrgOption -- ^ "organization"
  -> Username -- ^ "username" -  username of the user that will own the created organization
  -> GiteaRequest AdminCreateOrg MimeJSON Organization MimeJSON
adminCreateOrg organization (Username username) =
  _mkRequest "POST" ["/admin/users/",toPath username,"/orgs"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)
    `setBodyParam` organization

data AdminCreateOrg 
instance HasBodyParam AdminCreateOrg CreateOrgOption 

-- | @application/json@
instance Consumes AdminCreateOrg MimeJSON

-- | @application/json@
instance Produces AdminCreateOrg MimeJSON


-- *** adminCreatePublicKey

-- | @POST \/admin\/users\/{username}\/keys@
-- 
-- Add a public key on behalf of a user
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminCreatePublicKey 
  :: (Consumes AdminCreatePublicKey MimeJSON)
  => Username -- ^ "username" -  username of the user
  -> GiteaRequest AdminCreatePublicKey MimeJSON PublicKey MimeJSON
adminCreatePublicKey (Username username) =
  _mkRequest "POST" ["/admin/users/",toPath username,"/keys"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data AdminCreatePublicKey 
instance HasBodyParam AdminCreatePublicKey CreateKeyOption 

-- | @application/json@
instance Consumes AdminCreatePublicKey MimeJSON

-- | @application/json@
instance Produces AdminCreatePublicKey MimeJSON


-- *** adminCreateRepo

-- | @POST \/admin\/users\/{username}\/repos@
-- 
-- Create a repository on behalf a user
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminCreateRepo 
  :: (Consumes AdminCreateRepo MimeJSON, MimeRender MimeJSON CreateRepoOption)
  => CreateRepoOption -- ^ "repository"
  -> Username -- ^ "username" -  username of the user. This user will own the created repository
  -> GiteaRequest AdminCreateRepo MimeJSON Repository MimeJSON
adminCreateRepo repository (Username username) =
  _mkRequest "POST" ["/admin/users/",toPath username,"/repos"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)
    `setBodyParam` repository

data AdminCreateRepo 
instance HasBodyParam AdminCreateRepo CreateRepoOption 

-- | @application/json@
instance Consumes AdminCreateRepo MimeJSON

-- | @application/json@
instance Produces AdminCreateRepo MimeJSON


-- *** adminCreateUser

-- | @POST \/admin\/users@
-- 
-- Create a user
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminCreateUser 
  :: (Consumes AdminCreateUser MimeJSON)
  => GiteaRequest AdminCreateUser MimeJSON User MimeJSON
adminCreateUser =
  _mkRequest "POST" ["/admin/users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data AdminCreateUser 
instance HasBodyParam AdminCreateUser CreateUserOption 

-- | @application/json@
instance Consumes AdminCreateUser MimeJSON

-- | @application/json@
instance Produces AdminCreateUser MimeJSON


-- *** adminDeleteUser

-- | @DELETE \/admin\/users\/{username}@
-- 
-- Delete a user
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminDeleteUser 
  :: Username -- ^ "username" -  username of user to delete
  -> GiteaRequest AdminDeleteUser MimeNoContent NoContent MimeNoContent
adminDeleteUser (Username username) =
  _mkRequest "DELETE" ["/admin/users/",toPath username]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data AdminDeleteUser  
instance Produces AdminDeleteUser MimeNoContent


-- *** adminDeleteUserPublicKey

-- | @DELETE \/admin\/users\/{username}\/keys\/{id}@
-- 
-- Delete a user's public key
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminDeleteUserPublicKey 
  :: Username -- ^ "username" -  username of user
  -> Id -- ^ "id" -  id of the key to delete
  -> GiteaRequest AdminDeleteUserPublicKey MimeNoContent NoContent MimeNoContent
adminDeleteUserPublicKey (Username username) (Id id) =
  _mkRequest "DELETE" ["/admin/users/",toPath username,"/keys/",toPath id]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data AdminDeleteUserPublicKey  
instance Produces AdminDeleteUserPublicKey MimeNoContent


-- *** adminEditUser

-- | @PATCH \/admin\/users\/{username}@
-- 
-- Edit an existing user
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminEditUser 
  :: (Consumes AdminEditUser MimeJSON)
  => Username -- ^ "username" -  username of user to edit
  -> GiteaRequest AdminEditUser MimeJSON User MimeJSON
adminEditUser (Username username) =
  _mkRequest "PATCH" ["/admin/users/",toPath username]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data AdminEditUser 
instance HasBodyParam AdminEditUser EditUserOption 

-- | @application/json@
instance Consumes AdminEditUser MimeJSON

-- | @application/json@
instance Produces AdminEditUser MimeJSON


-- *** adminGetAllOrgs

-- | @GET \/admin\/orgs@
-- 
-- List all organizations
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminGetAllOrgs 
  :: GiteaRequest AdminGetAllOrgs MimeNoContent [Organization] MimeJSON
adminGetAllOrgs =
  _mkRequest "GET" ["/admin/orgs"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data AdminGetAllOrgs  

-- | /Optional Param/ "page" - page number of results to return (1-based)
instance HasOptionalParam AdminGetAllOrgs Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "limit" - page size of results, maximum page size is 50
instance HasOptionalParam AdminGetAllOrgs Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces AdminGetAllOrgs MimeJSON


-- *** adminGetAllUsers

-- | @GET \/admin\/users@
-- 
-- List all users
-- 
-- AuthMethod: 'AuthApiKeyAccessToken', 'AuthApiKeyAuthorizationHeaderToken', 'AuthBasicBasicAuth', 'AuthApiKeySudoHeader', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
adminGetAllUsers 
  :: GiteaRequest AdminGetAllUsers MimeNoContent [User] MimeJSON
adminGetAllUsers =
  _mkRequest "GET" ["/admin/users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data AdminGetAllUsers  
-- | @application/json@
instance Produces AdminGetAllUsers MimeJSON

