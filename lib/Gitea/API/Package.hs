{-
   Gitea API

   This documentation describes the Gitea API.

   OpenAPI Version: 3.0.1
   Gitea API API version: 1.21.11
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Gitea.API.Package
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Gitea.API.Package where

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


-- ** Package

-- *** deletePackage

-- | @DELETE \/packages\/{owner}\/{type}\/{name}\/{version}@
-- 
-- Delete a package
-- 
-- AuthMethod: 'AuthApiKeyTOTPHeader', 'AuthApiKeyAuthorizationHeaderToken', 'AuthApiKeySudoHeader', 'AuthBasicBasicAuth', 'AuthApiKeyAccessToken', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
deletePackage
  :: Owner -- ^ "owner" -  owner of the package
  -> ParamTypeText -- ^ "_type" -  type of the package
  -> Name -- ^ "name" -  name of the package
  -> Version -- ^ "version" -  version of the package
  -> GiteaRequest DeletePackage MimeNoContent NoContent MimeNoContent
deletePackage (Owner owner) (ParamTypeText _type) (Name name) (Version version) =
  _mkRequest "DELETE" ["/packages/",toPath owner,"/",toPath _type,"/",toPath name,"/",toPath version]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data DeletePackage  
instance Produces DeletePackage MimeNoContent


-- *** getPackage

-- | @GET \/packages\/{owner}\/{type}\/{name}\/{version}@
-- 
-- Gets a package
-- 
-- AuthMethod: 'AuthApiKeyTOTPHeader', 'AuthApiKeyAuthorizationHeaderToken', 'AuthApiKeySudoHeader', 'AuthBasicBasicAuth', 'AuthApiKeyAccessToken', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
getPackage
  :: Owner -- ^ "owner" -  owner of the package
  -> ParamTypeText -- ^ "_type" -  type of the package
  -> Name -- ^ "name" -  name of the package
  -> Version -- ^ "version" -  version of the package
  -> GiteaRequest GetPackage MimeNoContent Package MimeJSON
getPackage (Owner owner) (ParamTypeText _type) (Name name) (Version version) =
  _mkRequest "GET" ["/packages/",toPath owner,"/",toPath _type,"/",toPath name,"/",toPath version]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data GetPackage  
-- | @application/json@
instance Produces GetPackage MimeJSON


-- *** listPackageFiles

-- | @GET \/packages\/{owner}\/{type}\/{name}\/{version}\/files@
-- 
-- Gets all files of a package
-- 
-- AuthMethod: 'AuthApiKeyTOTPHeader', 'AuthApiKeyAuthorizationHeaderToken', 'AuthApiKeySudoHeader', 'AuthBasicBasicAuth', 'AuthApiKeyAccessToken', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
listPackageFiles
  :: Owner -- ^ "owner" -  owner of the package
  -> ParamTypeText -- ^ "_type" -  type of the package
  -> Name -- ^ "name" -  name of the package
  -> Version -- ^ "version" -  version of the package
  -> GiteaRequest ListPackageFiles MimeNoContent [PackageFile] MimeJSON
listPackageFiles (Owner owner) (ParamTypeText _type) (Name name) (Version version) =
  _mkRequest "GET" ["/packages/",toPath owner,"/",toPath _type,"/",toPath name,"/",toPath version,"/files"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data ListPackageFiles  
-- | @application/json@
instance Produces ListPackageFiles MimeJSON


-- *** listPackages

-- | @GET \/packages\/{owner}@
-- 
-- Gets all packages of an owner
-- 
-- AuthMethod: 'AuthApiKeyTOTPHeader', 'AuthApiKeyAuthorizationHeaderToken', 'AuthApiKeySudoHeader', 'AuthBasicBasicAuth', 'AuthApiKeyAccessToken', 'AuthApiKeySudoParam', 'AuthApiKeyToken'
-- 
listPackages
  :: Owner -- ^ "owner" -  owner of the packages
  -> GiteaRequest ListPackages MimeNoContent [Package] MimeJSON
listPackages (Owner owner) =
  _mkRequest "GET" ["/packages/",toPath owner]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyTOTPHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAuthorizationHeaderToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoHeader)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthBasicBasicAuth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyAccessToken)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeySudoParam)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyToken)

data ListPackages  

-- | /Optional Param/ "page" - page number of results to return (1-based)
instance HasOptionalParam ListPackages Page where
  applyOptionalParam req (Page xs) =
    req `addQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "limit" - page size of results
instance HasOptionalParam ListPackages Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "type" - package type filter
instance HasOptionalParam ListPackages ParamType where
  applyOptionalParam req (ParamType xs) =
    req `addQuery` toQuery ("type", Just xs)

-- | /Optional Param/ "q" - name filter
instance HasOptionalParam ListPackages Q where
  applyOptionalParam req (Q xs) =
    req `addQuery` toQuery ("q", Just xs)
-- | @application/json@
instance Produces ListPackages MimeJSON

