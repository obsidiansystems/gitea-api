{-
   Gitea API

   This documentation describes the Gitea API.

   OpenAPI Version: 3.0.1
   Gitea API API version: 1.23.1
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Gitea.Logging
Logging functions
-}
{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module Gitea.Logging
  ( module Gitea.LoggingKatip
  ) where

import Gitea.LoggingKatip

#else

module Gitea.Logging
  ( module Gitea.LoggingMonadLogger
  ) where

import Gitea.LoggingMonadLogger

#endif
