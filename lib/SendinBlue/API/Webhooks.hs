{-
   SendinBlue API

   SendinBlue provide a RESTFul API that can be used with any languages. With this API, you will be able to :   - Manage your campaigns and get the statistics   - Manage your contacts   - Send transactional Emails and SMS   - and much more...  You can download our wrappers at https://github.com/orgs/sendinblue  **Possible responses**   | Code | Message |   | :-------------: | ------------- |   | 200  | OK. Successful Request  |   | 201  | OK. Successful Creation |   | 202  | OK. Request accepted |   | 204  | OK. Successful Update/Deletion  |   | 400  | Error. Bad Request  |   | 401  | Error. Authentication Needed  |   | 402  | Error. Not enough credit, plan upgrade needed  |   | 403  | Error. Permission denied  |   | 404  | Error. Object does not exist |   | 405  | Error. Method not allowed  |   | 406  | Error. Not Acceptable  | 

   OpenAPI Version: 3.0.1
   SendinBlue API API version: 3.0.0
   Contact: contact@sendinblue.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : SendinBlue.API.Webhooks
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SendinBlue.API.Webhooks where

import SendinBlue.Core
import SendinBlue.MimeTypes
import SendinBlue.Model as M

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


-- ** Webhooks

-- *** createWebhook0

-- | @POST \/webhooks@
-- 
-- Create a webhook
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
createWebhook0
  :: (Consumes CreateWebhook0 MimeJSON, MimeRender MimeJSON CreateWebhook)
  => CreateWebhook -- ^ "createWebhook" -  Values to create a webhook
  -> SendinBlueRequest CreateWebhook0 MimeJSON CreateModel MimeJSON
createWebhook0 createWebhook =
  _mkRequest "POST" ["/webhooks"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` createWebhook

data CreateWebhook0 

-- | /Body Param/ "createWebhook" - Values to create a webhook
instance HasBodyParam CreateWebhook0 CreateWebhook 

-- | @application/json@
instance Consumes CreateWebhook0 MimeJSON

-- | @application/json@
instance Produces CreateWebhook0 MimeJSON


-- *** deleteWebhook

-- | @DELETE \/webhooks\/{webhookId}@
-- 
-- Delete a webhook
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
deleteWebhook
  :: WebhookId -- ^ "webhookId" -  Id of the webhook
  -> SendinBlueRequest DeleteWebhook MimeNoContent NoContent MimeNoContent
deleteWebhook (WebhookId webhookId) =
  _mkRequest "DELETE" ["/webhooks/",toPath webhookId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteWebhook  
instance Produces DeleteWebhook MimeNoContent


-- *** getWebhook0

-- | @GET \/webhooks\/{webhookId}@
-- 
-- Get a webhook details
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getWebhook0
  :: WebhookId -- ^ "webhookId" -  Id of the webhook
  -> SendinBlueRequest GetWebhook0 MimeNoContent GetWebhook MimeJSON
getWebhook0 (WebhookId webhookId) =
  _mkRequest "GET" ["/webhooks/",toPath webhookId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetWebhook0  
-- | @application/json@
instance Produces GetWebhook0 MimeJSON


-- *** getWebhooks0

-- | @GET \/webhooks@
-- 
-- Get all webhooks
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getWebhooks0
  :: SendinBlueRequest GetWebhooks0 MimeNoContent GetWebhooks MimeJSON
getWebhooks0 =
  _mkRequest "GET" ["/webhooks"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetWebhooks0  

-- | /Optional Param/ "type" - Filter on webhook type
instance HasOptionalParam GetWebhooks0 ParamType2 where
  applyOptionalParam req (ParamType2 xs) =
    req `addQuery` toQuery ("type", Just xs)

-- | /Optional Param/ "sort" - Sort the results in the ascending/descending order of webhook creation
instance HasOptionalParam GetWebhooks0 Sort where
  applyOptionalParam req (Sort xs) =
    req `addQuery` toQuery ("sort", Just xs)
-- | @application/json@
instance Produces GetWebhooks0 MimeJSON


-- *** updateWebhook0

-- | @PUT \/webhooks\/{webhookId}@
-- 
-- Update a webhook
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
updateWebhook0
  :: (Consumes UpdateWebhook0 MimeJSON, MimeRender MimeJSON UpdateWebhook)
  => UpdateWebhook -- ^ "updateWebhook" -  Values to update a webhook
  -> WebhookId -- ^ "webhookId" -  Id of the webhook
  -> SendinBlueRequest UpdateWebhook0 MimeJSON NoContent MimeNoContent
updateWebhook0 updateWebhook (WebhookId webhookId) =
  _mkRequest "PUT" ["/webhooks/",toPath webhookId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` updateWebhook

data UpdateWebhook0 

-- | /Body Param/ "updateWebhook" - Values to update a webhook
instance HasBodyParam UpdateWebhook0 UpdateWebhook 

-- | @application/json@
instance Consumes UpdateWebhook0 MimeJSON

instance Produces UpdateWebhook0 MimeNoContent

