{-
   SendinBlue API

   SendinBlue provide a RESTFul API that can be used with any languages. With this API, you will be able to :   - Manage your campaigns and get the statistics   - Manage your contacts   - Send transactional Emails and SMS   - and much more...  You can download our wrappers at https://github.com/orgs/sendinblue  **Possible responses**   | Code | Message |   | :-------------: | ------------- |   | 200  | OK. Successful Request  |   | 201  | OK. Successful Creation |   | 202  | OK. Request accepted |   | 204  | OK. Successful Update/Deletion  |   | 400  | Error. Bad Request  |   | 401  | Error. Authentication Needed  |   | 402  | Error. Not enough credit, plan upgrade needed  |   | 403  | Error. Permission denied  |   | 404  | Error. Object does not exist |   | 405  | Error. Method not allowed  |   | 406  | Error. Not Acceptable  | 

   OpenAPI Version: 3.0.1
   SendinBlue API API version: 3.0.0
   Contact: contact@sendinblue.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : SendinBlue.API.SMSCampaigns
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SendinBlue.API.SMSCampaigns where

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


-- ** SMSCampaigns

-- *** createSmsCampaign0

-- | @POST \/smsCampaigns@
-- 
-- Creates an SMS campaign
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
createSmsCampaign0
  :: (Consumes CreateSmsCampaign0 MimeJSON, MimeRender MimeJSON CreateSmsCampaign)
  => CreateSmsCampaign -- ^ "createSmsCampaign" -  Values to create an SMS Campaign
  -> SendinBlueRequest CreateSmsCampaign0 MimeJSON CreateModel MimeJSON
createSmsCampaign0 createSmsCampaign =
  _mkRequest "POST" ["/smsCampaigns"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` createSmsCampaign

data CreateSmsCampaign0 

-- | /Body Param/ "createSmsCampaign" - Values to create an SMS Campaign
instance HasBodyParam CreateSmsCampaign0 CreateSmsCampaign 

-- | @application/json@
instance Consumes CreateSmsCampaign0 MimeJSON

-- | @application/json@
instance Produces CreateSmsCampaign0 MimeJSON


-- *** deleteSmsCampaign

-- | @DELETE \/smsCampaigns\/{campaignId}@
-- 
-- Delete an SMS campaign
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
deleteSmsCampaign
  :: CampaignId -- ^ "campaignId" -  id of the SMS campaign
  -> SendinBlueRequest DeleteSmsCampaign MimeNoContent NoContent MimeNoContent
deleteSmsCampaign (CampaignId campaignId) =
  _mkRequest "DELETE" ["/smsCampaigns/",toPath campaignId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteSmsCampaign  
instance Produces DeleteSmsCampaign MimeNoContent


-- *** getSmsCampaign0

-- | @GET \/smsCampaigns\/{campaignId}@
-- 
-- Get an SMS campaign
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getSmsCampaign0
  :: CampaignId -- ^ "campaignId" -  id of the SMS campaign
  -> SendinBlueRequest GetSmsCampaign0 MimeNoContent GetSmsCampaign MimeJSON
getSmsCampaign0 (CampaignId campaignId) =
  _mkRequest "GET" ["/smsCampaigns/",toPath campaignId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetSmsCampaign0  
-- | @application/json@
instance Produces GetSmsCampaign0 MimeJSON


-- *** getSmsCampaigns0

-- | @GET \/smsCampaigns@
-- 
-- Returns the information for all your created SMS campaigns
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getSmsCampaigns0
  :: SendinBlueRequest GetSmsCampaigns0 MimeNoContent GetSmsCampaigns MimeJSON
getSmsCampaigns0 =
  _mkRequest "GET" ["/smsCampaigns"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetSmsCampaigns0  

-- | /Optional Param/ "status" - Status of campaign.
instance HasOptionalParam GetSmsCampaigns0 Status where
  applyOptionalParam req (Status xs) =
    req `addQuery` toQuery ("status", Just xs)

-- | /Optional Param/ "startDate" - Mandatory if endDate is used. Starting (urlencoded) UTC date-time (YYYY-MM-DDTHH:mm:ss.SSSZ) to filter the sent sms campaigns. Prefer to pass your timezone in date-time format for accurate result ( only available if either 'status' not passed and if passed is set to 'sent' )
instance HasOptionalParam GetSmsCampaigns0 StartDate where
  applyOptionalParam req (StartDate xs) =
    req `addQuery` toQuery ("startDate", Just xs)

-- | /Optional Param/ "endDate" - Mandatory if startDate is used. Ending (urlencoded) UTC date-time (YYYY-MM-DDTHH:mm:ss.SSSZ) to filter the sent sms campaigns. Prefer to pass your timezone in date-time format for accurate result ( only available if either 'status' not passed and if passed is set to 'sent' )
instance HasOptionalParam GetSmsCampaigns0 EndDate where
  applyOptionalParam req (EndDate xs) =
    req `addQuery` toQuery ("endDate", Just xs)

-- | /Optional Param/ "limit" - Number limitation for the result returned
instance HasOptionalParam GetSmsCampaigns0 Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Beginning point in the list to retrieve from.
instance HasOptionalParam GetSmsCampaigns0 Offset where
  applyOptionalParam req (Offset xs) =
    req `addQuery` toQuery ("offset", Just xs)
-- | @application/json@
instance Produces GetSmsCampaigns0 MimeJSON


-- *** requestSmsRecipientExport0

-- | @POST \/smsCampaigns\/{campaignId}\/exportRecipients@
-- 
-- Export an SMS campaign's recipients
-- 
-- It returns the background process ID which on completion calls the notify URL that you have set in the input.
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
requestSmsRecipientExport0
  :: (Consumes RequestSmsRecipientExport0 MimeJSON)
  => CampaignId -- ^ "campaignId" -  id of the campaign
  -> SendinBlueRequest RequestSmsRecipientExport0 MimeJSON CreatedProcessId MimeJSON
requestSmsRecipientExport0 (CampaignId campaignId) =
  _mkRequest "POST" ["/smsCampaigns/",toPath campaignId,"/exportRecipients"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data RequestSmsRecipientExport0 

-- | /Body Param/ "recipientExport" - Values to send for a recipient export request
instance HasBodyParam RequestSmsRecipientExport0 RequestSmsRecipientExport 

-- | @application/json@
instance Consumes RequestSmsRecipientExport0 MimeJSON

-- | @application/json@
instance Produces RequestSmsRecipientExport0 MimeJSON


-- *** sendSmsCampaignNow

-- | @POST \/smsCampaigns\/{campaignId}\/sendNow@
-- 
-- Send your SMS campaign immediately
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
sendSmsCampaignNow
  :: CampaignId -- ^ "campaignId" -  id of the campaign
  -> SendinBlueRequest SendSmsCampaignNow MimeNoContent NoContent MimeNoContent
sendSmsCampaignNow (CampaignId campaignId) =
  _mkRequest "POST" ["/smsCampaigns/",toPath campaignId,"/sendNow"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data SendSmsCampaignNow  
instance Produces SendSmsCampaignNow MimeNoContent


-- *** sendSmsReport

-- | @POST \/smsCampaigns\/{campaignId}\/sendReport@
-- 
-- Send an SMS campaign's report
-- 
-- Send report of Sent and Archived campaign, to the specified email addresses, with respective data and a pdf attachment in detail.
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
sendSmsReport
  :: (Consumes SendSmsReport MimeJSON, MimeRender MimeJSON SendReport)
  => SendReport -- ^ "sendReport" -  Values for send a report
  -> CampaignId -- ^ "campaignId" -  id of the campaign
  -> SendinBlueRequest SendSmsReport MimeJSON NoContent MimeNoContent
sendSmsReport sendReport (CampaignId campaignId) =
  _mkRequest "POST" ["/smsCampaigns/",toPath campaignId,"/sendReport"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` sendReport

data SendSmsReport 

-- | /Body Param/ "sendReport" - Values for send a report
instance HasBodyParam SendSmsReport SendReport 

-- | @application/json@
instance Consumes SendSmsReport MimeJSON

instance Produces SendSmsReport MimeNoContent


-- *** sendTestSms0

-- | @POST \/smsCampaigns\/{campaignId}\/sendTest@
-- 
-- Send a test SMS campaign
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
sendTestSms0
  :: (Consumes SendTestSms0 MimeJSON, MimeRender MimeJSON SendTestSms)
  => SendTestSms -- ^ "phoneNumber" -  Mobile number of the recipient with the country code. This number must belong to one of your contacts in SendinBlue account and must not be blacklisted
  -> CampaignId -- ^ "campaignId" -  Id of the SMS campaign
  -> SendinBlueRequest SendTestSms0 MimeJSON NoContent MimeNoContent
sendTestSms0 phoneNumber (CampaignId campaignId) =
  _mkRequest "POST" ["/smsCampaigns/",toPath campaignId,"/sendTest"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` phoneNumber

data SendTestSms0 

-- | /Body Param/ "phoneNumber" - Mobile number of the recipient with the country code. This number must belong to one of your contacts in SendinBlue account and must not be blacklisted
instance HasBodyParam SendTestSms0 SendTestSms 

-- | @application/json@
instance Consumes SendTestSms0 MimeJSON

instance Produces SendTestSms0 MimeNoContent


-- *** updateSmsCampaign0

-- | @PUT \/smsCampaigns\/{campaignId}@
-- 
-- Update an SMS campaign
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
updateSmsCampaign0
  :: (Consumes UpdateSmsCampaign0 MimeJSON, MimeRender MimeJSON UpdateSmsCampaign)
  => UpdateSmsCampaign -- ^ "updateSmsCampaign" -  Values to update an SMS Campaign
  -> CampaignId -- ^ "campaignId" -  id of the SMS campaign
  -> SendinBlueRequest UpdateSmsCampaign0 MimeJSON NoContent MimeNoContent
updateSmsCampaign0 updateSmsCampaign (CampaignId campaignId) =
  _mkRequest "PUT" ["/smsCampaigns/",toPath campaignId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` updateSmsCampaign

data UpdateSmsCampaign0 

-- | /Body Param/ "updateSmsCampaign" - Values to update an SMS Campaign
instance HasBodyParam UpdateSmsCampaign0 UpdateSmsCampaign 

-- | @application/json@
instance Consumes UpdateSmsCampaign0 MimeJSON

instance Produces UpdateSmsCampaign0 MimeNoContent


-- *** updateSmsCampaignStatus

-- | @PUT \/smsCampaigns\/{campaignId}\/status@
-- 
-- Update a campaign's status
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
updateSmsCampaignStatus
  :: (Consumes UpdateSmsCampaignStatus MimeJSON, MimeRender MimeJSON UpdateCampaignStatus)
  => UpdateCampaignStatus -- ^ "status" -  Status of the campaign.
  -> CampaignId -- ^ "campaignId" -  id of the campaign
  -> SendinBlueRequest UpdateSmsCampaignStatus MimeJSON NoContent MimeNoContent
updateSmsCampaignStatus status (CampaignId campaignId) =
  _mkRequest "PUT" ["/smsCampaigns/",toPath campaignId,"/status"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` status

data UpdateSmsCampaignStatus 

-- | /Body Param/ "status" - Status of the campaign.
instance HasBodyParam UpdateSmsCampaignStatus UpdateCampaignStatus 

-- | @application/json@
instance Consumes UpdateSmsCampaignStatus MimeJSON

instance Produces UpdateSmsCampaignStatus MimeNoContent

