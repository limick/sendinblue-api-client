{-
   SendinBlue API

   SendinBlue provide a RESTFul API that can be used with any languages. With this API, you will be able to :   - Manage your campaigns and get the statistics   - Manage your contacts   - Send transactional Emails and SMS   - and much more...  You can download our wrappers at https://github.com/orgs/sendinblue  **Possible responses**   | Code | Message |   | :-------------: | ------------- |   | 200  | OK. Successful Request  |   | 201  | OK. Successful Creation |   | 202  | OK. Request accepted |   | 204  | OK. Successful Update/Deletion  |   | 400  | Error. Bad Request  |   | 401  | Error. Authentication Needed  |   | 402  | Error. Not enough credit, plan upgrade needed  |   | 403  | Error. Permission denied  |   | 404  | Error. Object does not exist |   | 405  | Error. Method not allowed  |   | 406  | Error. Not Acceptable  | 

   OpenAPI spec version: 2.0
   SendinBlue API API version: 3.0.0
   Contact: contact@sendinblue.com
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : SendinBlue.API.Reseller
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SendinBlue.API.Reseller where

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


-- ** Reseller

-- *** addCredits0

-- | @POST \/reseller\/children\/{childAuthKey}\/credits\/add@
-- 
-- Add Email and/or SMS credits to a specific child account
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
addCredits0 
  :: (Consumes AddCredits0 MimeJSON, MimeRender MimeJSON AddCredits)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> AddCredits -- ^ "addCredits" -  Values to post to add credit to a specific child account
  -> SendinBlueRequest AddCredits0 MimeJSON RemainingCreditModel MimeJSON
addCredits0 (ChildAuthKey childAuthKey) addCredits =
  _mkRequest "POST" ["/reseller/children/",toPath childAuthKey,"/credits/add"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` addCredits

data AddCredits0 

-- | /Body Param/ "addCredits" - Values to post to add credit to a specific child account
instance HasBodyParam AddCredits0 AddCredits 

-- | @application/json@
instance Consumes AddCredits0 MimeJSON

-- | @application/json@
instance Produces AddCredits0 MimeJSON


-- *** associateIpToChild

-- | @POST \/reseller\/children\/{childAuthKey}\/ips\/associate@
-- 
-- Associate a dedicated IP to the child
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
associateIpToChild 
  :: (Consumes AssociateIpToChild MimeJSON, MimeRender MimeJSON ManageIp)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> ManageIp -- ^ "ip" -  IP to associate
  -> SendinBlueRequest AssociateIpToChild MimeJSON res MimeJSON
associateIpToChild (ChildAuthKey childAuthKey) ip =
  _mkRequest "POST" ["/reseller/children/",toPath childAuthKey,"/ips/associate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` ip

data AssociateIpToChild 

-- | /Body Param/ "ip" - IP to associate
instance HasBodyParam AssociateIpToChild ManageIp 

-- | @application/json@
instance Consumes AssociateIpToChild MimeJSON

-- | @application/json@
instance Produces AssociateIpToChild MimeJSON


-- *** createChildDomain

-- | @POST \/reseller\/children\/{childAuthKey}\/domains@
-- 
-- Creates a domain for a child account
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
createChildDomain 
  :: (Consumes CreateChildDomain MimeJSON, MimeRender MimeJSON AddChildDomain)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> AddChildDomain -- ^ "addChildDomain" -  Sender domain to add for a specific child account
  -> SendinBlueRequest CreateChildDomain MimeJSON res MimeJSON
createChildDomain (ChildAuthKey childAuthKey) addChildDomain =
  _mkRequest "POST" ["/reseller/children/",toPath childAuthKey,"/domains"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` addChildDomain

data CreateChildDomain 

-- | /Body Param/ "addChildDomain" - Sender domain to add for a specific child account
instance HasBodyParam CreateChildDomain AddChildDomain 

-- | @application/json@
instance Consumes CreateChildDomain MimeJSON

-- | @application/json@
instance Produces CreateChildDomain MimeJSON


-- *** createResellerChild

-- | @POST \/reseller\/children@
-- 
-- Creates a reseller child
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
createResellerChild 
  :: (Consumes CreateResellerChild MimeJSON)
  => SendinBlueRequest CreateResellerChild MimeJSON CreateReseller MimeJSON
createResellerChild =
  _mkRequest "POST" ["/reseller/children"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data CreateResellerChild 

-- | /Body Param/ "resellerChild" - reseller child to add
instance HasBodyParam CreateResellerChild CreateChild 

-- | @application/json@
instance Consumes CreateResellerChild MimeJSON

-- | @application/json@
instance Produces CreateResellerChild MimeJSON


-- *** deleteChildDomain

-- | @DELETE \/reseller\/children\/{childAuthKey}\/domains\/{domainName}@
-- 
-- Deletes the sender domain of the reseller child based on the childAuthKey and domainName passed
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
deleteChildDomain 
  :: ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> DomainName -- ^ "domainName" -  Pass the existing domain that needs to be deleted
  -> SendinBlueRequest DeleteChildDomain MimeNoContent res MimeJSON
deleteChildDomain (ChildAuthKey childAuthKey) (DomainName domainName) =
  _mkRequest "DELETE" ["/reseller/children/",toPath childAuthKey,"/domains/",toPath domainName]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteChildDomain  

-- | @application/json@
instance Consumes DeleteChildDomain MimeJSON

-- | @application/json@
instance Produces DeleteChildDomain MimeJSON


-- *** deleteResellerChild

-- | @DELETE \/reseller\/children\/{childAuthKey}@
-- 
-- Deletes a single reseller child based on the childAuthKey supplied
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
deleteResellerChild 
  :: ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> SendinBlueRequest DeleteResellerChild MimeNoContent res MimeJSON
deleteResellerChild (ChildAuthKey childAuthKey) =
  _mkRequest "DELETE" ["/reseller/children/",toPath childAuthKey]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteResellerChild  

-- | @application/json@
instance Consumes DeleteResellerChild MimeJSON

-- | @application/json@
instance Produces DeleteResellerChild MimeJSON


-- *** dissociateIpFromChild

-- | @POST \/reseller\/children\/{childAuthKey}\/ips\/dissociate@
-- 
-- Dissociate a dedicated IP to the child
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
dissociateIpFromChild 
  :: (Consumes DissociateIpFromChild MimeJSON, MimeRender MimeJSON ManageIp)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> ManageIp -- ^ "ip" -  IP to dissociate
  -> SendinBlueRequest DissociateIpFromChild MimeJSON res MimeJSON
dissociateIpFromChild (ChildAuthKey childAuthKey) ip =
  _mkRequest "POST" ["/reseller/children/",toPath childAuthKey,"/ips/dissociate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` ip

data DissociateIpFromChild 

-- | /Body Param/ "ip" - IP to dissociate
instance HasBodyParam DissociateIpFromChild ManageIp 

-- | @application/json@
instance Consumes DissociateIpFromChild MimeJSON

-- | @application/json@
instance Produces DissociateIpFromChild MimeJSON


-- *** getChildAccountCreationStatus0

-- | @GET \/reseller\/children\/{childAuthKey}\/accountCreationStatus@
-- 
-- Returns the status of reseller's child account creation, whether it is successfully created (exists) or not based on the childAuthKey supplied
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getChildAccountCreationStatus0 
  :: ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> SendinBlueRequest GetChildAccountCreationStatus0 MimeNoContent GetChildAccountCreationStatus MimeJSON
getChildAccountCreationStatus0 (ChildAuthKey childAuthKey) =
  _mkRequest "GET" ["/reseller/children/",toPath childAuthKey,"/accountCreationStatus"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetChildAccountCreationStatus0  

-- | @application/json@
instance Consumes GetChildAccountCreationStatus0 MimeJSON

-- | @application/json@
instance Produces GetChildAccountCreationStatus0 MimeJSON


-- *** getChildDomains0

-- | @GET \/reseller\/children\/{childAuthKey}\/domains@
-- 
-- Gets all the sender domains of a specific child account
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getChildDomains0 
  :: ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> SendinBlueRequest GetChildDomains0 MimeNoContent GetChildDomains MimeJSON
getChildDomains0 (ChildAuthKey childAuthKey) =
  _mkRequest "GET" ["/reseller/children/",toPath childAuthKey,"/domains"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetChildDomains0  

-- | @application/json@
instance Consumes GetChildDomains0 MimeJSON

-- | @application/json@
instance Produces GetChildDomains0 MimeJSON


-- *** getChildInfo0

-- | @GET \/reseller\/children\/{childAuthKey}@
-- 
-- Gets the info about a specific child account
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getChildInfo0 
  :: ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> SendinBlueRequest GetChildInfo0 MimeNoContent GetChildInfo MimeJSON
getChildInfo0 (ChildAuthKey childAuthKey) =
  _mkRequest "GET" ["/reseller/children/",toPath childAuthKey]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetChildInfo0  

-- | @application/json@
instance Consumes GetChildInfo0 MimeJSON

-- | @application/json@
instance Produces GetChildInfo0 MimeJSON


-- *** getResellerChilds

-- | @GET \/reseller\/children@
-- 
-- Gets the list of all reseller's children accounts
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getResellerChilds 
  :: SendinBlueRequest GetResellerChilds MimeNoContent GetChildrenList MimeJSON
getResellerChilds =
  _mkRequest "GET" ["/reseller/children"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetResellerChilds  

-- | /Optional Param/ "limit" - Number of documents for child accounts information per page
instance HasOptionalParam GetResellerChilds Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Index of the first document in the page
instance HasOptionalParam GetResellerChilds Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)

-- | @application/json@
instance Consumes GetResellerChilds MimeJSON

-- | @application/json@
instance Produces GetResellerChilds MimeJSON


-- *** getSsoToken0

-- | @GET \/reseller\/children\/{childAuthKey}\/auth@
-- 
-- Get session token to access Sendinblue (SSO)
-- 
-- It returns a session [token] which will remain valid for a short period of time. A child account will be able to access a white-labeled section by using the following url pattern => https:/email.mydomain.com/login/sso?token=[token]
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getSsoToken0 
  :: ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> SendinBlueRequest GetSsoToken0 MimeNoContent GetSsoToken MimeJSON
getSsoToken0 (ChildAuthKey childAuthKey) =
  _mkRequest "GET" ["/reseller/children/",toPath childAuthKey,"/auth"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetSsoToken0  

-- | @application/json@
instance Consumes GetSsoToken0 MimeJSON

-- | @application/json@
instance Produces GetSsoToken0 MimeJSON


-- *** removeCredits0

-- | @POST \/reseller\/children\/{childAuthKey}\/credits\/remove@
-- 
-- Remove Email and/or SMS credits from a specific child account
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
removeCredits0 
  :: (Consumes RemoveCredits0 MimeJSON, MimeRender MimeJSON RemoveCredits)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> RemoveCredits -- ^ "removeCredits" -  Values to post to remove email or SMS credits from a specific child account
  -> SendinBlueRequest RemoveCredits0 MimeJSON RemainingCreditModel MimeJSON
removeCredits0 (ChildAuthKey childAuthKey) removeCredits =
  _mkRequest "POST" ["/reseller/children/",toPath childAuthKey,"/credits/remove"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` removeCredits

data RemoveCredits0 

-- | /Body Param/ "removeCredits" - Values to post to remove email or SMS credits from a specific child account
instance HasBodyParam RemoveCredits0 RemoveCredits 

-- | @application/json@
instance Consumes RemoveCredits0 MimeJSON

-- | @application/json@
instance Produces RemoveCredits0 MimeJSON


-- *** updateChildAccountStatus0

-- | @PUT \/reseller\/children\/{childAuthKey}\/accountStatus@
-- 
-- Updates infos of reseller's child account status based on the childAuthKey supplied
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
updateChildAccountStatus0 
  :: (Consumes UpdateChildAccountStatus0 MimeJSON, MimeRender MimeJSON UpdateChildAccountStatus)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> UpdateChildAccountStatus -- ^ "updateChildAccountStatus" -  values to update in child account status
  -> SendinBlueRequest UpdateChildAccountStatus0 MimeJSON res MimeJSON
updateChildAccountStatus0 (ChildAuthKey childAuthKey) updateChildAccountStatus =
  _mkRequest "PUT" ["/reseller/children/",toPath childAuthKey,"/accountStatus"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` updateChildAccountStatus

data UpdateChildAccountStatus0 

-- | /Body Param/ "updateChildAccountStatus" - values to update in child account status
instance HasBodyParam UpdateChildAccountStatus0 UpdateChildAccountStatus 

-- | @application/json@
instance Consumes UpdateChildAccountStatus0 MimeJSON

-- | @application/json@
instance Produces UpdateChildAccountStatus0 MimeJSON


-- *** updateChildDomain0

-- | @PUT \/reseller\/children\/{childAuthKey}\/domains\/{domainName}@
-- 
-- Updates the sender domain of reseller's child based on the childAuthKey and domainName passed
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
updateChildDomain0 
  :: (Consumes UpdateChildDomain0 MimeJSON, MimeRender MimeJSON UpdateChildDomain)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> DomainName -- ^ "domainName" -  Pass the existing domain that needs to be updated
  -> UpdateChildDomain -- ^ "updateChildDomain" -  value to update for sender domain
  -> SendinBlueRequest UpdateChildDomain0 MimeJSON res MimeJSON
updateChildDomain0 (ChildAuthKey childAuthKey) (DomainName domainName) updateChildDomain =
  _mkRequest "PUT" ["/reseller/children/",toPath childAuthKey,"/domains/",toPath domainName]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` updateChildDomain

data UpdateChildDomain0 

-- | /Body Param/ "updateChildDomain" - value to update for sender domain
instance HasBodyParam UpdateChildDomain0 UpdateChildDomain 

-- | @application/json@
instance Consumes UpdateChildDomain0 MimeJSON

-- | @application/json@
instance Produces UpdateChildDomain0 MimeJSON


-- *** updateResellerChild

-- | @PUT \/reseller\/children\/{childAuthKey}@
-- 
-- Updates infos of reseller's child based on the childAuthKey supplied
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
updateResellerChild 
  :: (Consumes UpdateResellerChild MimeJSON, MimeRender MimeJSON UpdateChild)
  => ChildAuthKey -- ^ "childAuthKey" -  auth key of reseller's child
  -> UpdateChild -- ^ "resellerChild" -  values to update in child profile
  -> SendinBlueRequest UpdateResellerChild MimeJSON res MimeJSON
updateResellerChild (ChildAuthKey childAuthKey) resellerChild =
  _mkRequest "PUT" ["/reseller/children/",toPath childAuthKey]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` resellerChild

data UpdateResellerChild 

-- | /Body Param/ "resellerChild" - values to update in child profile
instance HasBodyParam UpdateResellerChild UpdateChild 

-- | @application/json@
instance Consumes UpdateResellerChild MimeJSON

-- | @application/json@
instance Produces UpdateResellerChild MimeJSON

