{-
   SendinBlue API

   SendinBlue provide a RESTFul API that can be used with any languages. With this API, you will be able to :   - Manage your campaigns and get the statistics   - Manage your contacts   - Send transactional Emails and SMS   - and much more...  You can download our wrappers at https://github.com/orgs/sendinblue  **Possible responses**   | Code | Message |   | :-------------: | ------------- |   | 200  | OK. Successful Request  |   | 201  | OK. Successful Creation |   | 202  | OK. Request accepted |   | 204  | OK. Successful Update/Deletion  |   | 400  | Error. Bad Request  |   | 401  | Error. Authentication Needed  |   | 402  | Error. Not enough credit, plan upgrade needed  |   | 403  | Error. Permission denied  |   | 404  | Error. Object does not exist |   | 405  | Error. Method not allowed  |   | 406  | Error. Not Acceptable  | 

   OpenAPI spec version: 2.0
   SendinBlue API API version: 3.0.0
   Contact: contact@sendinblue.com
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : SendinBlue.API.Folders
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SendinBlue.API.Folders where

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


-- ** Folders

-- *** createFolder0

-- | @POST \/contacts\/folders@
-- 
-- Create a folder
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
createFolder0 
  :: (Consumes CreateFolder0 MimeJSON, MimeRender MimeJSON CreateUpdateFolder)
  => CreateUpdateFolder -- ^ "createFolder" -  Name of the folder
  -> SendinBlueRequest CreateFolder0 MimeJSON CreateModel MimeJSON
createFolder0 createFolder =
  _mkRequest "POST" ["/contacts/folders"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` createFolder

data CreateFolder0 

-- | /Body Param/ "createFolder" - Name of the folder
instance HasBodyParam CreateFolder0 CreateUpdateFolder 

-- | @application/json@
instance Consumes CreateFolder0 MimeJSON

-- | @application/json@
instance Produces CreateFolder0 MimeJSON


-- *** deleteFolder0

-- | @DELETE \/contacts\/folders\/{folderId}@
-- 
-- Delete a folder (and all its lists)
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
deleteFolder0 
  :: FolderId -- ^ "folderId" -  Id of the folder
  -> SendinBlueRequest DeleteFolder0 MimeNoContent res MimeJSON
deleteFolder0 (FolderId folderId) =
  _mkRequest "DELETE" ["/contacts/folders/",toPath folderId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data DeleteFolder0  

-- | @application/json@
instance Consumes DeleteFolder0 MimeJSON

-- | @application/json@
instance Produces DeleteFolder0 MimeJSON


-- *** getFolder1

-- | @GET \/contacts\/folders\/{folderId}@
-- 
-- Returns folder details
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getFolder1 
  :: FolderId -- ^ "folderId" -  id of the folder
  -> SendinBlueRequest GetFolder1 MimeNoContent GetFolder MimeJSON
getFolder1 (FolderId folderId) =
  _mkRequest "GET" ["/contacts/folders/",toPath folderId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetFolder1  

-- | @application/json@
instance Consumes GetFolder1 MimeJSON

-- | @application/json@
instance Produces GetFolder1 MimeJSON


-- *** getFolderLists1

-- | @GET \/contacts\/folders\/{folderId}\/lists@
-- 
-- Get the lists in a folder
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getFolderLists1 
  :: FolderId -- ^ "folderId" -  Id of the folder
  -> SendinBlueRequest GetFolderLists1 MimeNoContent GetFolderLists MimeJSON
getFolderLists1 (FolderId folderId) =
  _mkRequest "GET" ["/contacts/folders/",toPath folderId,"/lists"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)

data GetFolderLists1  

-- | /Optional Param/ "limit" - Number of documents per page
instance HasOptionalParam GetFolderLists1 Limit where
  applyOptionalParam req (Limit xs) =
    req `setQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "offset" - Index of the first document of the page
instance HasOptionalParam GetFolderLists1 Offset where
  applyOptionalParam req (Offset xs) =
    req `setQuery` toQuery ("offset", Just xs)

-- | @application/json@
instance Consumes GetFolderLists1 MimeJSON

-- | @application/json@
instance Produces GetFolderLists1 MimeJSON


-- *** getFolders1

-- | @GET \/contacts\/folders@
-- 
-- Get all the folders
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
getFolders1 
  :: Limit -- ^ "limit" -  Number of documents per page
  -> Offset -- ^ "offset" -  Index of the first document of the page
  -> SendinBlueRequest GetFolders1 MimeNoContent GetFolders MimeJSON
getFolders1 (Limit limit) (Offset offset) =
  _mkRequest "GET" ["/contacts/folders"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("limit", Just limit)
    `setQuery` toQuery ("offset", Just offset)

data GetFolders1  

-- | @application/json@
instance Consumes GetFolders1 MimeJSON

-- | @application/json@
instance Produces GetFolders1 MimeJSON


-- *** updateFolder0

-- | @PUT \/contacts\/folders\/{folderId}@
-- 
-- Update a contact folder
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
-- Note: Has 'Produces' instances, but no response schema
-- 
updateFolder0 
  :: (Consumes UpdateFolder0 MimeJSON, MimeRender MimeJSON CreateUpdateFolder)
  => FolderId -- ^ "folderId" -  Id of the folder
  -> CreateUpdateFolder -- ^ "updateFolder" -  Name of the folder
  -> SendinBlueRequest UpdateFolder0 MimeJSON res MimeJSON
updateFolder0 (FolderId folderId) updateFolder =
  _mkRequest "PUT" ["/contacts/folders/",toPath folderId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setBodyParam` updateFolder

data UpdateFolder0 

-- | /Body Param/ "updateFolder" - Name of the folder
instance HasBodyParam UpdateFolder0 CreateUpdateFolder 

-- | @application/json@
instance Consumes UpdateFolder0 MimeJSON

-- | @application/json@
instance Produces UpdateFolder0 MimeJSON

