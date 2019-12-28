{-
   SendinBlue API

   SendinBlue provide a RESTFul API that can be used with any languages. With this API, you will be able to :   - Manage your campaigns and get the statistics   - Manage your contacts   - Send transactional Emails and SMS   - and much more...  You can download our wrappers at https://github.com/orgs/sendinblue  **Possible responses**   | Code | Message |   | :-------------: | ------------- |   | 200  | OK. Successful Request  |   | 201  | OK. Successful Creation |   | 202  | OK. Request accepted |   | 204  | OK. Successful Update/Deletion  |   | 400  | Error. Bad Request  |   | 401  | Error. Authentication Needed  |   | 402  | Error. Not enough credit, plan upgrade needed  |   | 403  | Error. Permission denied  |   | 404  | Error. Object does not exist |   | 405  | Error. Method not allowed  |   | 406  | Error. Not Acceptable  | 

   OpenAPI spec version: 2.0
   SendinBlue API API version: 3.0.0
   Contact: contact@sendinblue.com
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : SendinBlue.API
-}

module SendinBlue.API
  ( module SendinBlue.API.Account
  , module SendinBlue.API.Attributes
  , module SendinBlue.API.Contacts
  , module SendinBlue.API.EmailCampaigns
  , module SendinBlue.API.Folders
  , module SendinBlue.API.Lists
  , module SendinBlue.API.Process
  , module SendinBlue.API.Reseller
  , module SendinBlue.API.SMSCampaigns
  , module SendinBlue.API.SMTP
  , module SendinBlue.API.Senders
  , module SendinBlue.API.TransactionalSMS
  , module SendinBlue.API.Webhooks
  ) where

import SendinBlue.API.Account
import SendinBlue.API.Attributes
import SendinBlue.API.Contacts
import SendinBlue.API.EmailCampaigns
import SendinBlue.API.Folders
import SendinBlue.API.Lists
import SendinBlue.API.Process
import SendinBlue.API.Reseller
import SendinBlue.API.SMSCampaigns
import SendinBlue.API.SMTP
import SendinBlue.API.Senders
import SendinBlue.API.TransactionalSMS
import SendinBlue.API.Webhooks