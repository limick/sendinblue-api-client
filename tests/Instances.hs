{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import SendinBlue.Model
import SendinBlue.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary AddChildDomain where
  arbitrary =
    AddChildDomain
      <$> arbitrary -- addChildDomainDomain :: Maybe Text
    
instance Arbitrary AddContactToList where
  arbitrary =
    AddContactToList
      <$> arbitrary -- addContactToListEmails :: Maybe [Text]
    
instance Arbitrary AddCredits where
  arbitrary =
    AddCredits
      <$> arbitrary -- addCreditsSms :: Maybe Integer
      <*> arbitrary -- addCreditsEmail :: Maybe Integer
    
instance Arbitrary CreateAttribute where
  arbitrary =
    CreateAttribute
      <$> arbitrary -- createAttributeValue :: Maybe Text
      <*> arbitrary -- createAttributeEnumeration :: Maybe [CreateAttributeEnumeration]
      <*> arbitrary -- createAttributeType :: Maybe Text
    
instance Arbitrary CreateAttributeEnumeration where
  arbitrary =
    CreateAttributeEnumeration
      <$> arbitrary -- createAttributeEnumerationValue :: Int
      <*> arbitrary -- createAttributeEnumerationLabel :: Text
    
instance Arbitrary CreateChild where
  arbitrary =
    CreateChild
      <$> arbitrary -- createChildEmail :: Text
      <*> arbitrary -- createChildFirstName :: Text
      <*> arbitrary -- createChildLastName :: Text
      <*> arbitrary -- createChildCompanyName :: Text
      <*> arbitrary -- createChildPassword :: Text
    
instance Arbitrary CreateContact where
  arbitrary =
    CreateContact
      <$> arbitrary -- createContactEmail :: Maybe Text
      <*> arbitrary -- createContactAttributes :: Maybe A.Value
      <*> arbitrary -- createContactEmailBlacklisted :: Maybe Bool
      <*> arbitrary -- createContactSmsBlacklisted :: Maybe Bool
      <*> arbitrary -- createContactListIds :: Maybe [Integer]
      <*> arbitrary -- createContactUpdateEnabled :: Maybe Bool
      <*> arbitrary -- createContactSmtpBlacklistSender :: Maybe [Text]
    
instance Arbitrary CreateEmailCampaign where
  arbitrary =
    CreateEmailCampaign
      <$> arbitrary -- createEmailCampaignTag :: Maybe Text
      <*> arbitrary -- createEmailCampaignSender :: CreateEmailCampaignSender
      <*> arbitrary -- createEmailCampaignName :: Text
      <*> arbitrary -- createEmailCampaignHtmlContent :: Maybe Text
      <*> arbitrary -- createEmailCampaignHtmlUrl :: Maybe Text
      <*> arbitrary -- createEmailCampaignTemplateId :: Maybe Integer
      <*> arbitrary -- createEmailCampaignScheduledAt :: Maybe DateTime
      <*> arbitrary -- createEmailCampaignSubject :: Maybe Text
      <*> arbitrary -- createEmailCampaignReplyTo :: Maybe Text
      <*> arbitrary -- createEmailCampaignToField :: Maybe Text
      <*> arbitrary -- createEmailCampaignRecipients :: Maybe CreateEmailCampaignRecipients
      <*> arbitrary -- createEmailCampaignAttachmentUrl :: Maybe Text
      <*> arbitrary -- createEmailCampaignInlineImageActivation :: Maybe Bool
      <*> arbitrary -- createEmailCampaignMirrorActive :: Maybe Bool
      <*> arbitrary -- createEmailCampaignFooter :: Maybe Text
      <*> arbitrary -- createEmailCampaignHeader :: Maybe Text
      <*> arbitrary -- createEmailCampaignUtmCampaign :: Maybe Text
      <*> arbitrary -- createEmailCampaignParams :: Maybe A.Value
      <*> arbitrary -- createEmailCampaignSendAtBestTime :: Maybe Bool
      <*> arbitrary -- createEmailCampaignAbTesting :: Maybe Bool
      <*> arbitrary -- createEmailCampaignSubjectA :: Maybe Text
      <*> arbitrary -- createEmailCampaignSubjectB :: Maybe Text
      <*> arbitrary -- createEmailCampaignSplitRule :: Maybe Integer
      <*> arbitrary -- createEmailCampaignWinnerCriteria :: Maybe Text
      <*> arbitrary -- createEmailCampaignWinnerDelay :: Maybe Integer
      <*> arbitrary -- createEmailCampaignIpWarmupEnable :: Maybe Bool
      <*> arbitrary -- createEmailCampaignInitialQuota :: Maybe Integer
      <*> arbitrary -- createEmailCampaignIncreaseRate :: Maybe Integer
    
instance Arbitrary CreateEmailCampaignRecipients where
  arbitrary =
    CreateEmailCampaignRecipients
      <$> arbitrary -- createEmailCampaignRecipientsExclusionListIds :: Maybe [Integer]
      <*> arbitrary -- createEmailCampaignRecipientsListIds :: Maybe [Integer]
    
instance Arbitrary CreateEmailCampaignSender where
  arbitrary =
    CreateEmailCampaignSender
      <$> arbitrary -- createEmailCampaignSenderName :: Maybe Text
      <*> arbitrary -- createEmailCampaignSenderEmail :: Maybe Text
      <*> arbitrary -- createEmailCampaignSenderId :: Maybe Integer
    
instance Arbitrary CreateList where
  arbitrary =
    CreateList
      <$> arbitrary -- createListName :: Text
      <*> arbitrary -- createListFolderId :: Integer
    
instance Arbitrary CreateModel where
  arbitrary =
    CreateModel
      <$> arbitrary -- createModelId :: Integer
    
instance Arbitrary CreateReseller where
  arbitrary =
    CreateReseller
      <$> arbitrary -- createResellerAuthKey :: Text
    
instance Arbitrary CreateSender where
  arbitrary =
    CreateSender
      <$> arbitrary -- createSenderName :: Text
      <*> arbitrary -- createSenderEmail :: Text
      <*> arbitrary -- createSenderIps :: Maybe [CreateSenderIps]
    
instance Arbitrary CreateSenderIps where
  arbitrary =
    CreateSenderIps
      <$> arbitrary -- createSenderIpsIp :: Text
      <*> arbitrary -- createSenderIpsDomain :: Text
      <*> arbitrary -- createSenderIpsWeight :: Maybe Integer
    
instance Arbitrary CreateSenderModel where
  arbitrary =
    CreateSenderModel
      <$> arbitrary -- createSenderModelId :: Integer
      <*> arbitrary -- createSenderModelSpfError :: Maybe Bool
      <*> arbitrary -- createSenderModelDkimError :: Maybe Bool
    
instance Arbitrary CreateSmsCampaign where
  arbitrary =
    CreateSmsCampaign
      <$> arbitrary -- createSmsCampaignName :: Text
      <*> arbitrary -- createSmsCampaignSender :: Text
      <*> arbitrary -- createSmsCampaignContent :: Text
      <*> arbitrary -- createSmsCampaignRecipients :: Maybe CreateSmsCampaignRecipients
      <*> arbitrary -- createSmsCampaignScheduledAt :: Maybe DateTime
    
instance Arbitrary CreateSmsCampaignRecipients where
  arbitrary =
    CreateSmsCampaignRecipients
      <$> arbitrary -- createSmsCampaignRecipientsListIds :: [Integer]
      <*> arbitrary -- createSmsCampaignRecipientsExclusionListIds :: Maybe [Integer]
    
instance Arbitrary CreateSmtpEmail where
  arbitrary =
    CreateSmtpEmail
      <$> arbitrary -- createSmtpEmailMessageId :: Text
    
instance Arbitrary CreateSmtpTemplate where
  arbitrary =
    CreateSmtpTemplate
      <$> arbitrary -- createSmtpTemplateTag :: Maybe Text
      <*> arbitrary -- createSmtpTemplateSender :: CreateSmtpTemplateSender
      <*> arbitrary -- createSmtpTemplateTemplateName :: Text
      <*> arbitrary -- createSmtpTemplateHtmlContent :: Maybe Text
      <*> arbitrary -- createSmtpTemplateHtmlUrl :: Maybe Text
      <*> arbitrary -- createSmtpTemplateSubject :: Text
      <*> arbitrary -- createSmtpTemplateReplyTo :: Maybe Text
      <*> arbitrary -- createSmtpTemplateToField :: Maybe Text
      <*> arbitrary -- createSmtpTemplateAttachmentUrl :: Maybe Text
      <*> arbitrary -- createSmtpTemplateIsActive :: Maybe Bool
    
instance Arbitrary CreateSmtpTemplateSender where
  arbitrary =
    CreateSmtpTemplateSender
      <$> arbitrary -- createSmtpTemplateSenderName :: Maybe Text
      <*> arbitrary -- createSmtpTemplateSenderEmail :: Maybe Text
      <*> arbitrary -- createSmtpTemplateSenderId :: Maybe Integer
    
instance Arbitrary CreateUpdateContactModel where
  arbitrary =
    CreateUpdateContactModel
      <$> arbitrary -- createUpdateContactModelId :: Maybe Integer
    
instance Arbitrary CreateUpdateFolder where
  arbitrary =
    CreateUpdateFolder
      <$> arbitrary -- createUpdateFolderName :: Maybe Text
    
instance Arbitrary CreateWebhook where
  arbitrary =
    CreateWebhook
      <$> arbitrary -- createWebhookUrl :: Text
      <*> arbitrary -- createWebhookDescription :: Maybe Text
      <*> arbitrary -- createWebhookEvents :: [Text]
      <*> arbitrary -- createWebhookType :: Maybe Text
    
instance Arbitrary CreatedProcessId where
  arbitrary =
    CreatedProcessId
      <$> arbitrary -- createdProcessIdProcessId :: Integer
    
instance Arbitrary DeleteHardbounces where
  arbitrary =
    DeleteHardbounces
      <$> arbitrary -- deleteHardbouncesStartDate :: Maybe Text
      <*> arbitrary -- deleteHardbouncesEndDate :: Maybe Text
      <*> arbitrary -- deleteHardbouncesContactEmail :: Maybe Text
    
instance Arbitrary EmailExportRecipients where
  arbitrary =
    EmailExportRecipients
      <$> arbitrary -- emailExportRecipientsNotifyUrl :: Maybe Text
      <*> arbitrary -- emailExportRecipientsRecipientsType :: Text
    
instance Arbitrary ErrorModel where
  arbitrary =
    ErrorModel
      <$> arbitrary -- errorModelCode :: Text
      <*> arbitrary -- errorModelMessage :: Text
    
instance Arbitrary GetAccountMarketingAutomation where
  arbitrary =
    GetAccountMarketingAutomation
      <$> arbitrary -- getAccountMarketingAutomationKey :: Maybe Text
      <*> arbitrary -- getAccountMarketingAutomationEnabled :: Bool
    
instance Arbitrary GetAccountPlan where
  arbitrary =
    GetAccountPlan
      <$> arbitrary -- getAccountPlanType :: Text
      <*> arbitrary -- getAccountPlanCreditsType :: Text
      <*> arbitrary -- getAccountPlanCredits :: Float
      <*> arbitrary -- getAccountPlanStartDate :: Maybe Date
      <*> arbitrary -- getAccountPlanEndDate :: Maybe Date
      <*> arbitrary -- getAccountPlanUserLimit :: Maybe Int
    
instance Arbitrary GetAccountRelay where
  arbitrary =
    GetAccountRelay
      <$> arbitrary -- getAccountRelayEnabled :: Bool
      <*> arbitrary -- getAccountRelayData :: GetAccountRelayData
    
instance Arbitrary GetAccountRelayData where
  arbitrary =
    GetAccountRelayData
      <$> arbitrary -- getAccountRelayDataUserName :: Text
      <*> arbitrary -- getAccountRelayDataRelay :: Text
      <*> arbitrary -- getAccountRelayDataPort :: Int
    
instance Arbitrary GetAggregatedReport where
  arbitrary =
    GetAggregatedReport
      <$> arbitrary -- getAggregatedReportRange :: Text
      <*> arbitrary -- getAggregatedReportRequests :: Integer
      <*> arbitrary -- getAggregatedReportDelivered :: Integer
      <*> arbitrary -- getAggregatedReportHardBounces :: Integer
      <*> arbitrary -- getAggregatedReportSoftBounces :: Integer
      <*> arbitrary -- getAggregatedReportClicks :: Integer
      <*> arbitrary -- getAggregatedReportUniqueClicks :: Integer
      <*> arbitrary -- getAggregatedReportOpens :: Integer
      <*> arbitrary -- getAggregatedReportUniqueOpens :: Integer
      <*> arbitrary -- getAggregatedReportSpamReports :: Integer
      <*> arbitrary -- getAggregatedReportBlocked :: Integer
      <*> arbitrary -- getAggregatedReportInvalid :: Integer
      <*> arbitrary -- getAggregatedReportUnsubscribed :: Integer
    
instance Arbitrary GetAttributes where
  arbitrary =
    GetAttributes
      <$> arbitrary -- getAttributesAttributes :: [GetAttributesAttributes]
    
instance Arbitrary GetAttributesAttributes where
  arbitrary =
    GetAttributesAttributes
      <$> arbitrary -- getAttributesAttributesName :: Text
      <*> arbitrary -- getAttributesAttributesCategory :: Text
      <*> arbitrary -- getAttributesAttributesType :: Maybe Text
      <*> arbitrary -- getAttributesAttributesEnumeration :: Maybe [GetAttributesEnumeration]
      <*> arbitrary -- getAttributesAttributesCalculatedValue :: Maybe Text
    
instance Arbitrary GetAttributesEnumeration where
  arbitrary =
    GetAttributesEnumeration
      <$> arbitrary -- getAttributesEnumerationValue :: Integer
      <*> arbitrary -- getAttributesEnumerationLabel :: Text
    
instance Arbitrary GetCampaignOverview where
  arbitrary =
    GetCampaignOverview
      <$> arbitrary -- getCampaignOverviewId :: Integer
      <*> arbitrary -- getCampaignOverviewName :: Text
      <*> arbitrary -- getCampaignOverviewSubject :: Maybe Text
      <*> arbitrary -- getCampaignOverviewType :: Text
      <*> arbitrary -- getCampaignOverviewStatus :: Text
      <*> arbitrary -- getCampaignOverviewScheduledAt :: Maybe DateTime
      <*> arbitrary -- getCampaignOverviewAbTesting :: Maybe Bool
      <*> arbitrary -- getCampaignOverviewSubjectA :: Maybe Text
      <*> arbitrary -- getCampaignOverviewSubjectB :: Maybe Text
      <*> arbitrary -- getCampaignOverviewSplitRule :: Maybe Int
      <*> arbitrary -- getCampaignOverviewWinnerCriteria :: Maybe Text
      <*> arbitrary -- getCampaignOverviewWinnerDelay :: Maybe Int
      <*> arbitrary -- getCampaignOverviewSendAtBestTime :: Maybe Bool
    
instance Arbitrary GetCampaignRecipients where
  arbitrary =
    GetCampaignRecipients
      <$> arbitrary -- getCampaignRecipientsLists :: [Integer]
      <*> arbitrary -- getCampaignRecipientsExclusionLists :: [Integer]
    
instance Arbitrary GetCampaignStats where
  arbitrary =
    GetCampaignStats
      <$> arbitrary -- getCampaignStatsListId :: Maybe Integer
      <*> arbitrary -- getCampaignStatsUniqueClicks :: Integer
      <*> arbitrary -- getCampaignStatsClickers :: Integer
      <*> arbitrary -- getCampaignStatsComplaints :: Integer
      <*> arbitrary -- getCampaignStatsDelivered :: Integer
      <*> arbitrary -- getCampaignStatsSent :: Integer
      <*> arbitrary -- getCampaignStatsSoftBounces :: Integer
      <*> arbitrary -- getCampaignStatsHardBounces :: Integer
      <*> arbitrary -- getCampaignStatsUniqueViews :: Integer
      <*> arbitrary -- getCampaignStatsUnsubscriptions :: Integer
      <*> arbitrary -- getCampaignStatsViewed :: Integer
      <*> arbitrary -- getCampaignStatsDeferred :: Maybe Integer
    
instance Arbitrary GetChildAccountCreationStatus where
  arbitrary =
    GetChildAccountCreationStatus
      <$> arbitrary -- getChildAccountCreationStatusChildAccountCreated :: Bool
    
instance Arbitrary GetChildDomain where
  arbitrary =
    GetChildDomain
      <$> arbitrary -- getChildDomainDomain :: Text
      <*> arbitrary -- getChildDomainActive :: Bool
    
instance Arbitrary GetChildDomains where
  arbitrary =
    
    pure GetChildDomains
     
instance Arbitrary GetChildInfoApiKeys where
  arbitrary =
    GetChildInfoApiKeys
      <$> arbitrary -- getChildInfoApiKeysV2 :: [GetChildInfoApiKeysV2]
      <*> arbitrary -- getChildInfoApiKeysV3 :: Maybe [GetChildInfoApiKeysV3]
    
instance Arbitrary GetChildInfoApiKeysV2 where
  arbitrary =
    GetChildInfoApiKeysV2
      <$> arbitrary -- getChildInfoApiKeysV2Name :: Text
      <*> arbitrary -- getChildInfoApiKeysV2Key :: Text
    
instance Arbitrary GetChildInfoApiKeysV3 where
  arbitrary =
    GetChildInfoApiKeysV3
      <$> arbitrary -- getChildInfoApiKeysV3Name :: Text
      <*> arbitrary -- getChildInfoApiKeysV3Key :: Text
    
instance Arbitrary GetChildInfoCredits where
  arbitrary =
    GetChildInfoCredits
      <$> arbitrary -- getChildInfoCreditsEmailCredits :: Maybe Integer
      <*> arbitrary -- getChildInfoCreditsSmsCredits :: Maybe Integer
    
instance Arbitrary GetChildInfoStatistics where
  arbitrary =
    GetChildInfoStatistics
      <$> arbitrary -- getChildInfoStatisticsPreviousMonthTotalSent :: Maybe Integer
      <*> arbitrary -- getChildInfoStatisticsCurrentMonthTotalSent :: Maybe Integer
      <*> arbitrary -- getChildInfoStatisticsTotalSent :: Maybe Integer
    
instance Arbitrary GetChildrenList where
  arbitrary =
    GetChildrenList
      <$> arbitrary -- getChildrenListChildren :: Maybe [A.Value]
      <*> arbitrary -- getChildrenListCount :: Integer
    
instance Arbitrary GetClient where
  arbitrary =
    GetClient
      <$> arbitrary -- getClientEmail :: Text
      <*> arbitrary -- getClientFirstName :: Text
      <*> arbitrary -- getClientLastName :: Text
      <*> arbitrary -- getClientCompanyName :: Text
    
instance Arbitrary GetContactCampaignStats where
  arbitrary =
    GetContactCampaignStats
      <$> arbitrary -- getContactCampaignStatsMessagesSent :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getContactCampaignStatsHardBounces :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getContactCampaignStatsSoftBounces :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getContactCampaignStatsComplaints :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getContactCampaignStatsUnsubscriptions :: Maybe GetContactCampaignStatsUnsubscriptions
      <*> arbitrary -- getContactCampaignStatsOpened :: Maybe [GetContactCampaignStatsOpened]
      <*> arbitrary -- getContactCampaignStatsClicked :: Maybe [GetContactCampaignStatsClicked]
      <*> arbitrary -- getContactCampaignStatsTransacAttributes :: Maybe [GetContactCampaignStatsTransacAttributes]
    
instance Arbitrary GetContactCampaignStatsClicked where
  arbitrary =
    GetContactCampaignStatsClicked
      <$> arbitrary -- getContactCampaignStatsClickedCampaignId :: Integer
      <*> arbitrary -- getContactCampaignStatsClickedLinks :: [GetExtendedContactDetailsStatisticsLinks]
    
instance Arbitrary GetContactCampaignStatsOpened where
  arbitrary =
    GetContactCampaignStatsOpened
      <$> arbitrary -- getContactCampaignStatsOpenedCampaignId :: Integer
      <*> arbitrary -- getContactCampaignStatsOpenedCount :: Integer
      <*> arbitrary -- getContactCampaignStatsOpenedEventTime :: DateTime
      <*> arbitrary -- getContactCampaignStatsOpenedIp :: Text
    
instance Arbitrary GetContactCampaignStatsTransacAttributes where
  arbitrary =
    GetContactCampaignStatsTransacAttributes
      <$> arbitrary -- getContactCampaignStatsTransacAttributesOrderDate :: Date
      <*> arbitrary -- getContactCampaignStatsTransacAttributesOrderPrice :: Float
      <*> arbitrary -- getContactCampaignStatsTransacAttributesOrderId :: Integer
    
instance Arbitrary GetContactCampaignStatsUnsubscriptions where
  arbitrary =
    GetContactCampaignStatsUnsubscriptions
      <$> arbitrary -- getContactCampaignStatsUnsubscriptionsUserUnsubscription :: [GetExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscription]
      <*> arbitrary -- getContactCampaignStatsUnsubscriptionsAdminUnsubscription :: [GetExtendedContactDetailsStatisticsUnsubscriptionsAdminUnsubscription]
    
instance Arbitrary GetContactDetails where
  arbitrary =
    GetContactDetails
      <$> arbitrary -- getContactDetailsEmail :: Text
      <*> arbitrary -- getContactDetailsId :: Integer
      <*> arbitrary -- getContactDetailsEmailBlacklisted :: Bool
      <*> arbitrary -- getContactDetailsSmsBlacklisted :: Bool
      <*> arbitrary -- getContactDetailsCreatedAt :: DateTime
      <*> arbitrary -- getContactDetailsModifiedAt :: DateTime
      <*> arbitrary -- getContactDetailsListIds :: [Integer]
      <*> arbitrary -- getContactDetailsListUnsubscribed :: Maybe [Integer]
      <*> arbitrary -- getContactDetailsAttributes :: A.Value
    
instance Arbitrary GetContacts where
  arbitrary =
    GetContacts
      <$> arbitrary -- getContactsContacts :: [A.Value]
      <*> arbitrary -- getContactsCount :: Integer
    
instance Arbitrary GetDeviceBrowserStats where
  arbitrary =
    GetDeviceBrowserStats
      <$> arbitrary -- getDeviceBrowserStatsClickers :: Integer
      <*> arbitrary -- getDeviceBrowserStatsUniqueClicks :: Integer
      <*> arbitrary -- getDeviceBrowserStatsViewed :: Integer
      <*> arbitrary -- getDeviceBrowserStatsUniqueViews :: Integer
    
instance Arbitrary GetEmailCampaigns where
  arbitrary =
    GetEmailCampaigns
      <$> arbitrary -- getEmailCampaignsCampaigns :: Maybe [A.Value]
      <*> arbitrary -- getEmailCampaignsCount :: Integer
    
instance Arbitrary GetEmailEventReport where
  arbitrary =
    GetEmailEventReport
      <$> arbitrary -- getEmailEventReportEvents :: Maybe [GetEmailEventReportEvents]
    
instance Arbitrary GetEmailEventReportEvents where
  arbitrary =
    GetEmailEventReportEvents
      <$> arbitrary -- getEmailEventReportEventsEmail :: Text
      <*> arbitrary -- getEmailEventReportEventsDate :: DateTime
      <*> arbitrary -- getEmailEventReportEventsSubject :: Maybe Text
      <*> arbitrary -- getEmailEventReportEventsMessageId :: Text
      <*> arbitrary -- getEmailEventReportEventsEvent :: Text
      <*> arbitrary -- getEmailEventReportEventsReason :: Maybe Text
      <*> arbitrary -- getEmailEventReportEventsTag :: Maybe Text
      <*> arbitrary -- getEmailEventReportEventsIp :: Maybe Text
      <*> arbitrary -- getEmailEventReportEventsLink :: Maybe Text
      <*> arbitrary -- getEmailEventReportEventsFrom :: Maybe Text
    
instance Arbitrary GetExtendedCampaignOverviewSender where
  arbitrary =
    GetExtendedCampaignOverviewSender
      <$> arbitrary -- getExtendedCampaignOverviewSenderName :: Maybe Text
      <*> arbitrary -- getExtendedCampaignOverviewSenderEmail :: Maybe Text
      <*> arbitrary -- getExtendedCampaignOverviewSenderId :: Maybe Text
    
instance Arbitrary GetExtendedCampaignStats where
  arbitrary =
    GetExtendedCampaignStats
      <$> arbitrary -- getExtendedCampaignStatsGlobalStats :: A.Value
      <*> arbitrary -- getExtendedCampaignStatsCampaignStats :: [A.Value]
      <*> arbitrary -- getExtendedCampaignStatsMirrorClick :: Integer
      <*> arbitrary -- getExtendedCampaignStatsRemaining :: Integer
      <*> arbitrary -- getExtendedCampaignStatsLinksStats :: A.Value
      <*> arbitrary -- getExtendedCampaignStatsStatsByDomain :: GetStatsByDomain
      <*> arbitrary -- getExtendedCampaignStatsStatsByDevice :: GetStatsByDevice
      <*> arbitrary -- getExtendedCampaignStatsStatsByBrowser :: GetStatsByBrowser
    
instance Arbitrary GetExtendedClientAddress where
  arbitrary =
    GetExtendedClientAddress
      <$> arbitrary -- getExtendedClientAddressStreet :: Text
      <*> arbitrary -- getExtendedClientAddressCity :: Text
      <*> arbitrary -- getExtendedClientAddressZipCode :: Text
      <*> arbitrary -- getExtendedClientAddressCountry :: Text
    
instance Arbitrary GetExtendedContactDetailsStatistics where
  arbitrary =
    GetExtendedContactDetailsStatistics
      <$> arbitrary -- getExtendedContactDetailsStatisticsMessagesSent :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getExtendedContactDetailsStatisticsHardBounces :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getExtendedContactDetailsStatisticsSoftBounces :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getExtendedContactDetailsStatisticsComplaints :: Maybe [GetExtendedContactDetailsStatisticsMessagesSent]
      <*> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptions :: Maybe GetExtendedContactDetailsStatisticsUnsubscriptions
      <*> arbitrary -- getExtendedContactDetailsStatisticsOpened :: Maybe [GetExtendedContactDetailsStatisticsOpened]
      <*> arbitrary -- getExtendedContactDetailsStatisticsClicked :: Maybe [GetExtendedContactDetailsStatisticsClicked]
      <*> arbitrary -- getExtendedContactDetailsStatisticsTransacAttributes :: Maybe [A.Value]
    
instance Arbitrary GetExtendedContactDetailsStatisticsClicked where
  arbitrary =
    GetExtendedContactDetailsStatisticsClicked
      <$> arbitrary -- getExtendedContactDetailsStatisticsClickedCampaignId :: Integer
      <*> arbitrary -- getExtendedContactDetailsStatisticsClickedLinks :: [GetExtendedContactDetailsStatisticsLinks]
    
instance Arbitrary GetExtendedContactDetailsStatisticsLinks where
  arbitrary =
    GetExtendedContactDetailsStatisticsLinks
      <$> arbitrary -- getExtendedContactDetailsStatisticsLinksCount :: Integer
      <*> arbitrary -- getExtendedContactDetailsStatisticsLinksEventTime :: DateTime
      <*> arbitrary -- getExtendedContactDetailsStatisticsLinksIp :: Text
      <*> arbitrary -- getExtendedContactDetailsStatisticsLinksUrl :: Text
    
instance Arbitrary GetExtendedContactDetailsStatisticsMessagesSent where
  arbitrary =
    GetExtendedContactDetailsStatisticsMessagesSent
      <$> arbitrary -- getExtendedContactDetailsStatisticsMessagesSentCampaignId :: Integer
      <*> arbitrary -- getExtendedContactDetailsStatisticsMessagesSentEventTime :: DateTime
    
instance Arbitrary GetExtendedContactDetailsStatisticsOpened where
  arbitrary =
    GetExtendedContactDetailsStatisticsOpened
      <$> arbitrary -- getExtendedContactDetailsStatisticsOpenedCampaignId :: Integer
      <*> arbitrary -- getExtendedContactDetailsStatisticsOpenedCount :: Integer
      <*> arbitrary -- getExtendedContactDetailsStatisticsOpenedEventTime :: DateTime
      <*> arbitrary -- getExtendedContactDetailsStatisticsOpenedIp :: Text
    
instance Arbitrary GetExtendedContactDetailsStatisticsUnsubscriptions where
  arbitrary =
    GetExtendedContactDetailsStatisticsUnsubscriptions
      <$> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscription :: [GetExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscription]
      <*> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptionsAdminUnsubscription :: [GetExtendedContactDetailsStatisticsUnsubscriptionsAdminUnsubscription]
    
instance Arbitrary GetExtendedContactDetailsStatisticsUnsubscriptionsAdminUnsubscription where
  arbitrary =
    GetExtendedContactDetailsStatisticsUnsubscriptionsAdminUnsubscription
      <$> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptionsAdminUnsubscriptionEventTime :: DateTime
      <*> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptionsAdminUnsubscriptionIp :: Text
    
instance Arbitrary GetExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscription where
  arbitrary =
    GetExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscription
      <$> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscriptionCampaignId :: Integer
      <*> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscriptionEventTime :: DateTime
      <*> arbitrary -- getExtendedContactDetailsStatisticsUnsubscriptionsUserUnsubscriptionIp :: Text
    
instance Arbitrary GetExtendedListCampaignStats where
  arbitrary =
    GetExtendedListCampaignStats
      <$> arbitrary -- getExtendedListCampaignStatsCampaignId :: Integer
      <*> arbitrary -- getExtendedListCampaignStatsStats :: GetCampaignStats
    
instance Arbitrary GetFolder where
  arbitrary =
    GetFolder
      <$> arbitrary -- getFolderId :: Integer
      <*> arbitrary -- getFolderName :: Text
      <*> arbitrary -- getFolderTotalBlacklisted :: Integer
      <*> arbitrary -- getFolderTotalSubscribers :: Integer
      <*> arbitrary -- getFolderUniqueSubscribers :: Integer
    
instance Arbitrary GetFolderLists where
  arbitrary =
    GetFolderLists
      <$> arbitrary -- getFolderListsLists :: [A.Value]
      <*> arbitrary -- getFolderListsCount :: Integer
    
instance Arbitrary GetFolders where
  arbitrary =
    GetFolders
      <$> arbitrary -- getFoldersFolders :: Maybe [A.Value]
      <*> arbitrary -- getFoldersCount :: Maybe Integer
    
instance Arbitrary GetIp where
  arbitrary =
    GetIp
      <$> arbitrary -- getIpId :: Integer
      <*> arbitrary -- getIpIp :: Text
      <*> arbitrary -- getIpActive :: Bool
      <*> arbitrary -- getIpDomain :: Text
    
instance Arbitrary GetIpFromSender where
  arbitrary =
    GetIpFromSender
      <$> arbitrary -- getIpFromSenderId :: Integer
      <*> arbitrary -- getIpFromSenderIp :: Text
      <*> arbitrary -- getIpFromSenderDomain :: Text
      <*> arbitrary -- getIpFromSenderWeight :: Integer
    
instance Arbitrary GetIps where
  arbitrary =
    GetIps
      <$> arbitrary -- getIpsIps :: [GetIp]
    
instance Arbitrary GetIpsFromSender where
  arbitrary =
    GetIpsFromSender
      <$> arbitrary -- getIpsFromSenderIps :: [GetIpFromSender]
    
instance Arbitrary GetList where
  arbitrary =
    GetList
      <$> arbitrary -- getListId :: Integer
      <*> arbitrary -- getListName :: Text
      <*> arbitrary -- getListTotalBlacklisted :: Integer
      <*> arbitrary -- getListTotalSubscribers :: Integer
    
instance Arbitrary GetLists where
  arbitrary =
    GetLists
      <$> arbitrary -- getListsLists :: [A.Value]
      <*> arbitrary -- getListsCount :: Integer
    
instance Arbitrary GetProcess where
  arbitrary =
    GetProcess
      <$> arbitrary -- getProcessId :: Integer
      <*> arbitrary -- getProcessStatus :: Text
      <*> arbitrary -- getProcessName :: Text
      <*> arbitrary -- getProcessExportUrl :: Maybe Text
    
instance Arbitrary GetProcesses where
  arbitrary =
    GetProcesses
      <$> arbitrary -- getProcessesProcesses :: Maybe [GetProcess]
      <*> arbitrary -- getProcessesCount :: Integer
    
instance Arbitrary GetReports where
  arbitrary =
    GetReports
      <$> arbitrary -- getReportsReports :: Maybe [GetReportsReports]
    
instance Arbitrary GetReportsReports where
  arbitrary =
    GetReportsReports
      <$> arbitrary -- getReportsReportsDate :: Date
      <*> arbitrary -- getReportsReportsRequests :: Integer
      <*> arbitrary -- getReportsReportsDelivered :: Integer
      <*> arbitrary -- getReportsReportsHardBounces :: Integer
      <*> arbitrary -- getReportsReportsSoftBounces :: Integer
      <*> arbitrary -- getReportsReportsClicks :: Integer
      <*> arbitrary -- getReportsReportsUniqueClicks :: Integer
      <*> arbitrary -- getReportsReportsOpens :: Integer
      <*> arbitrary -- getReportsReportsUniqueOpens :: Integer
      <*> arbitrary -- getReportsReportsSpamReports :: Integer
      <*> arbitrary -- getReportsReportsBlocked :: Integer
      <*> arbitrary -- getReportsReportsInvalid :: Integer
      <*> arbitrary -- getReportsReportsUnsubscribed :: Integer
    
instance Arbitrary GetSendersList where
  arbitrary =
    GetSendersList
      <$> arbitrary -- getSendersListSenders :: Maybe [GetSendersListSenders]
    
instance Arbitrary GetSendersListIps where
  arbitrary =
    GetSendersListIps
      <$> arbitrary -- getSendersListIpsIp :: Text
      <*> arbitrary -- getSendersListIpsDomain :: Text
      <*> arbitrary -- getSendersListIpsWeight :: Integer
    
instance Arbitrary GetSendersListSenders where
  arbitrary =
    GetSendersListSenders
      <$> arbitrary -- getSendersListSendersId :: Integer
      <*> arbitrary -- getSendersListSendersName :: Text
      <*> arbitrary -- getSendersListSendersEmail :: Text
      <*> arbitrary -- getSendersListSendersActive :: Bool
      <*> arbitrary -- getSendersListSendersIps :: Maybe [GetSendersListIps]
    
instance Arbitrary GetSharedTemplateUrl where
  arbitrary =
    GetSharedTemplateUrl
      <$> arbitrary -- getSharedTemplateUrlSharedUrl :: Text
    
instance Arbitrary GetSmsCampaignOverview where
  arbitrary =
    GetSmsCampaignOverview
      <$> arbitrary -- getSmsCampaignOverviewId :: Integer
      <*> arbitrary -- getSmsCampaignOverviewName :: Text
      <*> arbitrary -- getSmsCampaignOverviewStatus :: Text
      <*> arbitrary -- getSmsCampaignOverviewContent :: Text
      <*> arbitrary -- getSmsCampaignOverviewScheduledAt :: DateTime
      <*> arbitrary -- getSmsCampaignOverviewSender :: Text
      <*> arbitrary -- getSmsCampaignOverviewCreatedAt :: DateTime
      <*> arbitrary -- getSmsCampaignOverviewModifiedAt :: DateTime
    
instance Arbitrary GetSmsCampaignStats where
  arbitrary =
    GetSmsCampaignStats
      <$> arbitrary -- getSmsCampaignStatsDelivered :: Integer
      <*> arbitrary -- getSmsCampaignStatsSent :: Integer
      <*> arbitrary -- getSmsCampaignStatsProcessing :: Integer
      <*> arbitrary -- getSmsCampaignStatsSoftBounces :: Integer
      <*> arbitrary -- getSmsCampaignStatsHardBounces :: Integer
      <*> arbitrary -- getSmsCampaignStatsUnsubscriptions :: Integer
      <*> arbitrary -- getSmsCampaignStatsAnswered :: Integer
    
instance Arbitrary GetSmsCampaigns where
  arbitrary =
    GetSmsCampaigns
      <$> arbitrary -- getSmsCampaignsCampaigns :: Maybe [A.Value]
      <*> arbitrary -- getSmsCampaignsCount :: Integer
    
instance Arbitrary GetSmsEventReport where
  arbitrary =
    GetSmsEventReport
      <$> arbitrary -- getSmsEventReportEvents :: Maybe [GetSmsEventReportEvents]
    
instance Arbitrary GetSmsEventReportEvents where
  arbitrary =
    GetSmsEventReportEvents
      <$> arbitrary -- getSmsEventReportEventsPhoneNumber :: Text
      <*> arbitrary -- getSmsEventReportEventsDate :: DateTime
      <*> arbitrary -- getSmsEventReportEventsMessageId :: Text
      <*> arbitrary -- getSmsEventReportEventsEvent :: Text
      <*> arbitrary -- getSmsEventReportEventsReason :: Maybe Text
      <*> arbitrary -- getSmsEventReportEventsReply :: Maybe Text
      <*> arbitrary -- getSmsEventReportEventsTag :: Maybe Text
    
instance Arbitrary GetSmtpTemplateOverview where
  arbitrary =
    GetSmtpTemplateOverview
      <$> arbitrary -- getSmtpTemplateOverviewId :: Integer
      <*> arbitrary -- getSmtpTemplateOverviewName :: Text
      <*> arbitrary -- getSmtpTemplateOverviewSubject :: Text
      <*> arbitrary -- getSmtpTemplateOverviewIsActive :: Bool
      <*> arbitrary -- getSmtpTemplateOverviewTestSent :: Bool
      <*> arbitrary -- getSmtpTemplateOverviewSender :: GetSmtpTemplateOverviewSender
      <*> arbitrary -- getSmtpTemplateOverviewReplyTo :: Text
      <*> arbitrary -- getSmtpTemplateOverviewToField :: Text
      <*> arbitrary -- getSmtpTemplateOverviewTag :: Text
      <*> arbitrary -- getSmtpTemplateOverviewHtmlContent :: Text
      <*> arbitrary -- getSmtpTemplateOverviewCreatedAt :: DateTime
      <*> arbitrary -- getSmtpTemplateOverviewModifiedAt :: DateTime
    
instance Arbitrary GetSmtpTemplateOverviewSender where
  arbitrary =
    GetSmtpTemplateOverviewSender
      <$> arbitrary -- getSmtpTemplateOverviewSenderName :: Maybe Text
      <*> arbitrary -- getSmtpTemplateOverviewSenderEmail :: Maybe Text
      <*> arbitrary -- getSmtpTemplateOverviewSenderId :: Maybe Text
    
instance Arbitrary GetSmtpTemplates where
  arbitrary =
    GetSmtpTemplates
      <$> arbitrary -- getSmtpTemplatesCount :: Maybe Integer
      <*> arbitrary -- getSmtpTemplatesTemplates :: Maybe [GetSmtpTemplateOverview]
    
instance Arbitrary GetSsoToken where
  arbitrary =
    GetSsoToken
      <$> arbitrary -- getSsoTokenToken :: Text
    
instance Arbitrary GetStatsByBrowser where
  arbitrary =
    
    pure GetStatsByBrowser
     
instance Arbitrary GetStatsByDevice where
  arbitrary =
    GetStatsByDevice
      <$> arbitrary -- getStatsByDeviceDesktop :: Maybe (Map.Map String GetDeviceBrowserStats)
      <*> arbitrary -- getStatsByDeviceMobile :: Maybe (Map.Map String GetDeviceBrowserStats)
      <*> arbitrary -- getStatsByDeviceTablet :: Maybe (Map.Map String GetDeviceBrowserStats)
      <*> arbitrary -- getStatsByDeviceUnknown :: Maybe (Map.Map String GetDeviceBrowserStats)
    
instance Arbitrary GetStatsByDomain where
  arbitrary =
    
    pure GetStatsByDomain
     
instance Arbitrary GetTransacAggregatedSmsReport where
  arbitrary =
    GetTransacAggregatedSmsReport
      <$> arbitrary -- getTransacAggregatedSmsReportRange :: Text
      <*> arbitrary -- getTransacAggregatedSmsReportRequests :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportDelivered :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportHardBounces :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportSoftBounces :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportBlocked :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportUnsubscribed :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportReplied :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportAccepted :: Integer
      <*> arbitrary -- getTransacAggregatedSmsReportRejected :: Integer
    
instance Arbitrary GetTransacBlockedContacts where
  arbitrary =
    GetTransacBlockedContacts
      <$> arbitrary -- getTransacBlockedContactsCount :: Maybe Integer
      <*> arbitrary -- getTransacBlockedContactsContacts :: Maybe [GetTransacBlockedContactsContacts]
    
instance Arbitrary GetTransacBlockedContactsContacts where
  arbitrary =
    GetTransacBlockedContactsContacts
      <$> arbitrary -- getTransacBlockedContactsContactsEmail :: Text
      <*> arbitrary -- getTransacBlockedContactsContactsSenderEmail :: Text
      <*> arbitrary -- getTransacBlockedContactsContactsReason :: GetTransacBlockedContactsReason
      <*> arbitrary -- getTransacBlockedContactsContactsBlockedAt :: Date
    
instance Arbitrary GetTransacBlockedContactsReason where
  arbitrary =
    GetTransacBlockedContactsReason
      <$> arbitrary -- getTransacBlockedContactsReasonCode :: Maybe Text
      <*> arbitrary -- getTransacBlockedContactsReasonMessage :: Maybe Text
    
instance Arbitrary GetTransacEmailContent where
  arbitrary =
    GetTransacEmailContent
      <$> arbitrary -- getTransacEmailContentEmail :: Text
      <*> arbitrary -- getTransacEmailContentSubject :: Text
      <*> arbitrary -- getTransacEmailContentTemplateId :: Maybe Integer
      <*> arbitrary -- getTransacEmailContentDate :: DateTime
      <*> arbitrary -- getTransacEmailContentEvents :: [GetTransacEmailContentEvents]
      <*> arbitrary -- getTransacEmailContentBody :: Text
      <*> arbitrary -- getTransacEmailContentAttachmentCount :: Integer
    
instance Arbitrary GetTransacEmailContentEvents where
  arbitrary =
    GetTransacEmailContentEvents
      <$> arbitrary -- getTransacEmailContentEventsName :: Text
      <*> arbitrary -- getTransacEmailContentEventsTime :: DateTime
    
instance Arbitrary GetTransacEmailsList where
  arbitrary =
    GetTransacEmailsList
      <$> arbitrary -- getTransacEmailsListTransactionalEmails :: Maybe [GetTransacEmailsListTransactionalEmails]
    
instance Arbitrary GetTransacEmailsListTransactionalEmails where
  arbitrary =
    GetTransacEmailsListTransactionalEmails
      <$> arbitrary -- getTransacEmailsListTransactionalEmailsEmail :: Text
      <*> arbitrary -- getTransacEmailsListTransactionalEmailsSubject :: Text
      <*> arbitrary -- getTransacEmailsListTransactionalEmailsTemplateId :: Maybe Integer
      <*> arbitrary -- getTransacEmailsListTransactionalEmailsMessageId :: Text
      <*> arbitrary -- getTransacEmailsListTransactionalEmailsUuid :: Text
      <*> arbitrary -- getTransacEmailsListTransactionalEmailsDate :: DateTime
    
instance Arbitrary GetTransacSmsReport where
  arbitrary =
    GetTransacSmsReport
      <$> arbitrary -- getTransacSmsReportReports :: Maybe [GetTransacSmsReportReports]
    
instance Arbitrary GetTransacSmsReportReports where
  arbitrary =
    GetTransacSmsReportReports
      <$> arbitrary -- getTransacSmsReportReportsDate :: Date
      <*> arbitrary -- getTransacSmsReportReportsRequests :: Integer
      <*> arbitrary -- getTransacSmsReportReportsDelivered :: Integer
      <*> arbitrary -- getTransacSmsReportReportsHardBounces :: Integer
      <*> arbitrary -- getTransacSmsReportReportsSoftBounces :: Integer
      <*> arbitrary -- getTransacSmsReportReportsBlocked :: Integer
      <*> arbitrary -- getTransacSmsReportReportsUnsubscribed :: Integer
      <*> arbitrary -- getTransacSmsReportReportsReplied :: Integer
      <*> arbitrary -- getTransacSmsReportReportsAccepted :: Integer
      <*> arbitrary -- getTransacSmsReportReportsRejected :: Integer
    
instance Arbitrary GetWebhook where
  arbitrary =
    GetWebhook
      <$> arbitrary -- getWebhookUrl :: Text
      <*> arbitrary -- getWebhookId :: Integer
      <*> arbitrary -- getWebhookDescription :: Text
      <*> arbitrary -- getWebhookEvents :: [Text]
      <*> arbitrary -- getWebhookType :: Text
      <*> arbitrary -- getWebhookCreatedAt :: DateTime
      <*> arbitrary -- getWebhookModifiedAt :: DateTime
    
instance Arbitrary GetWebhooks where
  arbitrary =
    GetWebhooks
      <$> arbitrary -- getWebhooksWebhooks :: [A.Value]
    
instance Arbitrary ManageIp where
  arbitrary =
    ManageIp
      <$> arbitrary -- manageIpIp :: Maybe Text
    
instance Arbitrary PostContactInfo where
  arbitrary =
    PostContactInfo
      <$> arbitrary -- postContactInfoContacts :: PostContactInfoContacts
    
instance Arbitrary PostContactInfoContacts where
  arbitrary =
    PostContactInfoContacts
      <$> arbitrary -- postContactInfoContactsSuccess :: Maybe [Text]
      <*> arbitrary -- postContactInfoContactsFailure :: Maybe [Text]
      <*> arbitrary -- postContactInfoContactsTotal :: Maybe Integer
    
instance Arbitrary PostSendFailed where
  arbitrary =
    PostSendFailed
      <$> arbitrary -- postSendFailedCode :: Integer
      <*> arbitrary -- postSendFailedMessage :: Text
      <*> arbitrary -- postSendFailedUnexistingEmails :: Maybe [Text]
      <*> arbitrary -- postSendFailedWithoutListEmails :: Maybe [Text]
      <*> arbitrary -- postSendFailedBlackListedEmails :: Maybe [Text]
    
instance Arbitrary PostSendSmsTestFailed where
  arbitrary =
    PostSendSmsTestFailed
      <$> arbitrary -- postSendSmsTestFailedCode :: Integer
      <*> arbitrary -- postSendSmsTestFailedMessage :: Text
      <*> arbitrary -- postSendSmsTestFailedUnexistingSms :: Maybe [Text]
      <*> arbitrary -- postSendSmsTestFailedWithoutListSms :: Maybe [Text]
    
instance Arbitrary RemainingCreditModel where
  arbitrary =
    RemainingCreditModel
      <$> arbitrary -- remainingCreditModelChild :: RemainingCreditModelChild
      <*> arbitrary -- remainingCreditModelReseller :: RemainingCreditModelReseller
    
instance Arbitrary RemainingCreditModelChild where
  arbitrary =
    RemainingCreditModelChild
      <$> arbitrary -- remainingCreditModelChildSms :: Integer
      <*> arbitrary -- remainingCreditModelChildEmail :: Integer
    
instance Arbitrary RemainingCreditModelReseller where
  arbitrary =
    RemainingCreditModelReseller
      <$> arbitrary -- remainingCreditModelResellerSms :: Integer
      <*> arbitrary -- remainingCreditModelResellerEmail :: Integer
    
instance Arbitrary RemoveContactFromList where
  arbitrary =
    RemoveContactFromList
      <$> arbitrary -- removeContactFromListEmails :: Maybe [Text]
      <*> arbitrary -- removeContactFromListAll :: Maybe Bool
    
instance Arbitrary RemoveCredits where
  arbitrary =
    RemoveCredits
      <$> arbitrary -- removeCreditsSms :: Maybe Integer
      <*> arbitrary -- removeCreditsEmail :: Maybe Integer
    
instance Arbitrary RequestContactExport where
  arbitrary =
    RequestContactExport
      <$> arbitrary -- requestContactExportExportAttributes :: Maybe [Text]
      <*> arbitrary -- requestContactExportContactFilter :: A.Value
      <*> arbitrary -- requestContactExportNotifyUrl :: Maybe Text
    
instance Arbitrary RequestContactImport where
  arbitrary =
    RequestContactImport
      <$> arbitrary -- requestContactImportFileUrl :: Maybe Text
      <*> arbitrary -- requestContactImportFileBody :: Maybe Text
      <*> arbitrary -- requestContactImportListIds :: Maybe [Integer]
      <*> arbitrary -- requestContactImportNotifyUrl :: Maybe Text
      <*> arbitrary -- requestContactImportNewList :: Maybe RequestContactImportNewList
      <*> arbitrary -- requestContactImportEmailBlacklist :: Maybe Bool
      <*> arbitrary -- requestContactImportSmsBlacklist :: Maybe Bool
      <*> arbitrary -- requestContactImportUpdateExistingContacts :: Maybe Bool
      <*> arbitrary -- requestContactImportEmptyContactsAttributes :: Maybe Bool
    
instance Arbitrary RequestContactImportNewList where
  arbitrary =
    RequestContactImportNewList
      <$> arbitrary -- requestContactImportNewListListName :: Maybe Text
      <*> arbitrary -- requestContactImportNewListFolderId :: Maybe Integer
    
instance Arbitrary RequestSmsRecipientExport where
  arbitrary =
    RequestSmsRecipientExport
      <$> arbitrary -- requestSmsRecipientExportNotifyUrl :: Maybe Text
      <*> arbitrary -- requestSmsRecipientExportRecipientsType :: Text
    
instance Arbitrary SendEmail where
  arbitrary =
    SendEmail
      <$> arbitrary -- sendEmailEmailTo :: [Text]
      <*> arbitrary -- sendEmailEmailBcc :: Maybe [Text]
      <*> arbitrary -- sendEmailEmailCc :: Maybe [Text]
      <*> arbitrary -- sendEmailReplyTo :: Maybe Text
      <*> arbitrary -- sendEmailAttachmentUrl :: Maybe Text
      <*> arbitrary -- sendEmailAttachment :: Maybe [SendEmailAttachment]
      <*> arbitrary -- sendEmailHeaders :: Maybe A.Value
      <*> arbitrary -- sendEmailAttributes :: Maybe A.Value
      <*> arbitrary -- sendEmailTags :: Maybe [Text]
    
instance Arbitrary SendEmailAttachment where
  arbitrary =
    SendEmailAttachment
      <$> arbitrary -- sendEmailAttachmentContent :: ByteArray
      <*> arbitrary -- sendEmailAttachmentName :: Text
    
instance Arbitrary SendReport where
  arbitrary =
    SendReport
      <$> arbitrary -- sendReportLanguage :: Maybe Text
      <*> arbitrary -- sendReportEmail :: SendReportEmail
    
instance Arbitrary SendReportEmail where
  arbitrary =
    SendReportEmail
      <$> arbitrary -- sendReportEmailSubject :: Text
      <*> arbitrary -- sendReportEmailTo :: [Text]
      <*> arbitrary -- sendReportEmailContentType :: Maybe Text
      <*> arbitrary -- sendReportEmailBcc :: Maybe [Text]
      <*> arbitrary -- sendReportEmailCc :: Maybe [Text]
      <*> arbitrary -- sendReportEmailBody :: Text
    
instance Arbitrary SendSms where
  arbitrary =
    SendSms
      <$> arbitrary -- sendSmsReference :: Text
      <*> arbitrary -- sendSmsMessageId :: Integer
      <*> arbitrary -- sendSmsSmsCount :: Maybe Integer
      <*> arbitrary -- sendSmsUsedCredits :: Maybe Float
      <*> arbitrary -- sendSmsRemainingCredits :: Maybe Float
    
instance Arbitrary SendSmtpEmail where
  arbitrary =
    SendSmtpEmail
      <$> arbitrary -- sendSmtpEmailSender :: Maybe SendSmtpEmailSender
      <*> arbitrary -- sendSmtpEmailTo :: [SendSmtpEmailTo]
      <*> arbitrary -- sendSmtpEmailBcc :: Maybe [SendSmtpEmailBcc]
      <*> arbitrary -- sendSmtpEmailCc :: Maybe [SendSmtpEmailCc]
      <*> arbitrary -- sendSmtpEmailHtmlContent :: Maybe Text
      <*> arbitrary -- sendSmtpEmailTextContent :: Maybe Text
      <*> arbitrary -- sendSmtpEmailSubject :: Maybe Text
      <*> arbitrary -- sendSmtpEmailReplyTo :: Maybe SendSmtpEmailReplyTo
      <*> arbitrary -- sendSmtpEmailAttachment :: Maybe [SendSmtpEmailAttachment]
      <*> arbitrary -- sendSmtpEmailHeaders :: Maybe A.Value
      <*> arbitrary -- sendSmtpEmailTemplateId :: Maybe Integer
      <*> arbitrary -- sendSmtpEmailParams :: Maybe A.Value
      <*> arbitrary -- sendSmtpEmailTags :: Maybe [Text]
    
instance Arbitrary SendSmtpEmailAttachment where
  arbitrary =
    SendSmtpEmailAttachment
      <$> arbitrary -- sendSmtpEmailAttachmentUrl :: Maybe Text
      <*> arbitrary -- sendSmtpEmailAttachmentContent :: Maybe ByteArray
      <*> arbitrary -- sendSmtpEmailAttachmentName :: Maybe Text
    
instance Arbitrary SendSmtpEmailBcc where
  arbitrary =
    SendSmtpEmailBcc
      <$> arbitrary -- sendSmtpEmailBccEmail :: Text
      <*> arbitrary -- sendSmtpEmailBccName :: Maybe Text
    
instance Arbitrary SendSmtpEmailCc where
  arbitrary =
    SendSmtpEmailCc
      <$> arbitrary -- sendSmtpEmailCcEmail :: Text
      <*> arbitrary -- sendSmtpEmailCcName :: Maybe Text
    
instance Arbitrary SendSmtpEmailReplyTo where
  arbitrary =
    SendSmtpEmailReplyTo
      <$> arbitrary -- sendSmtpEmailReplyToEmail :: Text
      <*> arbitrary -- sendSmtpEmailReplyToName :: Maybe Text
    
instance Arbitrary SendSmtpEmailSender where
  arbitrary =
    SendSmtpEmailSender
      <$> arbitrary -- sendSmtpEmailSenderName :: Maybe Text
      <*> arbitrary -- sendSmtpEmailSenderEmail :: Text
    
instance Arbitrary SendSmtpEmailTo where
  arbitrary =
    SendSmtpEmailTo
      <$> arbitrary -- sendSmtpEmailToEmail :: Text
      <*> arbitrary -- sendSmtpEmailToName :: Maybe Text
    
instance Arbitrary SendTemplateEmail where
  arbitrary =
    SendTemplateEmail
      <$> arbitrary -- sendTemplateEmailMessageId :: Text
    
instance Arbitrary SendTestEmail where
  arbitrary =
    SendTestEmail
      <$> arbitrary -- sendTestEmailEmailTo :: Maybe [Text]
    
instance Arbitrary SendTestSms where
  arbitrary =
    SendTestSms
      <$> arbitrary -- sendTestSmsPhoneNumber :: Maybe Text
    
instance Arbitrary SendTransacSms where
  arbitrary =
    SendTransacSms
      <$> arbitrary -- sendTransacSmsSender :: Text
      <*> arbitrary -- sendTransacSmsRecipient :: Text
      <*> arbitrary -- sendTransacSmsContent :: Text
      <*> arbitrary -- sendTransacSmsType :: Maybe Text
      <*> arbitrary -- sendTransacSmsTag :: Maybe Text
      <*> arbitrary -- sendTransacSmsWebUrl :: Maybe Text
    
instance Arbitrary UpdateAttribute where
  arbitrary =
    UpdateAttribute
      <$> arbitrary -- updateAttributeValue :: Maybe Text
      <*> arbitrary -- updateAttributeEnumeration :: Maybe [UpdateAttributeEnumeration]
    
instance Arbitrary UpdateAttributeEnumeration where
  arbitrary =
    UpdateAttributeEnumeration
      <$> arbitrary -- updateAttributeEnumerationValue :: Int
      <*> arbitrary -- updateAttributeEnumerationLabel :: Text
    
instance Arbitrary UpdateCampaignStatus where
  arbitrary =
    UpdateCampaignStatus
      <$> arbitrary -- updateCampaignStatusStatus :: Maybe Text
    
instance Arbitrary UpdateChild where
  arbitrary =
    UpdateChild
      <$> arbitrary -- updateChildEmail :: Maybe Text
      <*> arbitrary -- updateChildFirstName :: Maybe Text
      <*> arbitrary -- updateChildLastName :: Maybe Text
      <*> arbitrary -- updateChildCompanyName :: Maybe Text
      <*> arbitrary -- updateChildPassword :: Maybe Text
    
instance Arbitrary UpdateChildAccountStatus where
  arbitrary =
    UpdateChildAccountStatus
      <$> arbitrary -- updateChildAccountStatusTransactionalEmail :: Maybe Bool
      <*> arbitrary -- updateChildAccountStatusTransactionalSms :: Maybe Bool
      <*> arbitrary -- updateChildAccountStatusMarketingAutomation :: Maybe Bool
    
instance Arbitrary UpdateChildDomain where
  arbitrary =
    UpdateChildDomain
      <$> arbitrary -- updateChildDomainDomain :: Maybe Text
    
instance Arbitrary UpdateContact where
  arbitrary =
    UpdateContact
      <$> arbitrary -- updateContactAttributes :: Maybe A.Value
      <*> arbitrary -- updateContactEmailBlacklisted :: Maybe Bool
      <*> arbitrary -- updateContactSmsBlacklisted :: Maybe Bool
      <*> arbitrary -- updateContactListIds :: Maybe [Integer]
      <*> arbitrary -- updateContactUnlinkListIds :: Maybe [Integer]
      <*> arbitrary -- updateContactSmtpBlacklistSender :: Maybe [Text]
    
instance Arbitrary UpdateEmailCampaign where
  arbitrary =
    UpdateEmailCampaign
      <$> arbitrary -- updateEmailCampaignTag :: Maybe Text
      <*> arbitrary -- updateEmailCampaignSender :: Maybe UpdateEmailCampaignSender
      <*> arbitrary -- updateEmailCampaignName :: Maybe Text
      <*> arbitrary -- updateEmailCampaignHtmlContent :: Maybe Text
      <*> arbitrary -- updateEmailCampaignHtmlUrl :: Maybe Text
      <*> arbitrary -- updateEmailCampaignScheduledAt :: Maybe DateTime
      <*> arbitrary -- updateEmailCampaignSubject :: Maybe Text
      <*> arbitrary -- updateEmailCampaignReplyTo :: Maybe Text
      <*> arbitrary -- updateEmailCampaignToField :: Maybe Text
      <*> arbitrary -- updateEmailCampaignRecipients :: Maybe UpdateEmailCampaignRecipients
      <*> arbitrary -- updateEmailCampaignAttachmentUrl :: Maybe Text
      <*> arbitrary -- updateEmailCampaignInlineImageActivation :: Maybe Bool
      <*> arbitrary -- updateEmailCampaignMirrorActive :: Maybe Bool
      <*> arbitrary -- updateEmailCampaignRecurring :: Maybe Bool
      <*> arbitrary -- updateEmailCampaignFooter :: Maybe Text
      <*> arbitrary -- updateEmailCampaignHeader :: Maybe Text
      <*> arbitrary -- updateEmailCampaignUtmCampaign :: Maybe Text
      <*> arbitrary -- updateEmailCampaignParams :: Maybe A.Value
      <*> arbitrary -- updateEmailCampaignSendAtBestTime :: Maybe Bool
      <*> arbitrary -- updateEmailCampaignAbTesting :: Maybe Bool
      <*> arbitrary -- updateEmailCampaignSubjectA :: Maybe Text
      <*> arbitrary -- updateEmailCampaignSubjectB :: Maybe Text
      <*> arbitrary -- updateEmailCampaignSplitRule :: Maybe Integer
      <*> arbitrary -- updateEmailCampaignWinnerCriteria :: Maybe Text
      <*> arbitrary -- updateEmailCampaignWinnerDelay :: Maybe Integer
      <*> arbitrary -- updateEmailCampaignIpWarmupEnable :: Maybe Bool
      <*> arbitrary -- updateEmailCampaignInitialQuota :: Maybe Integer
      <*> arbitrary -- updateEmailCampaignIncreaseRate :: Maybe Integer
    
instance Arbitrary UpdateEmailCampaignRecipients where
  arbitrary =
    UpdateEmailCampaignRecipients
      <$> arbitrary -- updateEmailCampaignRecipientsExclusionListIds :: Maybe [Integer]
      <*> arbitrary -- updateEmailCampaignRecipientsListIds :: Maybe [Integer]
    
instance Arbitrary UpdateEmailCampaignSender where
  arbitrary =
    UpdateEmailCampaignSender
      <$> arbitrary -- updateEmailCampaignSenderName :: Maybe Text
      <*> arbitrary -- updateEmailCampaignSenderEmail :: Maybe Text
      <*> arbitrary -- updateEmailCampaignSenderId :: Maybe Integer
    
instance Arbitrary UpdateList where
  arbitrary =
    UpdateList
      <$> arbitrary -- updateListName :: Maybe Text
      <*> arbitrary -- updateListFolderId :: Maybe Integer
    
instance Arbitrary UpdateSender where
  arbitrary =
    UpdateSender
      <$> arbitrary -- updateSenderName :: Maybe Text
      <*> arbitrary -- updateSenderEmail :: Maybe Text
      <*> arbitrary -- updateSenderIps :: Maybe [CreateSenderIps]
    
instance Arbitrary UpdateSmsCampaign where
  arbitrary =
    UpdateSmsCampaign
      <$> arbitrary -- updateSmsCampaignName :: Maybe Text
      <*> arbitrary -- updateSmsCampaignSender :: Maybe Text
      <*> arbitrary -- updateSmsCampaignContent :: Maybe Text
      <*> arbitrary -- updateSmsCampaignRecipients :: Maybe CreateSmsCampaignRecipients
      <*> arbitrary -- updateSmsCampaignScheduledAt :: Maybe DateTime
    
instance Arbitrary UpdateSmtpTemplate where
  arbitrary =
    UpdateSmtpTemplate
      <$> arbitrary -- updateSmtpTemplateTag :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateSender :: Maybe UpdateSmtpTemplateSender
      <*> arbitrary -- updateSmtpTemplateTemplateName :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateHtmlContent :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateHtmlUrl :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateSubject :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateReplyTo :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateToField :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateAttachmentUrl :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateIsActive :: Maybe Bool
    
instance Arbitrary UpdateSmtpTemplateSender where
  arbitrary =
    UpdateSmtpTemplateSender
      <$> arbitrary -- updateSmtpTemplateSenderName :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateSenderEmail :: Maybe Text
      <*> arbitrary -- updateSmtpTemplateSenderId :: Maybe Integer
    
instance Arbitrary UpdateWebhook where
  arbitrary =
    UpdateWebhook
      <$> arbitrary -- updateWebhookUrl :: Maybe Text
      <*> arbitrary -- updateWebhookDescription :: Maybe Text
      <*> arbitrary -- updateWebhookEvents :: Maybe [Text]
    
instance Arbitrary GetChildInfo where
  arbitrary =
    GetChildInfo
      <$> arbitrary -- getChildInfoEmail :: Text
      <*> arbitrary -- getChildInfoFirstName :: Text
      <*> arbitrary -- getChildInfoLastName :: Text
      <*> arbitrary -- getChildInfoCompanyName :: Text
      <*> arbitrary -- getChildInfoCredits :: Maybe GetChildInfoCredits
      <*> arbitrary -- getChildInfoStatistics :: Maybe GetChildInfoStatistics
      <*> arbitrary -- getChildInfoPassword :: Text
      <*> arbitrary -- getChildInfoIps :: Maybe [Text]
      <*> arbitrary -- getChildInfoApiKeys :: Maybe GetChildInfoApiKeys
    
instance Arbitrary GetExtendedCampaignOverview where
  arbitrary =
    GetExtendedCampaignOverview
      <$> arbitrary -- getExtendedCampaignOverviewId :: Integer
      <*> arbitrary -- getExtendedCampaignOverviewName :: Text
      <*> arbitrary -- getExtendedCampaignOverviewSubject :: Maybe Text
      <*> arbitrary -- getExtendedCampaignOverviewType :: Text
      <*> arbitrary -- getExtendedCampaignOverviewStatus :: Text
      <*> arbitrary -- getExtendedCampaignOverviewScheduledAt :: Maybe DateTime
      <*> arbitrary -- getExtendedCampaignOverviewAbTesting :: Maybe Bool
      <*> arbitrary -- getExtendedCampaignOverviewSubjectA :: Maybe Text
      <*> arbitrary -- getExtendedCampaignOverviewSubjectB :: Maybe Text
      <*> arbitrary -- getExtendedCampaignOverviewSplitRule :: Maybe Int
      <*> arbitrary -- getExtendedCampaignOverviewWinnerCriteria :: Maybe Text
      <*> arbitrary -- getExtendedCampaignOverviewWinnerDelay :: Maybe Int
      <*> arbitrary -- getExtendedCampaignOverviewSendAtBestTime :: Maybe Bool
      <*> arbitrary -- getExtendedCampaignOverviewTestSent :: Bool
      <*> arbitrary -- getExtendedCampaignOverviewHeader :: Text
      <*> arbitrary -- getExtendedCampaignOverviewFooter :: Text
      <*> arbitrary -- getExtendedCampaignOverviewSender :: GetExtendedCampaignOverviewSender
      <*> arbitrary -- getExtendedCampaignOverviewReplyTo :: Text
      <*> arbitrary -- getExtendedCampaignOverviewToField :: Text
      <*> arbitrary -- getExtendedCampaignOverviewHtmlContent :: Text
      <*> arbitrary -- getExtendedCampaignOverviewShareLink :: Maybe Text
      <*> arbitrary -- getExtendedCampaignOverviewTag :: Text
      <*> arbitrary -- getExtendedCampaignOverviewCreatedAt :: DateTime
      <*> arbitrary -- getExtendedCampaignOverviewModifiedAt :: DateTime
      <*> arbitrary -- getExtendedCampaignOverviewInlineImageActivation :: Maybe Bool
      <*> arbitrary -- getExtendedCampaignOverviewMirrorActive :: Maybe Bool
      <*> arbitrary -- getExtendedCampaignOverviewRecurring :: Maybe Bool
      <*> arbitrary -- getExtendedCampaignOverviewSentDate :: Maybe DateTime
    
instance Arbitrary GetExtendedClient where
  arbitrary =
    GetExtendedClient
      <$> arbitrary -- getExtendedClientEmail :: Text
      <*> arbitrary -- getExtendedClientFirstName :: Text
      <*> arbitrary -- getExtendedClientLastName :: Text
      <*> arbitrary -- getExtendedClientCompanyName :: Text
      <*> arbitrary -- getExtendedClientAddress :: GetExtendedClientAddress
    
instance Arbitrary GetExtendedContactDetails where
  arbitrary =
    GetExtendedContactDetails
      <$> arbitrary -- getExtendedContactDetailsEmail :: Text
      <*> arbitrary -- getExtendedContactDetailsId :: Integer
      <*> arbitrary -- getExtendedContactDetailsEmailBlacklisted :: Bool
      <*> arbitrary -- getExtendedContactDetailsSmsBlacklisted :: Bool
      <*> arbitrary -- getExtendedContactDetailsCreatedAt :: DateTime
      <*> arbitrary -- getExtendedContactDetailsModifiedAt :: DateTime
      <*> arbitrary -- getExtendedContactDetailsListIds :: [Integer]
      <*> arbitrary -- getExtendedContactDetailsListUnsubscribed :: Maybe [Integer]
      <*> arbitrary -- getExtendedContactDetailsAttributes :: A.Value
      <*> arbitrary -- getExtendedContactDetailsStatistics :: GetExtendedContactDetailsStatistics
    
instance Arbitrary GetExtendedList where
  arbitrary =
    GetExtendedList
      <$> arbitrary -- getExtendedListId :: Integer
      <*> arbitrary -- getExtendedListName :: Text
      <*> arbitrary -- getExtendedListTotalBlacklisted :: Integer
      <*> arbitrary -- getExtendedListTotalSubscribers :: Integer
      <*> arbitrary -- getExtendedListFolderId :: Integer
      <*> arbitrary -- getExtendedListCreatedAt :: DateTime
      <*> arbitrary -- getExtendedListCampaignStats :: Maybe [GetExtendedListCampaignStats]
      <*> arbitrary -- getExtendedListDynamicList :: Maybe Bool
    
instance Arbitrary GetSmsCampaign where
  arbitrary =
    GetSmsCampaign
      <$> arbitrary -- getSmsCampaignId :: Integer
      <*> arbitrary -- getSmsCampaignName :: Text
      <*> arbitrary -- getSmsCampaignStatus :: Text
      <*> arbitrary -- getSmsCampaignContent :: Text
      <*> arbitrary -- getSmsCampaignScheduledAt :: DateTime
      <*> arbitrary -- getSmsCampaignSender :: Text
      <*> arbitrary -- getSmsCampaignCreatedAt :: DateTime
      <*> arbitrary -- getSmsCampaignModifiedAt :: DateTime
      <*> arbitrary -- getSmsCampaignRecipients :: A.Value
      <*> arbitrary -- getSmsCampaignStatistics :: A.Value
    
instance Arbitrary GetAccount where
  arbitrary =
    GetAccount
      <$> arbitrary -- getAccountEmail :: Text
      <*> arbitrary -- getAccountFirstName :: Text
      <*> arbitrary -- getAccountLastName :: Text
      <*> arbitrary -- getAccountCompanyName :: Text
      <*> arbitrary -- getAccountAddress :: GetExtendedClientAddress
      <*> arbitrary -- getAccountPlan :: [GetAccountPlan]
      <*> arbitrary -- getAccountRelay :: GetAccountRelay
      <*> arbitrary -- getAccountMarketingAutomation :: Maybe GetAccountMarketingAutomation
    
instance Arbitrary GetEmailCampaign where
  arbitrary =
    GetEmailCampaign
      <$> arbitrary -- getEmailCampaignId :: Integer
      <*> arbitrary -- getEmailCampaignName :: Text
      <*> arbitrary -- getEmailCampaignSubject :: Maybe Text
      <*> arbitrary -- getEmailCampaignType :: Text
      <*> arbitrary -- getEmailCampaignStatus :: Text
      <*> arbitrary -- getEmailCampaignScheduledAt :: Maybe DateTime
      <*> arbitrary -- getEmailCampaignAbTesting :: Maybe Bool
      <*> arbitrary -- getEmailCampaignSubjectA :: Maybe Text
      <*> arbitrary -- getEmailCampaignSubjectB :: Maybe Text
      <*> arbitrary -- getEmailCampaignSplitRule :: Maybe Int
      <*> arbitrary -- getEmailCampaignWinnerCriteria :: Maybe Text
      <*> arbitrary -- getEmailCampaignWinnerDelay :: Maybe Int
      <*> arbitrary -- getEmailCampaignSendAtBestTime :: Maybe Bool
      <*> arbitrary -- getEmailCampaignTestSent :: Bool
      <*> arbitrary -- getEmailCampaignHeader :: Text
      <*> arbitrary -- getEmailCampaignFooter :: Text
      <*> arbitrary -- getEmailCampaignSender :: GetExtendedCampaignOverviewSender
      <*> arbitrary -- getEmailCampaignReplyTo :: Text
      <*> arbitrary -- getEmailCampaignToField :: Text
      <*> arbitrary -- getEmailCampaignHtmlContent :: Text
      <*> arbitrary -- getEmailCampaignShareLink :: Maybe Text
      <*> arbitrary -- getEmailCampaignTag :: Text
      <*> arbitrary -- getEmailCampaignCreatedAt :: DateTime
      <*> arbitrary -- getEmailCampaignModifiedAt :: DateTime
      <*> arbitrary -- getEmailCampaignInlineImageActivation :: Maybe Bool
      <*> arbitrary -- getEmailCampaignMirrorActive :: Maybe Bool
      <*> arbitrary -- getEmailCampaignRecurring :: Maybe Bool
      <*> arbitrary -- getEmailCampaignSentDate :: Maybe DateTime
      <*> arbitrary -- getEmailCampaignRecipients :: A.Value
      <*> arbitrary -- getEmailCampaignStatistics :: A.Value
    



instance Arbitrary E'AttributeCategory where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Category where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Code where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Code2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ContentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CreditsType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Event where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Event2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Events where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Language where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RecipientsType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RecipientsType2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type6 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type7 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'WinnerCriteria where
  arbitrary = arbitraryBoundedEnum
