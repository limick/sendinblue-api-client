{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

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
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary AbTestCampaignResult where
  arbitrary = sized genAbTestCampaignResult

genAbTestCampaignResult :: Int -> Gen AbTestCampaignResult
genAbTestCampaignResult n =
  AbTestCampaignResult
    <$> arbitraryReducedMaybe n -- abTestCampaignResultWinningVersion :: Maybe E'WinningVersion
    <*> arbitraryReducedMaybe n -- abTestCampaignResultWinningCriteria :: Maybe E'WinningCriteria
    <*> arbitraryReducedMaybe n -- abTestCampaignResultWinningSubjectLine :: Maybe Text
    <*> arbitraryReducedMaybe n -- abTestCampaignResultOpenRate :: Maybe Text
    <*> arbitraryReducedMaybe n -- abTestCampaignResultClickRate :: Maybe Text
    <*> arbitraryReducedMaybe n -- abTestCampaignResultWinningVersionRate :: Maybe Text
    <*> arbitraryReducedMaybe n -- abTestCampaignResultStatistics :: Maybe AbTestCampaignResultStatistics
    <*> arbitraryReducedMaybe n -- abTestCampaignResultClickedLinks :: Maybe AbTestCampaignResultClickedLinks
  
instance Arbitrary AbTestCampaignResultClickedLinks where
  arbitrary = sized genAbTestCampaignResultClickedLinks

genAbTestCampaignResultClickedLinks :: Int -> Gen AbTestCampaignResultClickedLinks
genAbTestCampaignResultClickedLinks n =
  AbTestCampaignResultClickedLinks
    <$> arbitraryReduced n -- abTestCampaignResultClickedLinksVersionA :: [A.Value]
    <*> arbitraryReduced n -- abTestCampaignResultClickedLinksVersionB :: [A.Value]
  
instance Arbitrary AbTestCampaignResultStatistics where
  arbitrary = sized genAbTestCampaignResultStatistics

genAbTestCampaignResultStatistics :: Int -> Gen AbTestCampaignResultStatistics
genAbTestCampaignResultStatistics n =
  AbTestCampaignResultStatistics
    <$> arbitraryReduced n -- abTestCampaignResultStatisticsOpeners :: AbTestVersionStats
    <*> arbitraryReduced n -- abTestCampaignResultStatisticsClicks :: AbTestVersionStats
    <*> arbitraryReduced n -- abTestCampaignResultStatisticsUnsubscribed :: AbTestVersionStats
    <*> arbitraryReduced n -- abTestCampaignResultStatisticsHardBounces :: AbTestVersionStats
    <*> arbitraryReduced n -- abTestCampaignResultStatisticsSoftBounces :: AbTestVersionStats
    <*> arbitraryReduced n -- abTestCampaignResultStatisticsComplaints :: AbTestVersionStats
  
instance Arbitrary AbTestVersionStats where
  arbitrary = sized genAbTestVersionStats

genAbTestVersionStats :: Int -> Gen AbTestVersionStats
genAbTestVersionStats n =
  AbTestVersionStats
    <$> arbitrary -- abTestVersionStatsVersionA :: Text
    <*> arbitrary -- abTestVersionStatsVersionB :: Text
  
instance Arbitrary AddChildDomain where
  arbitrary = sized genAddChildDomain

genAddChildDomain :: Int -> Gen AddChildDomain
genAddChildDomain n =
  AddChildDomain
    <$> arbitraryReducedMaybe n -- addChildDomainDomain :: Maybe Text
  
instance Arbitrary AddContactToList where
  arbitrary = sized genAddContactToList

genAddContactToList :: Int -> Gen AddContactToList
genAddContactToList n =
  AddContactToList
    <$> arbitraryReducedMaybe n -- addContactToListEmails :: Maybe [Text]
  
instance Arbitrary AddCredits where
  arbitrary = sized genAddCredits

genAddCredits :: Int -> Gen AddCredits
genAddCredits n =
  AddCredits
    <$> arbitraryReducedMaybe n -- addCreditsSms :: Maybe Integer
    <*> arbitraryReducedMaybe n -- addCreditsEmail :: Maybe Integer
  
instance Arbitrary BlockDomain where
  arbitrary = sized genBlockDomain

genBlockDomain :: Int -> Gen BlockDomain
genBlockDomain n =
  BlockDomain
    <$> arbitrary -- blockDomainDomain :: Text
  
instance Arbitrary CreateAttribute where
  arbitrary = sized genCreateAttribute

genCreateAttribute :: Int -> Gen CreateAttribute
genCreateAttribute n =
  CreateAttribute
    <$> arbitraryReducedMaybe n -- createAttributeValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- createAttributeEnumeration :: Maybe [CreateAttributeEnumeration]
    <*> arbitraryReducedMaybe n -- createAttributeType :: Maybe E'Type4
  
instance Arbitrary CreateAttributeEnumeration where
  arbitrary = sized genCreateAttributeEnumeration

genCreateAttributeEnumeration :: Int -> Gen CreateAttributeEnumeration
genCreateAttributeEnumeration n =
  CreateAttributeEnumeration
    <$> arbitrary -- createAttributeEnumerationValue :: Int
    <*> arbitrary -- createAttributeEnumerationLabel :: Text
  
instance Arbitrary CreateChild where
  arbitrary = sized genCreateChild

genCreateChild :: Int -> Gen CreateChild
genCreateChild n =
  CreateChild
    <$> arbitrary -- createChildEmail :: Text
    <*> arbitrary -- createChildFirstName :: Text
    <*> arbitrary -- createChildLastName :: Text
    <*> arbitrary -- createChildCompanyName :: Text
    <*> arbitrary -- createChildPassword :: Text
    <*> arbitraryReducedMaybe n -- createChildLanguage :: Maybe E'Language
  
instance Arbitrary CreateContact where
  arbitrary = sized genCreateContact

genCreateContact :: Int -> Gen CreateContact
genCreateContact n =
  CreateContact
    <$> arbitraryReducedMaybe n -- createContactEmail :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- createContactAttributes :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- createContactEmailBlacklisted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createContactSmsBlacklisted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createContactListIds :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- createContactUpdateEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createContactSmtpBlacklistSender :: Maybe [Text]
  
instance Arbitrary CreateDoiContact where
  arbitrary = sized genCreateDoiContact

genCreateDoiContact :: Int -> Gen CreateDoiContact
genCreateDoiContact n =
  CreateDoiContact
    <$> arbitrary -- createDoiContactEmail :: Text
    <*> arbitraryReducedMaybeValue n -- createDoiContactAttributes :: Maybe A.Value
    <*> arbitrary -- createDoiContactIncludeListIds :: [Integer]
    <*> arbitraryReducedMaybe n -- createDoiContactExcludeListIds :: Maybe [Integer]
    <*> arbitrary -- createDoiContactTemplateId :: Integer
    <*> arbitrary -- createDoiContactRedirectionUrl :: Text
  
instance Arbitrary CreateEmailCampaign where
  arbitrary = sized genCreateEmailCampaign

genCreateEmailCampaign :: Int -> Gen CreateEmailCampaign
genCreateEmailCampaign n =
  CreateEmailCampaign
    <$> arbitraryReducedMaybe n -- createEmailCampaignTag :: Maybe Text
    <*> arbitraryReduced n -- createEmailCampaignSender :: CreateEmailCampaignSender
    <*> arbitrary -- createEmailCampaignName :: Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignHtmlContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignTemplateId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createEmailCampaignScheduledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- createEmailCampaignSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignReplyTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignToField :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignRecipients :: Maybe CreateEmailCampaignRecipients
    <*> arbitraryReducedMaybe n -- createEmailCampaignAttachmentUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignInlineImageActivation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createEmailCampaignMirrorActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createEmailCampaignFooter :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignHeader :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignUtmCampaign :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- createEmailCampaignParams :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- createEmailCampaignSendAtBestTime :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createEmailCampaignAbTesting :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createEmailCampaignSubjectA :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignSubjectB :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignSplitRule :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createEmailCampaignWinnerCriteria :: Maybe E'WinnerCriteria
    <*> arbitraryReducedMaybe n -- createEmailCampaignWinnerDelay :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createEmailCampaignIpWarmupEnable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createEmailCampaignInitialQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createEmailCampaignIncreaseRate :: Maybe Integer
  
instance Arbitrary CreateEmailCampaignRecipients where
  arbitrary = sized genCreateEmailCampaignRecipients

genCreateEmailCampaignRecipients :: Int -> Gen CreateEmailCampaignRecipients
genCreateEmailCampaignRecipients n =
  CreateEmailCampaignRecipients
    <$> arbitraryReducedMaybe n -- createEmailCampaignRecipientsExclusionListIds :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- createEmailCampaignRecipientsListIds :: Maybe [Integer]
  
instance Arbitrary CreateEmailCampaignSender where
  arbitrary = sized genCreateEmailCampaignSender

genCreateEmailCampaignSender :: Int -> Gen CreateEmailCampaignSender
genCreateEmailCampaignSender n =
  CreateEmailCampaignSender
    <$> arbitraryReducedMaybe n -- createEmailCampaignSenderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignSenderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- createEmailCampaignSenderId :: Maybe Integer
  
instance Arbitrary CreateList where
  arbitrary = sized genCreateList

genCreateList :: Int -> Gen CreateList
genCreateList n =
  CreateList
    <$> arbitrary -- createListName :: Text
    <*> arbitrary -- createListFolderId :: Integer
  
instance Arbitrary CreateModel where
  arbitrary = sized genCreateModel

genCreateModel :: Int -> Gen CreateModel
genCreateModel n =
  CreateModel
    <$> arbitrary -- createModelId :: Integer
  
instance Arbitrary CreateReseller where
  arbitrary = sized genCreateReseller

genCreateReseller :: Int -> Gen CreateReseller
genCreateReseller n =
  CreateReseller
    <$> arbitrary -- createResellerAuthKey :: Text
    <*> arbitraryReducedMaybe n -- createResellerId :: Maybe Integer
  
instance Arbitrary CreateSender where
  arbitrary = sized genCreateSender

genCreateSender :: Int -> Gen CreateSender
genCreateSender n =
  CreateSender
    <$> arbitrary -- createSenderName :: Text
    <*> arbitrary -- createSenderEmail :: Text
    <*> arbitraryReducedMaybe n -- createSenderIps :: Maybe [CreateSenderIps]
  
instance Arbitrary CreateSenderIps where
  arbitrary = sized genCreateSenderIps

genCreateSenderIps :: Int -> Gen CreateSenderIps
genCreateSenderIps n =
  CreateSenderIps
    <$> arbitrary -- createSenderIpsIp :: Text
    <*> arbitrary -- createSenderIpsDomain :: Text
    <*> arbitraryReducedMaybe n -- createSenderIpsWeight :: Maybe Integer
  
instance Arbitrary CreateSenderModel where
  arbitrary = sized genCreateSenderModel

genCreateSenderModel :: Int -> Gen CreateSenderModel
genCreateSenderModel n =
  CreateSenderModel
    <$> arbitrary -- createSenderModelId :: Integer
    <*> arbitraryReducedMaybe n -- createSenderModelSpfError :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createSenderModelDkimError :: Maybe Bool
  
instance Arbitrary CreateSmsCampaign where
  arbitrary = sized genCreateSmsCampaign

genCreateSmsCampaign :: Int -> Gen CreateSmsCampaign
genCreateSmsCampaign n =
  CreateSmsCampaign
    <$> arbitrary -- createSmsCampaignName :: Text
    <*> arbitrary -- createSmsCampaignSender :: Text
    <*> arbitrary -- createSmsCampaignContent :: Text
    <*> arbitraryReducedMaybe n -- createSmsCampaignRecipients :: Maybe CreateSmsCampaignRecipients
    <*> arbitraryReducedMaybe n -- createSmsCampaignScheduledAt :: Maybe DateTime
  
instance Arbitrary CreateSmsCampaignRecipients where
  arbitrary = sized genCreateSmsCampaignRecipients

genCreateSmsCampaignRecipients :: Int -> Gen CreateSmsCampaignRecipients
genCreateSmsCampaignRecipients n =
  CreateSmsCampaignRecipients
    <$> arbitrary -- createSmsCampaignRecipientsListIds :: [Integer]
    <*> arbitraryReducedMaybe n -- createSmsCampaignRecipientsExclusionListIds :: Maybe [Integer]
  
instance Arbitrary CreateSmtpEmail where
  arbitrary = sized genCreateSmtpEmail

genCreateSmtpEmail :: Int -> Gen CreateSmtpEmail
genCreateSmtpEmail n =
  CreateSmtpEmail
    <$> arbitrary -- createSmtpEmailMessageId :: Text
  
instance Arbitrary CreateSmtpTemplate where
  arbitrary = sized genCreateSmtpTemplate

genCreateSmtpTemplate :: Int -> Gen CreateSmtpTemplate
genCreateSmtpTemplate n =
  CreateSmtpTemplate
    <$> arbitraryReducedMaybe n -- createSmtpTemplateTag :: Maybe Text
    <*> arbitraryReduced n -- createSmtpTemplateSender :: CreateSmtpTemplateSender
    <*> arbitrary -- createSmtpTemplateTemplateName :: Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateHtmlContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateHtmlUrl :: Maybe Text
    <*> arbitrary -- createSmtpTemplateSubject :: Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateReplyTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateToField :: Maybe Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateAttachmentUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateIsActive :: Maybe Bool
  
instance Arbitrary CreateSmtpTemplateSender where
  arbitrary = sized genCreateSmtpTemplateSender

genCreateSmtpTemplateSender :: Int -> Gen CreateSmtpTemplateSender
genCreateSmtpTemplateSender n =
  CreateSmtpTemplateSender
    <$> arbitraryReducedMaybe n -- createSmtpTemplateSenderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateSenderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- createSmtpTemplateSenderId :: Maybe Integer
  
instance Arbitrary CreateUpdateContactModel where
  arbitrary = sized genCreateUpdateContactModel

genCreateUpdateContactModel :: Int -> Gen CreateUpdateContactModel
genCreateUpdateContactModel n =
  CreateUpdateContactModel
    <$> arbitraryReducedMaybe n -- createUpdateContactModelId :: Maybe Integer
  
instance Arbitrary CreateUpdateFolder where
  arbitrary = sized genCreateUpdateFolder

genCreateUpdateFolder :: Int -> Gen CreateUpdateFolder
genCreateUpdateFolder n =
  CreateUpdateFolder
    <$> arbitraryReducedMaybe n -- createUpdateFolderName :: Maybe Text
  
instance Arbitrary CreateWebhook where
  arbitrary = sized genCreateWebhook

genCreateWebhook :: Int -> Gen CreateWebhook
genCreateWebhook n =
  CreateWebhook
    <$> arbitrary -- createWebhookUrl :: Text
    <*> arbitraryReducedMaybe n -- createWebhookDescription :: Maybe Text
    <*> arbitrary -- createWebhookEvents :: [E'Events]
    <*> arbitraryReducedMaybe n -- createWebhookType :: Maybe E'Type3
  
instance Arbitrary CreatedProcessId where
  arbitrary = sized genCreatedProcessId

genCreatedProcessId :: Int -> Gen CreatedProcessId
genCreatedProcessId n =
  CreatedProcessId
    <$> arbitrary -- createdProcessIdProcessId :: Integer
  
instance Arbitrary DeleteHardbounces where
  arbitrary = sized genDeleteHardbounces

genDeleteHardbounces :: Int -> Gen DeleteHardbounces
genDeleteHardbounces n =
  DeleteHardbounces
    <$> arbitraryReducedMaybe n -- deleteHardbouncesStartDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteHardbouncesEndDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteHardbouncesContactEmail :: Maybe Text
  
instance Arbitrary EmailExportRecipients where
  arbitrary = sized genEmailExportRecipients

genEmailExportRecipients :: Int -> Gen EmailExportRecipients
genEmailExportRecipients n =
  EmailExportRecipients
    <$> arbitraryReducedMaybe n -- emailExportRecipientsNotifyUrl :: Maybe Text
    <*> arbitrary -- emailExportRecipientsRecipientsType :: E'RecipientsType
  
instance Arbitrary ErrorModel where
  arbitrary = sized genErrorModel

genErrorModel :: Int -> Gen ErrorModel
genErrorModel n =
  ErrorModel
    <$> arbitrary -- errorModelCode :: E'Code
    <*> arbitrary -- errorModelMessage :: Text
  
instance Arbitrary GetAccount where
  arbitrary = sized genGetAccount

genGetAccount :: Int -> Gen GetAccount
genGetAccount n =
  GetAccount
    <$> arbitrary -- getAccountEmail :: Text
    <*> arbitrary -- getAccountFirstName :: Text
    <*> arbitrary -- getAccountLastName :: Text
    <*> arbitrary -- getAccountCompanyName :: Text
    <*> arbitraryReduced n -- getAccountAddress :: GetExtendedClientAllOfAddress
    <*> arbitraryReduced n -- getAccountPlan :: [GetAccountAllOfPlan]
    <*> arbitraryReduced n -- getAccountRelay :: GetAccountAllOfRelay
    <*> arbitraryReducedMaybe n -- getAccountMarketingAutomation :: Maybe GetAccountAllOfMarketingAutomation
  
instance Arbitrary GetAccountAllOf where
  arbitrary = sized genGetAccountAllOf

genGetAccountAllOf :: Int -> Gen GetAccountAllOf
genGetAccountAllOf n =
  GetAccountAllOf
    <$> arbitraryReduced n -- getAccountAllOfPlan :: [GetAccountAllOfPlan]
    <*> arbitraryReduced n -- getAccountAllOfRelay :: GetAccountAllOfRelay
    <*> arbitraryReducedMaybe n -- getAccountAllOfMarketingAutomation :: Maybe GetAccountAllOfMarketingAutomation
  
instance Arbitrary GetAccountAllOfMarketingAutomation where
  arbitrary = sized genGetAccountAllOfMarketingAutomation

genGetAccountAllOfMarketingAutomation :: Int -> Gen GetAccountAllOfMarketingAutomation
genGetAccountAllOfMarketingAutomation n =
  GetAccountAllOfMarketingAutomation
    <$> arbitraryReducedMaybe n -- getAccountAllOfMarketingAutomationKey :: Maybe Text
    <*> arbitrary -- getAccountAllOfMarketingAutomationEnabled :: Bool
  
instance Arbitrary GetAccountAllOfPlan where
  arbitrary = sized genGetAccountAllOfPlan

genGetAccountAllOfPlan :: Int -> Gen GetAccountAllOfPlan
genGetAccountAllOfPlan n =
  GetAccountAllOfPlan
    <$> arbitrary -- getAccountAllOfPlanType :: E'Type5
    <*> arbitrary -- getAccountAllOfPlanCreditsType :: E'CreditsType
    <*> arbitrary -- getAccountAllOfPlanCredits :: Float
    <*> arbitraryReducedMaybe n -- getAccountAllOfPlanStartDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- getAccountAllOfPlanEndDate :: Maybe Date
    <*> arbitraryReducedMaybe n -- getAccountAllOfPlanUserLimit :: Maybe Int
  
instance Arbitrary GetAccountAllOfRelay where
  arbitrary = sized genGetAccountAllOfRelay

genGetAccountAllOfRelay :: Int -> Gen GetAccountAllOfRelay
genGetAccountAllOfRelay n =
  GetAccountAllOfRelay
    <$> arbitrary -- getAccountAllOfRelayEnabled :: Bool
    <*> arbitraryReduced n -- getAccountAllOfRelayData :: GetAccountAllOfRelayData
  
instance Arbitrary GetAccountAllOfRelayData where
  arbitrary = sized genGetAccountAllOfRelayData

genGetAccountAllOfRelayData :: Int -> Gen GetAccountAllOfRelayData
genGetAccountAllOfRelayData n =
  GetAccountAllOfRelayData
    <$> arbitrary -- getAccountAllOfRelayDataUserName :: Text
    <*> arbitrary -- getAccountAllOfRelayDataRelay :: Text
    <*> arbitrary -- getAccountAllOfRelayDataPort :: Int
  
instance Arbitrary GetAggregatedReport where
  arbitrary = sized genGetAggregatedReport

genGetAggregatedReport :: Int -> Gen GetAggregatedReport
genGetAggregatedReport n =
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
  arbitrary = sized genGetAttributes

genGetAttributes :: Int -> Gen GetAttributes
genGetAttributes n =
  GetAttributes
    <$> arbitraryReduced n -- getAttributesAttributes :: [GetAttributesAttributes]
  
instance Arbitrary GetAttributesAttributes where
  arbitrary = sized genGetAttributesAttributes

genGetAttributesAttributes :: Int -> Gen GetAttributesAttributes
genGetAttributesAttributes n =
  GetAttributesAttributes
    <$> arbitrary -- getAttributesAttributesName :: Text
    <*> arbitrary -- getAttributesAttributesCategory :: E'Category
    <*> arbitraryReducedMaybe n -- getAttributesAttributesType :: Maybe E'Type6
    <*> arbitraryReducedMaybe n -- getAttributesAttributesEnumeration :: Maybe [GetAttributesEnumeration]
    <*> arbitraryReducedMaybe n -- getAttributesAttributesCalculatedValue :: Maybe Text
  
instance Arbitrary GetAttributesEnumeration where
  arbitrary = sized genGetAttributesEnumeration

genGetAttributesEnumeration :: Int -> Gen GetAttributesEnumeration
genGetAttributesEnumeration n =
  GetAttributesEnumeration
    <$> arbitrary -- getAttributesEnumerationValue :: Integer
    <*> arbitrary -- getAttributesEnumerationLabel :: Text
  
instance Arbitrary GetBlockedDomains where
  arbitrary = sized genGetBlockedDomains

genGetBlockedDomains :: Int -> Gen GetBlockedDomains
genGetBlockedDomains n =
  GetBlockedDomains
    <$> arbitrary -- getBlockedDomainsDomains :: [Text]
  
instance Arbitrary GetCampaignOverview where
  arbitrary = sized genGetCampaignOverview

genGetCampaignOverview :: Int -> Gen GetCampaignOverview
genGetCampaignOverview n =
  GetCampaignOverview
    <$> arbitrary -- getCampaignOverviewId :: Integer
    <*> arbitrary -- getCampaignOverviewName :: Text
    <*> arbitraryReducedMaybe n -- getCampaignOverviewSubject :: Maybe Text
    <*> arbitrary -- getCampaignOverviewType :: E'Type
    <*> arbitrary -- getCampaignOverviewStatus :: E'Status3
    <*> arbitraryReducedMaybe n -- getCampaignOverviewScheduledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getCampaignOverviewAbTesting :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getCampaignOverviewSubjectA :: Maybe Text
    <*> arbitraryReducedMaybe n -- getCampaignOverviewSubjectB :: Maybe Text
    <*> arbitraryReducedMaybe n -- getCampaignOverviewSplitRule :: Maybe Int
    <*> arbitraryReducedMaybe n -- getCampaignOverviewWinnerCriteria :: Maybe Text
    <*> arbitraryReducedMaybe n -- getCampaignOverviewWinnerDelay :: Maybe Int
    <*> arbitraryReducedMaybe n -- getCampaignOverviewSendAtBestTime :: Maybe Bool
  
instance Arbitrary GetCampaignRecipients where
  arbitrary = sized genGetCampaignRecipients

genGetCampaignRecipients :: Int -> Gen GetCampaignRecipients
genGetCampaignRecipients n =
  GetCampaignRecipients
    <$> arbitrary -- getCampaignRecipientsLists :: [Integer]
    <*> arbitrary -- getCampaignRecipientsExclusionLists :: [Integer]
  
instance Arbitrary GetCampaignStats where
  arbitrary = sized genGetCampaignStats

genGetCampaignStats :: Int -> Gen GetCampaignStats
genGetCampaignStats n =
  GetCampaignStats
    <$> arbitraryReducedMaybe n -- getCampaignStatsListId :: Maybe Integer
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
    <*> arbitraryReducedMaybe n -- getCampaignStatsDeferred :: Maybe Integer
    <*> arbitraryReducedMaybe n -- getCampaignStatsReturnBounce :: Maybe Integer
  
instance Arbitrary GetChildAccountCreationStatus where
  arbitrary = sized genGetChildAccountCreationStatus

genGetChildAccountCreationStatus :: Int -> Gen GetChildAccountCreationStatus
genGetChildAccountCreationStatus n =
  GetChildAccountCreationStatus
    <$> arbitrary -- getChildAccountCreationStatusChildAccountCreated :: Bool
  
instance Arbitrary GetChildDomain where
  arbitrary = sized genGetChildDomain

genGetChildDomain :: Int -> Gen GetChildDomain
genGetChildDomain n =
  GetChildDomain
    <$> arbitrary -- getChildDomainDomain :: Text
    <*> arbitrary -- getChildDomainActive :: Bool
  
instance Arbitrary GetChildInfo where
  arbitrary = sized genGetChildInfo

genGetChildInfo :: Int -> Gen GetChildInfo
genGetChildInfo n =
  GetChildInfo
    <$> arbitrary -- getChildInfoEmail :: Text
    <*> arbitrary -- getChildInfoFirstName :: Text
    <*> arbitrary -- getChildInfoLastName :: Text
    <*> arbitrary -- getChildInfoCompanyName :: Text
    <*> arbitraryReducedMaybe n -- getChildInfoCredits :: Maybe GetChildInfoAllOfCredits
    <*> arbitraryReducedMaybe n -- getChildInfoStatistics :: Maybe GetChildInfoAllOfStatistics
    <*> arbitrary -- getChildInfoPassword :: Text
    <*> arbitraryReducedMaybe n -- getChildInfoIps :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- getChildInfoApiKeys :: Maybe GetChildInfoAllOfApiKeys
  
instance Arbitrary GetChildInfoAllOf where
  arbitrary = sized genGetChildInfoAllOf

genGetChildInfoAllOf :: Int -> Gen GetChildInfoAllOf
genGetChildInfoAllOf n =
  GetChildInfoAllOf
    <$> arbitraryReducedMaybe n -- getChildInfoAllOfCredits :: Maybe GetChildInfoAllOfCredits
    <*> arbitraryReducedMaybe n -- getChildInfoAllOfStatistics :: Maybe GetChildInfoAllOfStatistics
    <*> arbitrary -- getChildInfoAllOfPassword :: Text
    <*> arbitraryReducedMaybe n -- getChildInfoAllOfIps :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- getChildInfoAllOfApiKeys :: Maybe GetChildInfoAllOfApiKeys
  
instance Arbitrary GetChildInfoAllOfApiKeys where
  arbitrary = sized genGetChildInfoAllOfApiKeys

genGetChildInfoAllOfApiKeys :: Int -> Gen GetChildInfoAllOfApiKeys
genGetChildInfoAllOfApiKeys n =
  GetChildInfoAllOfApiKeys
    <$> arbitraryReduced n -- getChildInfoAllOfApiKeysV2 :: [GetChildInfoAllOfApiKeysV2]
    <*> arbitraryReducedMaybe n -- getChildInfoAllOfApiKeysV3 :: Maybe [GetChildInfoAllOfApiKeysV3]
  
instance Arbitrary GetChildInfoAllOfApiKeysV2 where
  arbitrary = sized genGetChildInfoAllOfApiKeysV2

genGetChildInfoAllOfApiKeysV2 :: Int -> Gen GetChildInfoAllOfApiKeysV2
genGetChildInfoAllOfApiKeysV2 n =
  GetChildInfoAllOfApiKeysV2
    <$> arbitrary -- getChildInfoAllOfApiKeysV2Name :: Text
    <*> arbitrary -- getChildInfoAllOfApiKeysV2Key :: Text
  
instance Arbitrary GetChildInfoAllOfApiKeysV3 where
  arbitrary = sized genGetChildInfoAllOfApiKeysV3

genGetChildInfoAllOfApiKeysV3 :: Int -> Gen GetChildInfoAllOfApiKeysV3
genGetChildInfoAllOfApiKeysV3 n =
  GetChildInfoAllOfApiKeysV3
    <$> arbitrary -- getChildInfoAllOfApiKeysV3Name :: Text
    <*> arbitrary -- getChildInfoAllOfApiKeysV3Key :: Text
  
instance Arbitrary GetChildInfoAllOfCredits where
  arbitrary = sized genGetChildInfoAllOfCredits

genGetChildInfoAllOfCredits :: Int -> Gen GetChildInfoAllOfCredits
genGetChildInfoAllOfCredits n =
  GetChildInfoAllOfCredits
    <$> arbitraryReducedMaybe n -- getChildInfoAllOfCreditsEmailCredits :: Maybe Integer
    <*> arbitraryReducedMaybe n -- getChildInfoAllOfCreditsSmsCredits :: Maybe Integer
  
instance Arbitrary GetChildInfoAllOfStatistics where
  arbitrary = sized genGetChildInfoAllOfStatistics

genGetChildInfoAllOfStatistics :: Int -> Gen GetChildInfoAllOfStatistics
genGetChildInfoAllOfStatistics n =
  GetChildInfoAllOfStatistics
    <$> arbitraryReducedMaybe n -- getChildInfoAllOfStatisticsPreviousMonthTotalSent :: Maybe Integer
    <*> arbitraryReducedMaybe n -- getChildInfoAllOfStatisticsCurrentMonthTotalSent :: Maybe Integer
    <*> arbitraryReducedMaybe n -- getChildInfoAllOfStatisticsTotalSent :: Maybe Integer
  
instance Arbitrary GetChildrenList where
  arbitrary = sized genGetChildrenList

genGetChildrenList :: Int -> Gen GetChildrenList
genGetChildrenList n =
  GetChildrenList
    <$> arbitraryReducedMaybe n -- getChildrenListChildren :: Maybe [GetChildInfo]
    <*> arbitrary -- getChildrenListCount :: Integer
  
instance Arbitrary GetClient where
  arbitrary = sized genGetClient

genGetClient :: Int -> Gen GetClient
genGetClient n =
  GetClient
    <$> arbitrary -- getClientEmail :: Text
    <*> arbitrary -- getClientFirstName :: Text
    <*> arbitrary -- getClientLastName :: Text
    <*> arbitrary -- getClientCompanyName :: Text
  
instance Arbitrary GetContactCampaignStats where
  arbitrary = sized genGetContactCampaignStats

genGetContactCampaignStats :: Int -> Gen GetContactCampaignStats
genGetContactCampaignStats n =
  GetContactCampaignStats
    <$> arbitraryReducedMaybe n -- getContactCampaignStatsMessagesSent :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getContactCampaignStatsHardBounces :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getContactCampaignStatsSoftBounces :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getContactCampaignStatsComplaints :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getContactCampaignStatsUnsubscriptions :: Maybe GetContactCampaignStatsUnsubscriptions
    <*> arbitraryReducedMaybe n -- getContactCampaignStatsOpened :: Maybe [GetContactCampaignStatsOpened]
    <*> arbitraryReducedMaybe n -- getContactCampaignStatsClicked :: Maybe [GetContactCampaignStatsClicked]
    <*> arbitraryReducedMaybe n -- getContactCampaignStatsTransacAttributes :: Maybe [GetContactCampaignStatsTransacAttributes]
  
instance Arbitrary GetContactCampaignStatsClicked where
  arbitrary = sized genGetContactCampaignStatsClicked

genGetContactCampaignStatsClicked :: Int -> Gen GetContactCampaignStatsClicked
genGetContactCampaignStatsClicked n =
  GetContactCampaignStatsClicked
    <$> arbitrary -- getContactCampaignStatsClickedCampaignId :: Integer
    <*> arbitraryReduced n -- getContactCampaignStatsClickedLinks :: [GetExtendedContactDetailsAllOfStatisticsLinks]
  
instance Arbitrary GetContactCampaignStatsOpened where
  arbitrary = sized genGetContactCampaignStatsOpened

genGetContactCampaignStatsOpened :: Int -> Gen GetContactCampaignStatsOpened
genGetContactCampaignStatsOpened n =
  GetContactCampaignStatsOpened
    <$> arbitrary -- getContactCampaignStatsOpenedCampaignId :: Integer
    <*> arbitrary -- getContactCampaignStatsOpenedCount :: Integer
    <*> arbitraryReduced n -- getContactCampaignStatsOpenedEventTime :: DateTime
    <*> arbitrary -- getContactCampaignStatsOpenedIp :: Text
  
instance Arbitrary GetContactCampaignStatsTransacAttributes where
  arbitrary = sized genGetContactCampaignStatsTransacAttributes

genGetContactCampaignStatsTransacAttributes :: Int -> Gen GetContactCampaignStatsTransacAttributes
genGetContactCampaignStatsTransacAttributes n =
  GetContactCampaignStatsTransacAttributes
    <$> arbitraryReduced n -- getContactCampaignStatsTransacAttributesOrderDate :: Date
    <*> arbitrary -- getContactCampaignStatsTransacAttributesOrderPrice :: Float
    <*> arbitrary -- getContactCampaignStatsTransacAttributesOrderId :: Integer
  
instance Arbitrary GetContactCampaignStatsUnsubscriptions where
  arbitrary = sized genGetContactCampaignStatsUnsubscriptions

genGetContactCampaignStatsUnsubscriptions :: Int -> Gen GetContactCampaignStatsUnsubscriptions
genGetContactCampaignStatsUnsubscriptions n =
  GetContactCampaignStatsUnsubscriptions
    <$> arbitraryReduced n -- getContactCampaignStatsUnsubscriptionsUserUnsubscription :: [GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription]
    <*> arbitraryReduced n -- getContactCampaignStatsUnsubscriptionsAdminUnsubscription :: [GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription]
  
instance Arbitrary GetContactDetails where
  arbitrary = sized genGetContactDetails

genGetContactDetails :: Int -> Gen GetContactDetails
genGetContactDetails n =
  GetContactDetails
    <$> arbitrary -- getContactDetailsEmail :: Text
    <*> arbitrary -- getContactDetailsId :: Integer
    <*> arbitrary -- getContactDetailsEmailBlacklisted :: Bool
    <*> arbitrary -- getContactDetailsSmsBlacklisted :: Bool
    <*> arbitraryReduced n -- getContactDetailsCreatedAt :: DateTime
    <*> arbitraryReduced n -- getContactDetailsModifiedAt :: DateTime
    <*> arbitrary -- getContactDetailsListIds :: [Integer]
    <*> arbitraryReducedMaybe n -- getContactDetailsListUnsubscribed :: Maybe [Integer]
    <*> arbitraryReduced n -- getContactDetailsAttributes :: A.Value
  
instance Arbitrary GetContacts where
  arbitrary = sized genGetContacts

genGetContacts :: Int -> Gen GetContacts
genGetContacts n =
  GetContacts
    <$> arbitraryReduced n -- getContactsContacts :: [GetContactDetails]
    <*> arbitrary -- getContactsCount :: Integer
  
instance Arbitrary GetDeviceBrowserStats where
  arbitrary = sized genGetDeviceBrowserStats

genGetDeviceBrowserStats :: Int -> Gen GetDeviceBrowserStats
genGetDeviceBrowserStats n =
  GetDeviceBrowserStats
    <$> arbitrary -- getDeviceBrowserStatsClickers :: Integer
    <*> arbitrary -- getDeviceBrowserStatsUniqueClicks :: Integer
    <*> arbitrary -- getDeviceBrowserStatsViewed :: Integer
    <*> arbitrary -- getDeviceBrowserStatsUniqueViews :: Integer
  
instance Arbitrary GetEmailCampaign where
  arbitrary = sized genGetEmailCampaign

genGetEmailCampaign :: Int -> Gen GetEmailCampaign
genGetEmailCampaign n =
  GetEmailCampaign
    <$> arbitrary -- getEmailCampaignId :: Integer
    <*> arbitrary -- getEmailCampaignName :: Text
    <*> arbitraryReducedMaybe n -- getEmailCampaignSubject :: Maybe Text
    <*> arbitrary -- getEmailCampaignType :: E'Type
    <*> arbitrary -- getEmailCampaignStatus :: E'Status3
    <*> arbitraryReducedMaybe n -- getEmailCampaignScheduledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getEmailCampaignAbTesting :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getEmailCampaignSubjectA :: Maybe Text
    <*> arbitraryReducedMaybe n -- getEmailCampaignSubjectB :: Maybe Text
    <*> arbitraryReducedMaybe n -- getEmailCampaignSplitRule :: Maybe Int
    <*> arbitraryReducedMaybe n -- getEmailCampaignWinnerCriteria :: Maybe Text
    <*> arbitraryReducedMaybe n -- getEmailCampaignWinnerDelay :: Maybe Int
    <*> arbitraryReducedMaybe n -- getEmailCampaignSendAtBestTime :: Maybe Bool
    <*> arbitrary -- getEmailCampaignTestSent :: Bool
    <*> arbitrary -- getEmailCampaignHeader :: Text
    <*> arbitrary -- getEmailCampaignFooter :: Text
    <*> arbitraryReduced n -- getEmailCampaignSender :: GetExtendedCampaignOverviewAllOfSender
    <*> arbitrary -- getEmailCampaignReplyTo :: Text
    <*> arbitrary -- getEmailCampaignToField :: Text
    <*> arbitrary -- getEmailCampaignHtmlContent :: Text
    <*> arbitraryReducedMaybe n -- getEmailCampaignShareLink :: Maybe Text
    <*> arbitrary -- getEmailCampaignTag :: Text
    <*> arbitraryReduced n -- getEmailCampaignCreatedAt :: DateTime
    <*> arbitraryReduced n -- getEmailCampaignModifiedAt :: DateTime
    <*> arbitraryReducedMaybe n -- getEmailCampaignInlineImageActivation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getEmailCampaignMirrorActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getEmailCampaignRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getEmailCampaignSentDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getEmailCampaignReturnBounce :: Maybe Integer
    <*> arbitraryReduced n -- getEmailCampaignRecipients :: GetCampaignRecipients
    <*> arbitraryReduced n -- getEmailCampaignStatistics :: GetExtendedCampaignStats
  
instance Arbitrary GetEmailCampaignAllOf where
  arbitrary = sized genGetEmailCampaignAllOf

genGetEmailCampaignAllOf :: Int -> Gen GetEmailCampaignAllOf
genGetEmailCampaignAllOf n =
  GetEmailCampaignAllOf
    <$> arbitraryReduced n -- getEmailCampaignAllOfRecipients :: GetCampaignRecipients
    <*> arbitraryReduced n -- getEmailCampaignAllOfStatistics :: GetExtendedCampaignStats
  
instance Arbitrary GetEmailCampaigns where
  arbitrary = sized genGetEmailCampaigns

genGetEmailCampaigns :: Int -> Gen GetEmailCampaigns
genGetEmailCampaigns n =
  GetEmailCampaigns
    <$> arbitraryReducedMaybe n -- getEmailCampaignsCampaigns :: Maybe [GetExtendedCampaignOverview]
    <*> arbitrary -- getEmailCampaignsCount :: Integer
  
instance Arbitrary GetEmailEventReport where
  arbitrary = sized genGetEmailEventReport

genGetEmailEventReport :: Int -> Gen GetEmailEventReport
genGetEmailEventReport n =
  GetEmailEventReport
    <$> arbitraryReducedMaybe n -- getEmailEventReportEvents :: Maybe [GetEmailEventReportEvents]
  
instance Arbitrary GetEmailEventReportEvents where
  arbitrary = sized genGetEmailEventReportEvents

genGetEmailEventReportEvents :: Int -> Gen GetEmailEventReportEvents
genGetEmailEventReportEvents n =
  GetEmailEventReportEvents
    <$> arbitrary -- getEmailEventReportEventsEmail :: Text
    <*> arbitraryReduced n -- getEmailEventReportEventsDate :: DateTime
    <*> arbitraryReducedMaybe n -- getEmailEventReportEventsSubject :: Maybe Text
    <*> arbitrary -- getEmailEventReportEventsMessageId :: Text
    <*> arbitrary -- getEmailEventReportEventsEvent :: E'Event
    <*> arbitraryReducedMaybe n -- getEmailEventReportEventsReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- getEmailEventReportEventsTag :: Maybe Text
    <*> arbitraryReducedMaybe n -- getEmailEventReportEventsIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- getEmailEventReportEventsLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- getEmailEventReportEventsFrom :: Maybe Text
  
instance Arbitrary GetExtendedCampaignOverview where
  arbitrary = sized genGetExtendedCampaignOverview

genGetExtendedCampaignOverview :: Int -> Gen GetExtendedCampaignOverview
genGetExtendedCampaignOverview n =
  GetExtendedCampaignOverview
    <$> arbitrary -- getExtendedCampaignOverviewId :: Integer
    <*> arbitrary -- getExtendedCampaignOverviewName :: Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewSubject :: Maybe Text
    <*> arbitrary -- getExtendedCampaignOverviewType :: E'Type
    <*> arbitrary -- getExtendedCampaignOverviewStatus :: E'Status3
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewScheduledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAbTesting :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewSubjectA :: Maybe Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewSubjectB :: Maybe Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewSplitRule :: Maybe Int
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewWinnerCriteria :: Maybe Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewWinnerDelay :: Maybe Int
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewSendAtBestTime :: Maybe Bool
    <*> arbitrary -- getExtendedCampaignOverviewTestSent :: Bool
    <*> arbitrary -- getExtendedCampaignOverviewHeader :: Text
    <*> arbitrary -- getExtendedCampaignOverviewFooter :: Text
    <*> arbitraryReduced n -- getExtendedCampaignOverviewSender :: GetExtendedCampaignOverviewAllOfSender
    <*> arbitrary -- getExtendedCampaignOverviewReplyTo :: Text
    <*> arbitrary -- getExtendedCampaignOverviewToField :: Text
    <*> arbitrary -- getExtendedCampaignOverviewHtmlContent :: Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewShareLink :: Maybe Text
    <*> arbitrary -- getExtendedCampaignOverviewTag :: Text
    <*> arbitraryReduced n -- getExtendedCampaignOverviewCreatedAt :: DateTime
    <*> arbitraryReduced n -- getExtendedCampaignOverviewModifiedAt :: DateTime
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewInlineImageActivation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewMirrorActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewSentDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewReturnBounce :: Maybe Integer
  
instance Arbitrary GetExtendedCampaignOverviewAllOf where
  arbitrary = sized genGetExtendedCampaignOverviewAllOf

genGetExtendedCampaignOverviewAllOf :: Int -> Gen GetExtendedCampaignOverviewAllOf
genGetExtendedCampaignOverviewAllOf n =
  GetExtendedCampaignOverviewAllOf
    <$> arbitrary -- getExtendedCampaignOverviewAllOfTestSent :: Bool
    <*> arbitrary -- getExtendedCampaignOverviewAllOfHeader :: Text
    <*> arbitrary -- getExtendedCampaignOverviewAllOfFooter :: Text
    <*> arbitraryReduced n -- getExtendedCampaignOverviewAllOfSender :: GetExtendedCampaignOverviewAllOfSender
    <*> arbitrary -- getExtendedCampaignOverviewAllOfReplyTo :: Text
    <*> arbitrary -- getExtendedCampaignOverviewAllOfToField :: Text
    <*> arbitrary -- getExtendedCampaignOverviewAllOfHtmlContent :: Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfShareLink :: Maybe Text
    <*> arbitrary -- getExtendedCampaignOverviewAllOfTag :: Text
    <*> arbitraryReduced n -- getExtendedCampaignOverviewAllOfCreatedAt :: DateTime
    <*> arbitraryReduced n -- getExtendedCampaignOverviewAllOfModifiedAt :: DateTime
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfInlineImageActivation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfMirrorActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfSentDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfReturnBounce :: Maybe Integer
  
instance Arbitrary GetExtendedCampaignOverviewAllOfSender where
  arbitrary = sized genGetExtendedCampaignOverviewAllOfSender

genGetExtendedCampaignOverviewAllOfSender :: Int -> Gen GetExtendedCampaignOverviewAllOfSender
genGetExtendedCampaignOverviewAllOfSender n =
  GetExtendedCampaignOverviewAllOfSender
    <$> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfSenderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfSenderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- getExtendedCampaignOverviewAllOfSenderId :: Maybe Text
  
instance Arbitrary GetExtendedCampaignStats where
  arbitrary = sized genGetExtendedCampaignStats

genGetExtendedCampaignStats :: Int -> Gen GetExtendedCampaignStats
genGetExtendedCampaignStats n =
  GetExtendedCampaignStats
    <$> arbitraryReduced n -- getExtendedCampaignStatsGlobalStats :: GetCampaignStats
    <*> arbitraryReduced n -- getExtendedCampaignStatsCampaignStats :: [GetCampaignStats]
    <*> arbitrary -- getExtendedCampaignStatsMirrorClick :: Integer
    <*> arbitrary -- getExtendedCampaignStatsRemaining :: Integer
    <*> arbitraryReduced n -- getExtendedCampaignStatsLinksStats :: A.Value
    <*> arbitraryReduced n -- getExtendedCampaignStatsStatsByDomain :: (Map.Map String GetCampaignStats)
    <*> arbitraryReduced n -- getExtendedCampaignStatsStatsByDevice :: GetStatsByDevice
    <*> arbitraryReduced n -- getExtendedCampaignStatsStatsByBrowser :: (Map.Map String GetDeviceBrowserStats)
  
instance Arbitrary GetExtendedClient where
  arbitrary = sized genGetExtendedClient

genGetExtendedClient :: Int -> Gen GetExtendedClient
genGetExtendedClient n =
  GetExtendedClient
    <$> arbitrary -- getExtendedClientEmail :: Text
    <*> arbitrary -- getExtendedClientFirstName :: Text
    <*> arbitrary -- getExtendedClientLastName :: Text
    <*> arbitrary -- getExtendedClientCompanyName :: Text
    <*> arbitraryReduced n -- getExtendedClientAddress :: GetExtendedClientAllOfAddress
  
instance Arbitrary GetExtendedClientAllOf where
  arbitrary = sized genGetExtendedClientAllOf

genGetExtendedClientAllOf :: Int -> Gen GetExtendedClientAllOf
genGetExtendedClientAllOf n =
  GetExtendedClientAllOf
    <$> arbitraryReduced n -- getExtendedClientAllOfAddress :: GetExtendedClientAllOfAddress
  
instance Arbitrary GetExtendedClientAllOfAddress where
  arbitrary = sized genGetExtendedClientAllOfAddress

genGetExtendedClientAllOfAddress :: Int -> Gen GetExtendedClientAllOfAddress
genGetExtendedClientAllOfAddress n =
  GetExtendedClientAllOfAddress
    <$> arbitrary -- getExtendedClientAllOfAddressStreet :: Text
    <*> arbitrary -- getExtendedClientAllOfAddressCity :: Text
    <*> arbitrary -- getExtendedClientAllOfAddressZipCode :: Text
    <*> arbitrary -- getExtendedClientAllOfAddressCountry :: Text
  
instance Arbitrary GetExtendedContactDetails where
  arbitrary = sized genGetExtendedContactDetails

genGetExtendedContactDetails :: Int -> Gen GetExtendedContactDetails
genGetExtendedContactDetails n =
  GetExtendedContactDetails
    <$> arbitrary -- getExtendedContactDetailsEmail :: Text
    <*> arbitrary -- getExtendedContactDetailsId :: Integer
    <*> arbitrary -- getExtendedContactDetailsEmailBlacklisted :: Bool
    <*> arbitrary -- getExtendedContactDetailsSmsBlacklisted :: Bool
    <*> arbitraryReduced n -- getExtendedContactDetailsCreatedAt :: DateTime
    <*> arbitraryReduced n -- getExtendedContactDetailsModifiedAt :: DateTime
    <*> arbitrary -- getExtendedContactDetailsListIds :: [Integer]
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsListUnsubscribed :: Maybe [Integer]
    <*> arbitraryReduced n -- getExtendedContactDetailsAttributes :: A.Value
    <*> arbitraryReduced n -- getExtendedContactDetailsStatistics :: GetExtendedContactDetailsAllOfStatistics
  
instance Arbitrary GetExtendedContactDetailsAllOf where
  arbitrary = sized genGetExtendedContactDetailsAllOf

genGetExtendedContactDetailsAllOf :: Int -> Gen GetExtendedContactDetailsAllOf
genGetExtendedContactDetailsAllOf n =
  GetExtendedContactDetailsAllOf
    <$> arbitraryReduced n -- getExtendedContactDetailsAllOfStatistics :: GetExtendedContactDetailsAllOfStatistics
  
instance Arbitrary GetExtendedContactDetailsAllOfStatistics where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatistics

genGetExtendedContactDetailsAllOfStatistics :: Int -> Gen GetExtendedContactDetailsAllOfStatistics
genGetExtendedContactDetailsAllOfStatistics n =
  GetExtendedContactDetailsAllOfStatistics
    <$> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsMessagesSent :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsHardBounces :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsSoftBounces :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsComplaints :: Maybe [GetExtendedContactDetailsAllOfStatisticsMessagesSent]
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsUnsubscriptions :: Maybe GetExtendedContactDetailsAllOfStatisticsUnsubscriptions
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsOpened :: Maybe [GetExtendedContactDetailsAllOfStatisticsOpened]
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsClicked :: Maybe [GetExtendedContactDetailsAllOfStatisticsClicked]
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsTransacAttributes :: Maybe [A.Value]
  
instance Arbitrary GetExtendedContactDetailsAllOfStatisticsClicked where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatisticsClicked

genGetExtendedContactDetailsAllOfStatisticsClicked :: Int -> Gen GetExtendedContactDetailsAllOfStatisticsClicked
genGetExtendedContactDetailsAllOfStatisticsClicked n =
  GetExtendedContactDetailsAllOfStatisticsClicked
    <$> arbitrary -- getExtendedContactDetailsAllOfStatisticsClickedCampaignId :: Integer
    <*> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsClickedLinks :: [GetExtendedContactDetailsAllOfStatisticsLinks]
  
instance Arbitrary GetExtendedContactDetailsAllOfStatisticsLinks where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatisticsLinks

genGetExtendedContactDetailsAllOfStatisticsLinks :: Int -> Gen GetExtendedContactDetailsAllOfStatisticsLinks
genGetExtendedContactDetailsAllOfStatisticsLinks n =
  GetExtendedContactDetailsAllOfStatisticsLinks
    <$> arbitrary -- getExtendedContactDetailsAllOfStatisticsLinksCount :: Integer
    <*> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsLinksEventTime :: DateTime
    <*> arbitrary -- getExtendedContactDetailsAllOfStatisticsLinksIp :: Text
    <*> arbitrary -- getExtendedContactDetailsAllOfStatisticsLinksUrl :: Text
  
instance Arbitrary GetExtendedContactDetailsAllOfStatisticsMessagesSent where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatisticsMessagesSent

genGetExtendedContactDetailsAllOfStatisticsMessagesSent :: Int -> Gen GetExtendedContactDetailsAllOfStatisticsMessagesSent
genGetExtendedContactDetailsAllOfStatisticsMessagesSent n =
  GetExtendedContactDetailsAllOfStatisticsMessagesSent
    <$> arbitrary -- getExtendedContactDetailsAllOfStatisticsMessagesSentCampaignId :: Integer
    <*> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsMessagesSentEventTime :: DateTime
  
instance Arbitrary GetExtendedContactDetailsAllOfStatisticsOpened where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatisticsOpened

genGetExtendedContactDetailsAllOfStatisticsOpened :: Int -> Gen GetExtendedContactDetailsAllOfStatisticsOpened
genGetExtendedContactDetailsAllOfStatisticsOpened n =
  GetExtendedContactDetailsAllOfStatisticsOpened
    <$> arbitrary -- getExtendedContactDetailsAllOfStatisticsOpenedCampaignId :: Integer
    <*> arbitrary -- getExtendedContactDetailsAllOfStatisticsOpenedCount :: Integer
    <*> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsOpenedEventTime :: DateTime
    <*> arbitrary -- getExtendedContactDetailsAllOfStatisticsOpenedIp :: Text
  
instance Arbitrary GetExtendedContactDetailsAllOfStatisticsUnsubscriptions where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatisticsUnsubscriptions

genGetExtendedContactDetailsAllOfStatisticsUnsubscriptions :: Int -> Gen GetExtendedContactDetailsAllOfStatisticsUnsubscriptions
genGetExtendedContactDetailsAllOfStatisticsUnsubscriptions n =
  GetExtendedContactDetailsAllOfStatisticsUnsubscriptions
    <$> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription :: [GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription]
    <*> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription :: [GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription]
  
instance Arbitrary GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription

genGetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription :: Int -> Gen GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription
genGetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription n =
  GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscription
    <$> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscriptionEventTime :: DateTime
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsUnsubscriptionsAdminUnsubscriptionIp :: Maybe Text
  
instance Arbitrary GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription where
  arbitrary = sized genGetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription

genGetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription :: Int -> Gen GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription
genGetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription n =
  GetExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscription
    <$> arbitrary -- getExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscriptionCampaignId :: Integer
    <*> arbitraryReduced n -- getExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscriptionEventTime :: DateTime
    <*> arbitraryReducedMaybe n -- getExtendedContactDetailsAllOfStatisticsUnsubscriptionsUserUnsubscriptionIp :: Maybe Text
  
instance Arbitrary GetExtendedList where
  arbitrary = sized genGetExtendedList

genGetExtendedList :: Int -> Gen GetExtendedList
genGetExtendedList n =
  GetExtendedList
    <$> arbitrary -- getExtendedListId :: Integer
    <*> arbitrary -- getExtendedListName :: Text
    <*> arbitrary -- getExtendedListTotalBlacklisted :: Integer
    <*> arbitrary -- getExtendedListTotalSubscribers :: Integer
    <*> arbitrary -- getExtendedListFolderId :: Integer
    <*> arbitraryReduced n -- getExtendedListCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- getExtendedListCampaignStats :: Maybe [GetExtendedListAllOfCampaignStats]
    <*> arbitraryReducedMaybe n -- getExtendedListDynamicList :: Maybe Bool
  
instance Arbitrary GetExtendedListAllOf where
  arbitrary = sized genGetExtendedListAllOf

genGetExtendedListAllOf :: Int -> Gen GetExtendedListAllOf
genGetExtendedListAllOf n =
  GetExtendedListAllOf
    <$> arbitrary -- getExtendedListAllOfFolderId :: Integer
    <*> arbitraryReduced n -- getExtendedListAllOfCreatedAt :: DateTime
    <*> arbitraryReducedMaybe n -- getExtendedListAllOfCampaignStats :: Maybe [GetExtendedListAllOfCampaignStats]
    <*> arbitraryReducedMaybe n -- getExtendedListAllOfDynamicList :: Maybe Bool
  
instance Arbitrary GetExtendedListAllOfCampaignStats where
  arbitrary = sized genGetExtendedListAllOfCampaignStats

genGetExtendedListAllOfCampaignStats :: Int -> Gen GetExtendedListAllOfCampaignStats
genGetExtendedListAllOfCampaignStats n =
  GetExtendedListAllOfCampaignStats
    <$> arbitrary -- getExtendedListAllOfCampaignStatsCampaignId :: Integer
    <*> arbitraryReduced n -- getExtendedListAllOfCampaignStatsStats :: GetCampaignStats
  
instance Arbitrary GetFolder where
  arbitrary = sized genGetFolder

genGetFolder :: Int -> Gen GetFolder
genGetFolder n =
  GetFolder
    <$> arbitrary -- getFolderId :: Integer
    <*> arbitrary -- getFolderName :: Text
    <*> arbitrary -- getFolderTotalBlacklisted :: Integer
    <*> arbitrary -- getFolderTotalSubscribers :: Integer
    <*> arbitrary -- getFolderUniqueSubscribers :: Integer
  
instance Arbitrary GetFolderLists where
  arbitrary = sized genGetFolderLists

genGetFolderLists :: Int -> Gen GetFolderLists
genGetFolderLists n =
  GetFolderLists
    <$> arbitraryReduced n -- getFolderListsLists :: [GetList]
    <*> arbitrary -- getFolderListsCount :: Integer
  
instance Arbitrary GetFolders where
  arbitrary = sized genGetFolders

genGetFolders :: Int -> Gen GetFolders
genGetFolders n =
  GetFolders
    <$> arbitraryReducedMaybe n -- getFoldersFolders :: Maybe [GetFolder]
    <*> arbitraryReducedMaybe n -- getFoldersCount :: Maybe Integer
  
instance Arbitrary GetIp where
  arbitrary = sized genGetIp

genGetIp :: Int -> Gen GetIp
genGetIp n =
  GetIp
    <$> arbitrary -- getIpId :: Integer
    <*> arbitrary -- getIpIp :: Text
    <*> arbitrary -- getIpActive :: Bool
    <*> arbitrary -- getIpDomain :: Text
  
instance Arbitrary GetIpFromSender where
  arbitrary = sized genGetIpFromSender

genGetIpFromSender :: Int -> Gen GetIpFromSender
genGetIpFromSender n =
  GetIpFromSender
    <$> arbitrary -- getIpFromSenderId :: Integer
    <*> arbitrary -- getIpFromSenderIp :: Text
    <*> arbitrary -- getIpFromSenderDomain :: Text
    <*> arbitrary -- getIpFromSenderWeight :: Integer
  
instance Arbitrary GetIps where
  arbitrary = sized genGetIps

genGetIps :: Int -> Gen GetIps
genGetIps n =
  GetIps
    <$> arbitraryReduced n -- getIpsIps :: [GetIp]
  
instance Arbitrary GetIpsFromSender where
  arbitrary = sized genGetIpsFromSender

genGetIpsFromSender :: Int -> Gen GetIpsFromSender
genGetIpsFromSender n =
  GetIpsFromSender
    <$> arbitraryReduced n -- getIpsFromSenderIps :: [GetIpFromSender]
  
instance Arbitrary GetList where
  arbitrary = sized genGetList

genGetList :: Int -> Gen GetList
genGetList n =
  GetList
    <$> arbitrary -- getListId :: Integer
    <*> arbitrary -- getListName :: Text
    <*> arbitrary -- getListTotalBlacklisted :: Integer
    <*> arbitrary -- getListTotalSubscribers :: Integer
  
instance Arbitrary GetLists where
  arbitrary = sized genGetLists

genGetLists :: Int -> Gen GetLists
genGetLists n =
  GetLists
    <$> arbitraryReduced n -- getListsLists :: [GetList]
    <*> arbitrary -- getListsCount :: Integer
  
instance Arbitrary GetProcess where
  arbitrary = sized genGetProcess

genGetProcess :: Int -> Gen GetProcess
genGetProcess n =
  GetProcess
    <$> arbitrary -- getProcessId :: Integer
    <*> arbitrary -- getProcessStatus :: E'Status
    <*> arbitrary -- getProcessName :: Text
    <*> arbitraryReducedMaybe n -- getProcessExportUrl :: Maybe Text
  
instance Arbitrary GetProcesses where
  arbitrary = sized genGetProcesses

genGetProcesses :: Int -> Gen GetProcesses
genGetProcesses n =
  GetProcesses
    <$> arbitraryReducedMaybe n -- getProcessesProcesses :: Maybe [GetProcess]
    <*> arbitrary -- getProcessesCount :: Integer
  
instance Arbitrary GetReports where
  arbitrary = sized genGetReports

genGetReports :: Int -> Gen GetReports
genGetReports n =
  GetReports
    <$> arbitraryReducedMaybe n -- getReportsReports :: Maybe [GetReportsReports]
  
instance Arbitrary GetReportsReports where
  arbitrary = sized genGetReportsReports

genGetReportsReports :: Int -> Gen GetReportsReports
genGetReportsReports n =
  GetReportsReports
    <$> arbitraryReduced n -- getReportsReportsDate :: Date
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
  arbitrary = sized genGetSendersList

genGetSendersList :: Int -> Gen GetSendersList
genGetSendersList n =
  GetSendersList
    <$> arbitraryReducedMaybe n -- getSendersListSenders :: Maybe [GetSendersListSenders]
  
instance Arbitrary GetSendersListIps where
  arbitrary = sized genGetSendersListIps

genGetSendersListIps :: Int -> Gen GetSendersListIps
genGetSendersListIps n =
  GetSendersListIps
    <$> arbitrary -- getSendersListIpsIp :: Text
    <*> arbitrary -- getSendersListIpsDomain :: Text
    <*> arbitrary -- getSendersListIpsWeight :: Integer
  
instance Arbitrary GetSendersListSenders where
  arbitrary = sized genGetSendersListSenders

genGetSendersListSenders :: Int -> Gen GetSendersListSenders
genGetSendersListSenders n =
  GetSendersListSenders
    <$> arbitrary -- getSendersListSendersId :: Integer
    <*> arbitrary -- getSendersListSendersName :: Text
    <*> arbitrary -- getSendersListSendersEmail :: Text
    <*> arbitrary -- getSendersListSendersActive :: Bool
    <*> arbitraryReducedMaybe n -- getSendersListSendersIps :: Maybe [GetSendersListIps]
  
instance Arbitrary GetSharedTemplateUrl where
  arbitrary = sized genGetSharedTemplateUrl

genGetSharedTemplateUrl :: Int -> Gen GetSharedTemplateUrl
genGetSharedTemplateUrl n =
  GetSharedTemplateUrl
    <$> arbitrary -- getSharedTemplateUrlSharedUrl :: Text
  
instance Arbitrary GetSmsCampaign where
  arbitrary = sized genGetSmsCampaign

genGetSmsCampaign :: Int -> Gen GetSmsCampaign
genGetSmsCampaign n =
  GetSmsCampaign
    <$> arbitrary -- getSmsCampaignId :: Integer
    <*> arbitrary -- getSmsCampaignName :: Text
    <*> arbitrary -- getSmsCampaignStatus :: E'Status2
    <*> arbitrary -- getSmsCampaignContent :: Text
    <*> arbitraryReduced n -- getSmsCampaignScheduledAt :: DateTime
    <*> arbitrary -- getSmsCampaignSender :: Text
    <*> arbitraryReduced n -- getSmsCampaignCreatedAt :: DateTime
    <*> arbitraryReduced n -- getSmsCampaignModifiedAt :: DateTime
    <*> arbitraryReduced n -- getSmsCampaignRecipients :: GetCampaignRecipients
    <*> arbitraryReduced n -- getSmsCampaignStatistics :: GetSmsCampaignStats
  
instance Arbitrary GetSmsCampaignAllOf where
  arbitrary = sized genGetSmsCampaignAllOf

genGetSmsCampaignAllOf :: Int -> Gen GetSmsCampaignAllOf
genGetSmsCampaignAllOf n =
  GetSmsCampaignAllOf
    <$> arbitraryReduced n -- getSmsCampaignAllOfRecipients :: GetCampaignRecipients
    <*> arbitraryReduced n -- getSmsCampaignAllOfStatistics :: GetSmsCampaignStats
  
instance Arbitrary GetSmsCampaignOverview where
  arbitrary = sized genGetSmsCampaignOverview

genGetSmsCampaignOverview :: Int -> Gen GetSmsCampaignOverview
genGetSmsCampaignOverview n =
  GetSmsCampaignOverview
    <$> arbitrary -- getSmsCampaignOverviewId :: Integer
    <*> arbitrary -- getSmsCampaignOverviewName :: Text
    <*> arbitrary -- getSmsCampaignOverviewStatus :: E'Status2
    <*> arbitrary -- getSmsCampaignOverviewContent :: Text
    <*> arbitraryReduced n -- getSmsCampaignOverviewScheduledAt :: DateTime
    <*> arbitrary -- getSmsCampaignOverviewSender :: Text
    <*> arbitraryReduced n -- getSmsCampaignOverviewCreatedAt :: DateTime
    <*> arbitraryReduced n -- getSmsCampaignOverviewModifiedAt :: DateTime
  
instance Arbitrary GetSmsCampaignStats where
  arbitrary = sized genGetSmsCampaignStats

genGetSmsCampaignStats :: Int -> Gen GetSmsCampaignStats
genGetSmsCampaignStats n =
  GetSmsCampaignStats
    <$> arbitrary -- getSmsCampaignStatsDelivered :: Integer
    <*> arbitrary -- getSmsCampaignStatsSent :: Integer
    <*> arbitrary -- getSmsCampaignStatsProcessing :: Integer
    <*> arbitrary -- getSmsCampaignStatsSoftBounces :: Integer
    <*> arbitrary -- getSmsCampaignStatsHardBounces :: Integer
    <*> arbitrary -- getSmsCampaignStatsUnsubscriptions :: Integer
    <*> arbitrary -- getSmsCampaignStatsAnswered :: Integer
  
instance Arbitrary GetSmsCampaigns where
  arbitrary = sized genGetSmsCampaigns

genGetSmsCampaigns :: Int -> Gen GetSmsCampaigns
genGetSmsCampaigns n =
  GetSmsCampaigns
    <$> arbitraryReducedMaybe n -- getSmsCampaignsCampaigns :: Maybe [GetSmsCampaignOverview]
    <*> arbitrary -- getSmsCampaignsCount :: Integer
  
instance Arbitrary GetSmsEventReport where
  arbitrary = sized genGetSmsEventReport

genGetSmsEventReport :: Int -> Gen GetSmsEventReport
genGetSmsEventReport n =
  GetSmsEventReport
    <$> arbitraryReducedMaybe n -- getSmsEventReportEvents :: Maybe [GetSmsEventReportEvents]
  
instance Arbitrary GetSmsEventReportEvents where
  arbitrary = sized genGetSmsEventReportEvents

genGetSmsEventReportEvents :: Int -> Gen GetSmsEventReportEvents
genGetSmsEventReportEvents n =
  GetSmsEventReportEvents
    <$> arbitrary -- getSmsEventReportEventsPhoneNumber :: Text
    <*> arbitraryReduced n -- getSmsEventReportEventsDate :: DateTime
    <*> arbitrary -- getSmsEventReportEventsMessageId :: Text
    <*> arbitrary -- getSmsEventReportEventsEvent :: E'Event2
    <*> arbitraryReducedMaybe n -- getSmsEventReportEventsReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- getSmsEventReportEventsReply :: Maybe Text
    <*> arbitraryReducedMaybe n -- getSmsEventReportEventsTag :: Maybe Text
  
instance Arbitrary GetSmtpTemplateOverview where
  arbitrary = sized genGetSmtpTemplateOverview

genGetSmtpTemplateOverview :: Int -> Gen GetSmtpTemplateOverview
genGetSmtpTemplateOverview n =
  GetSmtpTemplateOverview
    <$> arbitrary -- getSmtpTemplateOverviewId :: Integer
    <*> arbitrary -- getSmtpTemplateOverviewName :: Text
    <*> arbitrary -- getSmtpTemplateOverviewSubject :: Text
    <*> arbitrary -- getSmtpTemplateOverviewIsActive :: Bool
    <*> arbitrary -- getSmtpTemplateOverviewTestSent :: Bool
    <*> arbitraryReduced n -- getSmtpTemplateOverviewSender :: GetSmtpTemplateOverviewSender
    <*> arbitrary -- getSmtpTemplateOverviewReplyTo :: Text
    <*> arbitrary -- getSmtpTemplateOverviewToField :: Text
    <*> arbitrary -- getSmtpTemplateOverviewTag :: Text
    <*> arbitrary -- getSmtpTemplateOverviewHtmlContent :: Text
    <*> arbitraryReduced n -- getSmtpTemplateOverviewCreatedAt :: DateTime
    <*> arbitraryReduced n -- getSmtpTemplateOverviewModifiedAt :: DateTime
    <*> arbitraryReducedMaybe n -- getSmtpTemplateOverviewDoiTemplate :: Maybe Bool
  
instance Arbitrary GetSmtpTemplateOverviewSender where
  arbitrary = sized genGetSmtpTemplateOverviewSender

genGetSmtpTemplateOverviewSender :: Int -> Gen GetSmtpTemplateOverviewSender
genGetSmtpTemplateOverviewSender n =
  GetSmtpTemplateOverviewSender
    <$> arbitraryReducedMaybe n -- getSmtpTemplateOverviewSenderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- getSmtpTemplateOverviewSenderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- getSmtpTemplateOverviewSenderId :: Maybe Text
  
instance Arbitrary GetSmtpTemplates where
  arbitrary = sized genGetSmtpTemplates

genGetSmtpTemplates :: Int -> Gen GetSmtpTemplates
genGetSmtpTemplates n =
  GetSmtpTemplates
    <$> arbitraryReducedMaybe n -- getSmtpTemplatesCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- getSmtpTemplatesTemplates :: Maybe [GetSmtpTemplateOverview]
  
instance Arbitrary GetSsoToken where
  arbitrary = sized genGetSsoToken

genGetSsoToken :: Int -> Gen GetSsoToken
genGetSsoToken n =
  GetSsoToken
    <$> arbitrary -- getSsoTokenToken :: Text
  
instance Arbitrary GetStatsByDevice where
  arbitrary = sized genGetStatsByDevice

genGetStatsByDevice :: Int -> Gen GetStatsByDevice
genGetStatsByDevice n =
  GetStatsByDevice
    <$> arbitraryReducedMaybe n -- getStatsByDeviceDesktop :: Maybe (Map.Map String GetDeviceBrowserStats)
    <*> arbitraryReducedMaybe n -- getStatsByDeviceMobile :: Maybe (Map.Map String GetDeviceBrowserStats)
    <*> arbitraryReducedMaybe n -- getStatsByDeviceTablet :: Maybe (Map.Map String GetDeviceBrowserStats)
    <*> arbitraryReducedMaybe n -- getStatsByDeviceUnknown :: Maybe (Map.Map String GetDeviceBrowserStats)
  
instance Arbitrary GetTransacAggregatedSmsReport where
  arbitrary = sized genGetTransacAggregatedSmsReport

genGetTransacAggregatedSmsReport :: Int -> Gen GetTransacAggregatedSmsReport
genGetTransacAggregatedSmsReport n =
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
  arbitrary = sized genGetTransacBlockedContacts

genGetTransacBlockedContacts :: Int -> Gen GetTransacBlockedContacts
genGetTransacBlockedContacts n =
  GetTransacBlockedContacts
    <$> arbitraryReducedMaybe n -- getTransacBlockedContactsCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- getTransacBlockedContactsContacts :: Maybe [GetTransacBlockedContactsContacts]
  
instance Arbitrary GetTransacBlockedContactsContacts where
  arbitrary = sized genGetTransacBlockedContactsContacts

genGetTransacBlockedContactsContacts :: Int -> Gen GetTransacBlockedContactsContacts
genGetTransacBlockedContactsContacts n =
  GetTransacBlockedContactsContacts
    <$> arbitrary -- getTransacBlockedContactsContactsEmail :: Text
    <*> arbitrary -- getTransacBlockedContactsContactsSenderEmail :: Text
    <*> arbitraryReduced n -- getTransacBlockedContactsContactsReason :: GetTransacBlockedContactsReason
    <*> arbitraryReduced n -- getTransacBlockedContactsContactsBlockedAt :: DateTime
  
instance Arbitrary GetTransacBlockedContactsReason where
  arbitrary = sized genGetTransacBlockedContactsReason

genGetTransacBlockedContactsReason :: Int -> Gen GetTransacBlockedContactsReason
genGetTransacBlockedContactsReason n =
  GetTransacBlockedContactsReason
    <$> arbitraryReducedMaybe n -- getTransacBlockedContactsReasonCode :: Maybe E'Code2
    <*> arbitraryReducedMaybe n -- getTransacBlockedContactsReasonMessage :: Maybe Text
  
instance Arbitrary GetTransacEmailContent where
  arbitrary = sized genGetTransacEmailContent

genGetTransacEmailContent :: Int -> Gen GetTransacEmailContent
genGetTransacEmailContent n =
  GetTransacEmailContent
    <$> arbitrary -- getTransacEmailContentEmail :: Text
    <*> arbitrary -- getTransacEmailContentSubject :: Text
    <*> arbitraryReducedMaybe n -- getTransacEmailContentTemplateId :: Maybe Integer
    <*> arbitraryReduced n -- getTransacEmailContentDate :: DateTime
    <*> arbitraryReduced n -- getTransacEmailContentEvents :: [GetTransacEmailContentEvents]
    <*> arbitrary -- getTransacEmailContentBody :: Text
    <*> arbitrary -- getTransacEmailContentAttachmentCount :: Integer
  
instance Arbitrary GetTransacEmailContentEvents where
  arbitrary = sized genGetTransacEmailContentEvents

genGetTransacEmailContentEvents :: Int -> Gen GetTransacEmailContentEvents
genGetTransacEmailContentEvents n =
  GetTransacEmailContentEvents
    <$> arbitrary -- getTransacEmailContentEventsName :: Text
    <*> arbitraryReduced n -- getTransacEmailContentEventsTime :: DateTime
  
instance Arbitrary GetTransacEmailsList where
  arbitrary = sized genGetTransacEmailsList

genGetTransacEmailsList :: Int -> Gen GetTransacEmailsList
genGetTransacEmailsList n =
  GetTransacEmailsList
    <$> arbitraryReducedMaybe n -- getTransacEmailsListTransactionalEmails :: Maybe [GetTransacEmailsListTransactionalEmails]
  
instance Arbitrary GetTransacEmailsListTransactionalEmails where
  arbitrary = sized genGetTransacEmailsListTransactionalEmails

genGetTransacEmailsListTransactionalEmails :: Int -> Gen GetTransacEmailsListTransactionalEmails
genGetTransacEmailsListTransactionalEmails n =
  GetTransacEmailsListTransactionalEmails
    <$> arbitrary -- getTransacEmailsListTransactionalEmailsEmail :: Text
    <*> arbitrary -- getTransacEmailsListTransactionalEmailsSubject :: Text
    <*> arbitraryReducedMaybe n -- getTransacEmailsListTransactionalEmailsTemplateId :: Maybe Integer
    <*> arbitrary -- getTransacEmailsListTransactionalEmailsMessageId :: Text
    <*> arbitrary -- getTransacEmailsListTransactionalEmailsUuid :: Text
    <*> arbitraryReduced n -- getTransacEmailsListTransactionalEmailsDate :: DateTime
    <*> arbitraryReducedMaybe n -- getTransacEmailsListTransactionalEmailsFrom :: Maybe Text
    <*> arbitraryReducedMaybe n -- getTransacEmailsListTransactionalEmailsTags :: Maybe [Text]
  
instance Arbitrary GetTransacSmsReport where
  arbitrary = sized genGetTransacSmsReport

genGetTransacSmsReport :: Int -> Gen GetTransacSmsReport
genGetTransacSmsReport n =
  GetTransacSmsReport
    <$> arbitraryReducedMaybe n -- getTransacSmsReportReports :: Maybe [GetTransacSmsReportReports]
  
instance Arbitrary GetTransacSmsReportReports where
  arbitrary = sized genGetTransacSmsReportReports

genGetTransacSmsReportReports :: Int -> Gen GetTransacSmsReportReports
genGetTransacSmsReportReports n =
  GetTransacSmsReportReports
    <$> arbitraryReduced n -- getTransacSmsReportReportsDate :: Date
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
  arbitrary = sized genGetWebhook

genGetWebhook :: Int -> Gen GetWebhook
genGetWebhook n =
  GetWebhook
    <$> arbitrary -- getWebhookUrl :: Text
    <*> arbitrary -- getWebhookId :: Integer
    <*> arbitrary -- getWebhookDescription :: Text
    <*> arbitrary -- getWebhookEvents :: [Text]
    <*> arbitrary -- getWebhookType :: E'Type2
    <*> arbitraryReduced n -- getWebhookCreatedAt :: DateTime
    <*> arbitraryReduced n -- getWebhookModifiedAt :: DateTime
  
instance Arbitrary GetWebhooks where
  arbitrary = sized genGetWebhooks

genGetWebhooks :: Int -> Gen GetWebhooks
genGetWebhooks n =
  GetWebhooks
    <$> arbitraryReduced n -- getWebhooksWebhooks :: [GetWebhook]
  
instance Arbitrary ManageIp where
  arbitrary = sized genManageIp

genManageIp :: Int -> Gen ManageIp
genManageIp n =
  ManageIp
    <$> arbitraryReducedMaybe n -- manageIpIp :: Maybe Text
  
instance Arbitrary PostContactInfo where
  arbitrary = sized genPostContactInfo

genPostContactInfo :: Int -> Gen PostContactInfo
genPostContactInfo n =
  PostContactInfo
    <$> arbitraryReduced n -- postContactInfoContacts :: PostContactInfoContacts
  
instance Arbitrary PostContactInfoContacts where
  arbitrary = sized genPostContactInfoContacts

genPostContactInfoContacts :: Int -> Gen PostContactInfoContacts
genPostContactInfoContacts n =
  PostContactInfoContacts
    <$> arbitraryReducedMaybe n -- postContactInfoContactsSuccess :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- postContactInfoContactsFailure :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- postContactInfoContactsTotal :: Maybe Integer
    <*> arbitraryReducedMaybe n -- postContactInfoContactsProcessId :: Maybe Integer
  
instance Arbitrary PostSendFailed where
  arbitrary = sized genPostSendFailed

genPostSendFailed :: Int -> Gen PostSendFailed
genPostSendFailed n =
  PostSendFailed
    <$> arbitrary -- postSendFailedCode :: Integer
    <*> arbitrary -- postSendFailedMessage :: Text
    <*> arbitraryReducedMaybe n -- postSendFailedUnexistingEmails :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- postSendFailedWithoutListEmails :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- postSendFailedBlackListedEmails :: Maybe [Text]
  
instance Arbitrary PostSendSmsTestFailed where
  arbitrary = sized genPostSendSmsTestFailed

genPostSendSmsTestFailed :: Int -> Gen PostSendSmsTestFailed
genPostSendSmsTestFailed n =
  PostSendSmsTestFailed
    <$> arbitrary -- postSendSmsTestFailedCode :: Integer
    <*> arbitrary -- postSendSmsTestFailedMessage :: Text
    <*> arbitraryReducedMaybe n -- postSendSmsTestFailedUnexistingSms :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- postSendSmsTestFailedWithoutListSms :: Maybe [Text]
  
instance Arbitrary RemainingCreditModel where
  arbitrary = sized genRemainingCreditModel

genRemainingCreditModel :: Int -> Gen RemainingCreditModel
genRemainingCreditModel n =
  RemainingCreditModel
    <$> arbitraryReduced n -- remainingCreditModelChild :: RemainingCreditModelChild
    <*> arbitraryReduced n -- remainingCreditModelReseller :: RemainingCreditModelReseller
  
instance Arbitrary RemainingCreditModelChild where
  arbitrary = sized genRemainingCreditModelChild

genRemainingCreditModelChild :: Int -> Gen RemainingCreditModelChild
genRemainingCreditModelChild n =
  RemainingCreditModelChild
    <$> arbitrary -- remainingCreditModelChildSms :: Double
    <*> arbitrary -- remainingCreditModelChildEmail :: Double
  
instance Arbitrary RemainingCreditModelReseller where
  arbitrary = sized genRemainingCreditModelReseller

genRemainingCreditModelReseller :: Int -> Gen RemainingCreditModelReseller
genRemainingCreditModelReseller n =
  RemainingCreditModelReseller
    <$> arbitrary -- remainingCreditModelResellerSms :: Double
    <*> arbitrary -- remainingCreditModelResellerEmail :: Double
  
instance Arbitrary RemoveContactFromList where
  arbitrary = sized genRemoveContactFromList

genRemoveContactFromList :: Int -> Gen RemoveContactFromList
genRemoveContactFromList n =
  RemoveContactFromList
    <$> arbitraryReducedMaybe n -- removeContactFromListEmails :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- removeContactFromListAll :: Maybe Bool
  
instance Arbitrary RemoveCredits where
  arbitrary = sized genRemoveCredits

genRemoveCredits :: Int -> Gen RemoveCredits
genRemoveCredits n =
  RemoveCredits
    <$> arbitraryReducedMaybe n -- removeCreditsSms :: Maybe Integer
    <*> arbitraryReducedMaybe n -- removeCreditsEmail :: Maybe Integer
  
instance Arbitrary RequestContactExport where
  arbitrary = sized genRequestContactExport

genRequestContactExport :: Int -> Gen RequestContactExport
genRequestContactExport n =
  RequestContactExport
    <$> arbitraryReducedMaybe n -- requestContactExportExportAttributes :: Maybe [Text]
    <*> arbitraryReducedMaybeValue n -- requestContactExportContactFilter :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- requestContactExportCustomContactFilter :: Maybe RequestContactExportCustomContactFilter
    <*> arbitraryReducedMaybe n -- requestContactExportNotifyUrl :: Maybe Text
  
instance Arbitrary RequestContactExportCustomContactFilter where
  arbitrary = sized genRequestContactExportCustomContactFilter

genRequestContactExportCustomContactFilter :: Int -> Gen RequestContactExportCustomContactFilter
genRequestContactExportCustomContactFilter n =
  RequestContactExportCustomContactFilter
    <$> arbitraryReducedMaybe n -- requestContactExportCustomContactFilterActionForContacts :: Maybe E'ActionForContacts
    <*> arbitraryReducedMaybe n -- requestContactExportCustomContactFilterActionForEmailCampaigns :: Maybe E'ActionForEmailCampaigns
    <*> arbitraryReducedMaybe n -- requestContactExportCustomContactFilterActionForSmsCampaigns :: Maybe E'ActionForSmsCampaigns
    <*> arbitraryReducedMaybe n -- requestContactExportCustomContactFilterListId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- requestContactExportCustomContactFilterEmailCampaignId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- requestContactExportCustomContactFilterSmsCampaignId :: Maybe Integer
  
instance Arbitrary RequestContactImport where
  arbitrary = sized genRequestContactImport

genRequestContactImport :: Int -> Gen RequestContactImport
genRequestContactImport n =
  RequestContactImport
    <$> arbitraryReducedMaybe n -- requestContactImportFileUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- requestContactImportFileBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- requestContactImportListIds :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- requestContactImportNotifyUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- requestContactImportNewList :: Maybe RequestContactImportNewList
    <*> arbitraryReducedMaybe n -- requestContactImportEmailBlacklist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- requestContactImportSmsBlacklist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- requestContactImportUpdateExistingContacts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- requestContactImportEmptyContactsAttributes :: Maybe Bool
  
instance Arbitrary RequestContactImportNewList where
  arbitrary = sized genRequestContactImportNewList

genRequestContactImportNewList :: Int -> Gen RequestContactImportNewList
genRequestContactImportNewList n =
  RequestContactImportNewList
    <$> arbitraryReducedMaybe n -- requestContactImportNewListListName :: Maybe Text
    <*> arbitraryReducedMaybe n -- requestContactImportNewListFolderId :: Maybe Integer
  
instance Arbitrary RequestSmsRecipientExport where
  arbitrary = sized genRequestSmsRecipientExport

genRequestSmsRecipientExport :: Int -> Gen RequestSmsRecipientExport
genRequestSmsRecipientExport n =
  RequestSmsRecipientExport
    <$> arbitraryReducedMaybe n -- requestSmsRecipientExportNotifyUrl :: Maybe Text
    <*> arbitrary -- requestSmsRecipientExportRecipientsType :: E'RecipientsType2
  
instance Arbitrary SendEmail where
  arbitrary = sized genSendEmail

genSendEmail :: Int -> Gen SendEmail
genSendEmail n =
  SendEmail
    <$> arbitrary -- sendEmailEmailTo :: [Text]
    <*> arbitraryReducedMaybe n -- sendEmailEmailBcc :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- sendEmailEmailCc :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- sendEmailReplyTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- sendEmailAttachmentUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- sendEmailAttachment :: Maybe [SendEmailAttachment]
    <*> arbitraryReducedMaybeValue n -- sendEmailHeaders :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- sendEmailAttributes :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- sendEmailTags :: Maybe [Text]
  
instance Arbitrary SendEmailAttachment where
  arbitrary = sized genSendEmailAttachment

genSendEmailAttachment :: Int -> Gen SendEmailAttachment
genSendEmailAttachment n =
  SendEmailAttachment
    <$> arbitraryReduced n -- sendEmailAttachmentContent :: ByteArray
    <*> arbitrary -- sendEmailAttachmentName :: Text
  
instance Arbitrary SendReport where
  arbitrary = sized genSendReport

genSendReport :: Int -> Gen SendReport
genSendReport n =
  SendReport
    <$> arbitraryReducedMaybe n -- sendReportLanguage :: Maybe E'Language
    <*> arbitraryReduced n -- sendReportEmail :: SendReportEmail
  
instance Arbitrary SendReportEmail where
  arbitrary = sized genSendReportEmail

genSendReportEmail :: Int -> Gen SendReportEmail
genSendReportEmail n =
  SendReportEmail
    <$> arbitrary -- sendReportEmailTo :: [Text]
    <*> arbitrary -- sendReportEmailBody :: Text
  
instance Arbitrary SendSms where
  arbitrary = sized genSendSms

genSendSms :: Int -> Gen SendSms
genSendSms n =
  SendSms
    <$> arbitrary -- sendSmsReference :: Text
    <*> arbitrary -- sendSmsMessageId :: Integer
    <*> arbitraryReducedMaybe n -- sendSmsSmsCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- sendSmsUsedCredits :: Maybe Float
    <*> arbitraryReducedMaybe n -- sendSmsRemainingCredits :: Maybe Float
  
instance Arbitrary SendSmtpEmail where
  arbitrary = sized genSendSmtpEmail

genSendSmtpEmail :: Int -> Gen SendSmtpEmail
genSendSmtpEmail n =
  SendSmtpEmail
    <$> arbitraryReducedMaybe n -- sendSmtpEmailSender :: Maybe SendSmtpEmailSender
    <*> arbitraryReduced n -- sendSmtpEmailTo :: [SendSmtpEmailTo]
    <*> arbitraryReducedMaybe n -- sendSmtpEmailBcc :: Maybe [SendSmtpEmailBcc]
    <*> arbitraryReducedMaybe n -- sendSmtpEmailCc :: Maybe [SendSmtpEmailCc]
    <*> arbitraryReducedMaybe n -- sendSmtpEmailHtmlContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailTextContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailReplyTo :: Maybe SendSmtpEmailReplyTo
    <*> arbitraryReducedMaybe n -- sendSmtpEmailAttachment :: Maybe [SendSmtpEmailAttachment]
    <*> arbitraryReducedMaybeValue n -- sendSmtpEmailHeaders :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- sendSmtpEmailTemplateId :: Maybe Integer
    <*> arbitraryReducedMaybeValue n -- sendSmtpEmailParams :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- sendSmtpEmailTags :: Maybe [Text]
  
instance Arbitrary SendSmtpEmailAttachment where
  arbitrary = sized genSendSmtpEmailAttachment

genSendSmtpEmailAttachment :: Int -> Gen SendSmtpEmailAttachment
genSendSmtpEmailAttachment n =
  SendSmtpEmailAttachment
    <$> arbitraryReducedMaybe n -- sendSmtpEmailAttachmentUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailAttachmentContent :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- sendSmtpEmailAttachmentName :: Maybe Text
  
instance Arbitrary SendSmtpEmailBcc where
  arbitrary = sized genSendSmtpEmailBcc

genSendSmtpEmailBcc :: Int -> Gen SendSmtpEmailBcc
genSendSmtpEmailBcc n =
  SendSmtpEmailBcc
    <$> arbitrary -- sendSmtpEmailBccEmail :: Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailBccName :: Maybe Text
  
instance Arbitrary SendSmtpEmailCc where
  arbitrary = sized genSendSmtpEmailCc

genSendSmtpEmailCc :: Int -> Gen SendSmtpEmailCc
genSendSmtpEmailCc n =
  SendSmtpEmailCc
    <$> arbitrary -- sendSmtpEmailCcEmail :: Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailCcName :: Maybe Text
  
instance Arbitrary SendSmtpEmailReplyTo where
  arbitrary = sized genSendSmtpEmailReplyTo

genSendSmtpEmailReplyTo :: Int -> Gen SendSmtpEmailReplyTo
genSendSmtpEmailReplyTo n =
  SendSmtpEmailReplyTo
    <$> arbitrary -- sendSmtpEmailReplyToEmail :: Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailReplyToName :: Maybe Text
  
instance Arbitrary SendSmtpEmailSender where
  arbitrary = sized genSendSmtpEmailSender

genSendSmtpEmailSender :: Int -> Gen SendSmtpEmailSender
genSendSmtpEmailSender n =
  SendSmtpEmailSender
    <$> arbitraryReducedMaybe n -- sendSmtpEmailSenderName :: Maybe Text
    <*> arbitrary -- sendSmtpEmailSenderEmail :: Text
  
instance Arbitrary SendSmtpEmailTo where
  arbitrary = sized genSendSmtpEmailTo

genSendSmtpEmailTo :: Int -> Gen SendSmtpEmailTo
genSendSmtpEmailTo n =
  SendSmtpEmailTo
    <$> arbitrary -- sendSmtpEmailToEmail :: Text
    <*> arbitraryReducedMaybe n -- sendSmtpEmailToName :: Maybe Text
  
instance Arbitrary SendTemplateEmail where
  arbitrary = sized genSendTemplateEmail

genSendTemplateEmail :: Int -> Gen SendTemplateEmail
genSendTemplateEmail n =
  SendTemplateEmail
    <$> arbitrary -- sendTemplateEmailMessageId :: Text
  
instance Arbitrary SendTestEmail where
  arbitrary = sized genSendTestEmail

genSendTestEmail :: Int -> Gen SendTestEmail
genSendTestEmail n =
  SendTestEmail
    <$> arbitraryReducedMaybe n -- sendTestEmailEmailTo :: Maybe [Text]
  
instance Arbitrary SendTestSms where
  arbitrary = sized genSendTestSms

genSendTestSms :: Int -> Gen SendTestSms
genSendTestSms n =
  SendTestSms
    <$> arbitraryReducedMaybe n -- sendTestSmsPhoneNumber :: Maybe Text
  
instance Arbitrary SendTransacSms where
  arbitrary = sized genSendTransacSms

genSendTransacSms :: Int -> Gen SendTransacSms
genSendTransacSms n =
  SendTransacSms
    <$> arbitrary -- sendTransacSmsSender :: Text
    <*> arbitrary -- sendTransacSmsRecipient :: Text
    <*> arbitrary -- sendTransacSmsContent :: Text
    <*> arbitraryReducedMaybe n -- sendTransacSmsType :: Maybe E'Type3
    <*> arbitraryReducedMaybe n -- sendTransacSmsTag :: Maybe Text
    <*> arbitraryReducedMaybe n -- sendTransacSmsWebUrl :: Maybe Text
  
instance Arbitrary UpdateAttribute where
  arbitrary = sized genUpdateAttribute

genUpdateAttribute :: Int -> Gen UpdateAttribute
genUpdateAttribute n =
  UpdateAttribute
    <$> arbitraryReducedMaybe n -- updateAttributeValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateAttributeEnumeration :: Maybe [UpdateAttributeEnumeration]
  
instance Arbitrary UpdateAttributeEnumeration where
  arbitrary = sized genUpdateAttributeEnumeration

genUpdateAttributeEnumeration :: Int -> Gen UpdateAttributeEnumeration
genUpdateAttributeEnumeration n =
  UpdateAttributeEnumeration
    <$> arbitrary -- updateAttributeEnumerationValue :: Int
    <*> arbitrary -- updateAttributeEnumerationLabel :: Text
  
instance Arbitrary UpdateCampaignStatus where
  arbitrary = sized genUpdateCampaignStatus

genUpdateCampaignStatus :: Int -> Gen UpdateCampaignStatus
genUpdateCampaignStatus n =
  UpdateCampaignStatus
    <$> arbitraryReducedMaybe n -- updateCampaignStatusStatus :: Maybe E'Status4
  
instance Arbitrary UpdateChild where
  arbitrary = sized genUpdateChild

genUpdateChild :: Int -> Gen UpdateChild
genUpdateChild n =
  UpdateChild
    <$> arbitraryReducedMaybe n -- updateChildEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateChildFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateChildLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateChildCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateChildPassword :: Maybe Text
  
instance Arbitrary UpdateChildAccountStatus where
  arbitrary = sized genUpdateChildAccountStatus

genUpdateChildAccountStatus :: Int -> Gen UpdateChildAccountStatus
genUpdateChildAccountStatus n =
  UpdateChildAccountStatus
    <$> arbitraryReducedMaybe n -- updateChildAccountStatusTransactionalEmail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateChildAccountStatusTransactionalSms :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateChildAccountStatusMarketingAutomation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateChildAccountStatusSmsCampaign :: Maybe Bool
  
instance Arbitrary UpdateChildDomain where
  arbitrary = sized genUpdateChildDomain

genUpdateChildDomain :: Int -> Gen UpdateChildDomain
genUpdateChildDomain n =
  UpdateChildDomain
    <$> arbitraryReducedMaybe n -- updateChildDomainDomain :: Maybe Text
  
instance Arbitrary UpdateContact where
  arbitrary = sized genUpdateContact

genUpdateContact :: Int -> Gen UpdateContact
genUpdateContact n =
  UpdateContact
    <$> arbitraryReducedMaybeValue n -- updateContactAttributes :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- updateContactEmailBlacklisted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateContactSmsBlacklisted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateContactListIds :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- updateContactUnlinkListIds :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- updateContactSmtpBlacklistSender :: Maybe [Text]
  
instance Arbitrary UpdateEmailCampaign where
  arbitrary = sized genUpdateEmailCampaign

genUpdateEmailCampaign :: Int -> Gen UpdateEmailCampaign
genUpdateEmailCampaign n =
  UpdateEmailCampaign
    <$> arbitraryReducedMaybe n -- updateEmailCampaignTag :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSender :: Maybe UpdateEmailCampaignSender
    <*> arbitraryReducedMaybe n -- updateEmailCampaignName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignHtmlContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignScheduledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignReplyTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignToField :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignRecipients :: Maybe UpdateEmailCampaignRecipients
    <*> arbitraryReducedMaybe n -- updateEmailCampaignAttachmentUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignInlineImageActivation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateEmailCampaignMirrorActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateEmailCampaignRecurring :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateEmailCampaignFooter :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignHeader :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignUtmCampaign :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- updateEmailCampaignParams :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSendAtBestTime :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateEmailCampaignAbTesting :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSubjectA :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSubjectB :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSplitRule :: Maybe Integer
    <*> arbitraryReducedMaybe n -- updateEmailCampaignWinnerCriteria :: Maybe E'WinnerCriteria
    <*> arbitraryReducedMaybe n -- updateEmailCampaignWinnerDelay :: Maybe Integer
    <*> arbitraryReducedMaybe n -- updateEmailCampaignIpWarmupEnable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- updateEmailCampaignInitialQuota :: Maybe Integer
    <*> arbitraryReducedMaybe n -- updateEmailCampaignIncreaseRate :: Maybe Integer
  
instance Arbitrary UpdateEmailCampaignRecipients where
  arbitrary = sized genUpdateEmailCampaignRecipients

genUpdateEmailCampaignRecipients :: Int -> Gen UpdateEmailCampaignRecipients
genUpdateEmailCampaignRecipients n =
  UpdateEmailCampaignRecipients
    <$> arbitraryReducedMaybe n -- updateEmailCampaignRecipientsExclusionListIds :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- updateEmailCampaignRecipientsListIds :: Maybe [Integer]
  
instance Arbitrary UpdateEmailCampaignSender where
  arbitrary = sized genUpdateEmailCampaignSender

genUpdateEmailCampaignSender :: Int -> Gen UpdateEmailCampaignSender
genUpdateEmailCampaignSender n =
  UpdateEmailCampaignSender
    <$> arbitraryReducedMaybe n -- updateEmailCampaignSenderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSenderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateEmailCampaignSenderId :: Maybe Integer
  
instance Arbitrary UpdateList where
  arbitrary = sized genUpdateList

genUpdateList :: Int -> Gen UpdateList
genUpdateList n =
  UpdateList
    <$> arbitraryReducedMaybe n -- updateListName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateListFolderId :: Maybe Integer
  
instance Arbitrary UpdateSender where
  arbitrary = sized genUpdateSender

genUpdateSender :: Int -> Gen UpdateSender
genUpdateSender n =
  UpdateSender
    <$> arbitraryReducedMaybe n -- updateSenderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSenderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSenderIps :: Maybe [CreateSenderIps]
  
instance Arbitrary UpdateSmsCampaign where
  arbitrary = sized genUpdateSmsCampaign

genUpdateSmsCampaign :: Int -> Gen UpdateSmsCampaign
genUpdateSmsCampaign n =
  UpdateSmsCampaign
    <$> arbitraryReducedMaybe n -- updateSmsCampaignName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmsCampaignSender :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmsCampaignContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmsCampaignRecipients :: Maybe CreateSmsCampaignRecipients
    <*> arbitraryReducedMaybe n -- updateSmsCampaignScheduledAt :: Maybe DateTime
  
instance Arbitrary UpdateSmtpTemplate where
  arbitrary = sized genUpdateSmtpTemplate

genUpdateSmtpTemplate :: Int -> Gen UpdateSmtpTemplate
genUpdateSmtpTemplate n =
  UpdateSmtpTemplate
    <$> arbitraryReducedMaybe n -- updateSmtpTemplateTag :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateSender :: Maybe UpdateSmtpTemplateSender
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateTemplateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateHtmlContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateReplyTo :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateToField :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateAttachmentUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateIsActive :: Maybe Bool
  
instance Arbitrary UpdateSmtpTemplateSender where
  arbitrary = sized genUpdateSmtpTemplateSender

genUpdateSmtpTemplateSender :: Int -> Gen UpdateSmtpTemplateSender
genUpdateSmtpTemplateSender n =
  UpdateSmtpTemplateSender
    <$> arbitraryReducedMaybe n -- updateSmtpTemplateSenderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateSenderEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateSmtpTemplateSenderId :: Maybe Integer
  
instance Arbitrary UpdateWebhook where
  arbitrary = sized genUpdateWebhook

genUpdateWebhook :: Int -> Gen UpdateWebhook
genUpdateWebhook n =
  UpdateWebhook
    <$> arbitraryReducedMaybe n -- updateWebhookUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateWebhookDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateWebhookEvents :: Maybe [E'Events2]
  
instance Arbitrary UploadImageToGallery where
  arbitrary = sized genUploadImageToGallery

genUploadImageToGallery :: Int -> Gen UploadImageToGallery
genUploadImageToGallery n =
  UploadImageToGallery
    <$> arbitrary -- uploadImageToGalleryImageUrl :: Text
    <*> arbitraryReducedMaybe n -- uploadImageToGalleryName :: Maybe Text
  



instance Arbitrary E'ActionForContacts where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ActionForEmailCampaigns where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ActionForSmsCampaigns where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AttributeCategory where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Category where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Code where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Code2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CreditsType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Event where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Event2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Events where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Events2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Language where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RecipientsType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RecipientsType2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Sort where
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

instance Arbitrary E'WinningCriteria where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'WinningVersion where
  arbitrary = arbitraryBoundedEnum

