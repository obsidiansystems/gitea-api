{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Gitea.Model
import Gitea.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

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

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
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

instance Arbitrary APIError where
  arbitrary = sized genAPIError

genAPIError :: Int -> Gen APIError
genAPIError n =
  APIError
    <$> arbitraryReducedMaybe n -- aPIErrorMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- aPIErrorUrl :: Maybe Text
  
instance Arbitrary AccessToken where
  arbitrary = sized genAccessToken

genAccessToken :: Int -> Gen AccessToken
genAccessToken n =
  AccessToken
    <$> arbitraryReducedMaybe n -- accessTokenId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- accessTokenName :: Maybe Text
    <*> arbitraryReducedMaybe n -- accessTokenScopes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- accessTokenSha1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- accessTokenTokenLastEight :: Maybe Text
  
instance Arbitrary Activity where
  arbitrary = sized genActivity

genActivity :: Int -> Gen Activity
genActivity n =
  Activity
    <$> arbitraryReducedMaybe n -- activityActUser :: Maybe User
    <*> arbitraryReducedMaybe n -- activityActUserId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- activityComment :: Maybe Comment
    <*> arbitraryReducedMaybe n -- activityCommentId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- activityContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- activityId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- activityIsPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- activityOpType :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityRefName :: Maybe Text
    <*> arbitraryReducedMaybe n -- activityRepo :: Maybe Repository
    <*> arbitraryReducedMaybe n -- activityRepoId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- activityUserId :: Maybe Integer
  
instance Arbitrary ActivityPub where
  arbitrary = sized genActivityPub

genActivityPub :: Int -> Gen ActivityPub
genActivityPub n =
  ActivityPub
    <$> arbitraryReducedMaybe n -- activityPubContext :: Maybe Text
  
instance Arbitrary AddCollaboratorOption where
  arbitrary = sized genAddCollaboratorOption

genAddCollaboratorOption :: Int -> Gen AddCollaboratorOption
genAddCollaboratorOption n =
  AddCollaboratorOption
    <$> arbitraryReducedMaybe n -- addCollaboratorOptionPermission :: Maybe Text
  
instance Arbitrary AddTimeOption where
  arbitrary = sized genAddTimeOption

genAddTimeOption :: Int -> Gen AddTimeOption
genAddTimeOption n =
  AddTimeOption
    <$> arbitraryReducedMaybe n -- addTimeOptionCreated :: Maybe DateTime
    <*> arbitrary -- addTimeOptionTime :: Integer
    <*> arbitraryReducedMaybe n -- addTimeOptionUserName :: Maybe Text
  
instance Arbitrary AnnotatedTag where
  arbitrary = sized genAnnotatedTag

genAnnotatedTag :: Int -> Gen AnnotatedTag
genAnnotatedTag n =
  AnnotatedTag
    <$> arbitraryReducedMaybe n -- annotatedTagMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- annotatedTagObject :: Maybe AnnotatedTagObject
    <*> arbitraryReducedMaybe n -- annotatedTagSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- annotatedTagTag :: Maybe Text
    <*> arbitraryReducedMaybe n -- annotatedTagTagger :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- annotatedTagUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- annotatedTagVerification :: Maybe PayloadCommitVerification
  
instance Arbitrary AnnotatedTagObject where
  arbitrary = sized genAnnotatedTagObject

genAnnotatedTagObject :: Int -> Gen AnnotatedTagObject
genAnnotatedTagObject n =
  AnnotatedTagObject
    <$> arbitraryReducedMaybe n -- annotatedTagObjectSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- annotatedTagObjectType :: Maybe Text
    <*> arbitraryReducedMaybe n -- annotatedTagObjectUrl :: Maybe Text
  
instance Arbitrary Attachment where
  arbitrary = sized genAttachment

genAttachment :: Int -> Gen Attachment
genAttachment n =
  Attachment
    <$> arbitraryReducedMaybe n -- attachmentBrowserDownloadUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- attachmentDownloadCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- attachmentId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- attachmentName :: Maybe Text
    <*> arbitraryReducedMaybe n -- attachmentSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- attachmentUuid :: Maybe Text
  
instance Arbitrary Branch where
  arbitrary = sized genBranch

genBranch :: Int -> Gen Branch
genBranch n =
  Branch
    <$> arbitraryReducedMaybe n -- branchCommit :: Maybe PayloadCommit
    <*> arbitraryReducedMaybe n -- branchEffectiveBranchProtectionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- branchEnableStatusCheck :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchName :: Maybe Text
    <*> arbitraryReducedMaybe n -- branchProtected :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchRequiredApprovals :: Maybe Integer
    <*> arbitraryReducedMaybe n -- branchStatusCheckContexts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchUserCanMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchUserCanPush :: Maybe Bool
  
instance Arbitrary BranchProtection where
  arbitrary = sized genBranchProtection

genBranchProtection :: Int -> Gen BranchProtection
genBranchProtection n =
  BranchProtection
    <$> arbitraryReducedMaybe n -- branchProtectionApprovalsWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchProtectionApprovalsWhitelistUsername :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchProtectionBlockOnOfficialReviewRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionBlockOnOutdatedBranch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionBlockOnRejectedReviews :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionBranchName :: Maybe Text
    <*> arbitraryReducedMaybe n -- branchProtectionCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- branchProtectionDismissStaleApprovals :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionEnableApprovalsWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionEnableMergeWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionEnablePush :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionEnablePushWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionEnableStatusCheck :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionMergeWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchProtectionMergeWhitelistUsernames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchProtectionProtectedFilePatterns :: Maybe Text
    <*> arbitraryReducedMaybe n -- branchProtectionPushWhitelistDeployKeys :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionPushWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchProtectionPushWhitelistUsernames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchProtectionRequireSignedCommits :: Maybe Bool
    <*> arbitraryReducedMaybe n -- branchProtectionRequiredApprovals :: Maybe Integer
    <*> arbitraryReducedMaybe n -- branchProtectionRuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- branchProtectionStatusCheckContexts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- branchProtectionUnprotectedFilePatterns :: Maybe Text
    <*> arbitraryReducedMaybe n -- branchProtectionUpdatedAt :: Maybe DateTime
  
instance Arbitrary ChangeFileOperation where
  arbitrary = sized genChangeFileOperation

genChangeFileOperation :: Int -> Gen ChangeFileOperation
genChangeFileOperation n =
  ChangeFileOperation
    <$> arbitraryReducedMaybe n -- changeFileOperationContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- changeFileOperationFromPath :: Maybe Text
    <*> arbitrary -- changeFileOperationOperation :: E'Operation
    <*> arbitrary -- changeFileOperationPath :: Text
    <*> arbitraryReducedMaybe n -- changeFileOperationSha :: Maybe Text
  
instance Arbitrary ChangeFilesOptions where
  arbitrary = sized genChangeFilesOptions

genChangeFilesOptions :: Int -> Gen ChangeFilesOptions
genChangeFilesOptions n =
  ChangeFilesOptions
    <$> arbitraryReducedMaybe n -- changeFilesOptionsAuthor :: Maybe Identity
    <*> arbitraryReducedMaybe n -- changeFilesOptionsBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- changeFilesOptionsCommitter :: Maybe Identity
    <*> arbitraryReducedMaybe n -- changeFilesOptionsDates :: Maybe CommitDateOptions
    <*> arbitraryReduced n -- changeFilesOptionsFiles :: [ChangeFileOperation]
    <*> arbitraryReducedMaybe n -- changeFilesOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- changeFilesOptionsNewBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- changeFilesOptionsSignoff :: Maybe Bool
  
instance Arbitrary ChangedFile where
  arbitrary = sized genChangedFile

genChangedFile :: Int -> Gen ChangedFile
genChangedFile n =
  ChangedFile
    <$> arbitraryReducedMaybe n -- changedFileAdditions :: Maybe Integer
    <*> arbitraryReducedMaybe n -- changedFileChanges :: Maybe Integer
    <*> arbitraryReducedMaybe n -- changedFileContentsUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- changedFileDeletions :: Maybe Integer
    <*> arbitraryReducedMaybe n -- changedFileFilename :: Maybe Text
    <*> arbitraryReducedMaybe n -- changedFileHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- changedFilePreviousFilename :: Maybe Text
    <*> arbitraryReducedMaybe n -- changedFileRawUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- changedFileStatus :: Maybe Text
  
instance Arbitrary CombinedStatus where
  arbitrary = sized genCombinedStatus

genCombinedStatus :: Int -> Gen CombinedStatus
genCombinedStatus n =
  CombinedStatus
    <$> arbitraryReducedMaybe n -- combinedStatusCommitUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- combinedStatusRepository :: Maybe Repository
    <*> arbitraryReducedMaybe n -- combinedStatusSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- combinedStatusState :: Maybe Text
    <*> arbitraryReducedMaybe n -- combinedStatusStatuses :: Maybe [CommitStatus]
    <*> arbitraryReducedMaybe n -- combinedStatusTotalCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- combinedStatusUrl :: Maybe Text
  
instance Arbitrary Comment where
  arbitrary = sized genComment

genComment :: Int -> Gen Comment
genComment n =
  Comment
    <$> arbitraryReducedMaybe n -- commentAssets :: Maybe [Attachment]
    <*> arbitraryReducedMaybe n -- commentBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- commentCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commentHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- commentId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- commentIssueUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- commentOriginalAuthor :: Maybe Text
    <*> arbitraryReducedMaybe n -- commentOriginalAuthorId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- commentPullRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- commentUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commentUser :: Maybe User
  
instance Arbitrary Commit where
  arbitrary = sized genCommit

genCommit :: Int -> Gen Commit
genCommit n =
  Commit
    <$> arbitraryReducedMaybe n -- commitAuthor :: Maybe User
    <*> arbitraryReducedMaybe n -- commitCommit :: Maybe RepoCommit
    <*> arbitraryReducedMaybe n -- commitCommitter :: Maybe User
    <*> arbitraryReducedMaybe n -- commitCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commitFiles :: Maybe [CommitAffectedFiles]
    <*> arbitraryReducedMaybe n -- commitHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitParents :: Maybe [CommitMeta]
    <*> arbitraryReducedMaybe n -- commitSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitStats :: Maybe CommitStats
    <*> arbitraryReducedMaybe n -- commitUrl :: Maybe Text
  
instance Arbitrary CommitAffectedFiles where
  arbitrary = sized genCommitAffectedFiles

genCommitAffectedFiles :: Int -> Gen CommitAffectedFiles
genCommitAffectedFiles n =
  CommitAffectedFiles
    <$> arbitraryReducedMaybe n -- commitAffectedFilesFilename :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitAffectedFilesStatus :: Maybe Text
  
instance Arbitrary CommitDateOptions where
  arbitrary = sized genCommitDateOptions

genCommitDateOptions :: Int -> Gen CommitDateOptions
genCommitDateOptions n =
  CommitDateOptions
    <$> arbitraryReducedMaybe n -- commitDateOptionsAuthor :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commitDateOptionsCommitter :: Maybe DateTime
  
instance Arbitrary CommitMeta where
  arbitrary = sized genCommitMeta

genCommitMeta :: Int -> Gen CommitMeta
genCommitMeta n =
  CommitMeta
    <$> arbitraryReducedMaybe n -- commitMetaCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commitMetaSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitMetaUrl :: Maybe Text
  
instance Arbitrary CommitStats where
  arbitrary = sized genCommitStats

genCommitStats :: Int -> Gen CommitStats
genCommitStats n =
  CommitStats
    <$> arbitraryReducedMaybe n -- commitStatsAdditions :: Maybe Integer
    <*> arbitraryReducedMaybe n -- commitStatsDeletions :: Maybe Integer
    <*> arbitraryReducedMaybe n -- commitStatsTotal :: Maybe Integer
  
instance Arbitrary CommitStatus where
  arbitrary = sized genCommitStatus

genCommitStatus :: Int -> Gen CommitStatus
genCommitStatus n =
  CommitStatus
    <$> arbitraryReducedMaybe n -- commitStatusContext :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitStatusCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commitStatusCreator :: Maybe User
    <*> arbitraryReducedMaybe n -- commitStatusDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitStatusId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- commitStatusStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitStatusTargetUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitStatusUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commitStatusUrl :: Maybe Text
  
instance Arbitrary CommitUser where
  arbitrary = sized genCommitUser

genCommitUser :: Int -> Gen CommitUser
genCommitUser n =
  CommitUser
    <$> arbitraryReducedMaybe n -- commitUserDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitUserEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitUserName :: Maybe Text
  
instance Arbitrary ContentsResponse where
  arbitrary = sized genContentsResponse

genContentsResponse :: Int -> Gen ContentsResponse
genContentsResponse n =
  ContentsResponse
    <$> arbitraryReducedMaybe n -- contentsResponseLinks :: Maybe FileLinksResponse
    <*> arbitraryReducedMaybe n -- contentsResponseContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseDownloadUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseEncoding :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseGitUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseLastCommitSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponsePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- contentsResponseSubmoduleGitUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseTarget :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseUrl :: Maybe Text
  
instance Arbitrary CreateAccessTokenOption where
  arbitrary = sized genCreateAccessTokenOption

genCreateAccessTokenOption :: Int -> Gen CreateAccessTokenOption
genCreateAccessTokenOption n =
  CreateAccessTokenOption
    <$> arbitrary -- createAccessTokenOptionName :: Text
    <*> arbitraryReducedMaybe n -- createAccessTokenOptionScopes :: Maybe [Text]
  
instance Arbitrary CreateBranchProtectionOption where
  arbitrary = sized genCreateBranchProtectionOption

genCreateBranchProtectionOption :: Int -> Gen CreateBranchProtectionOption
genCreateBranchProtectionOption n =
  CreateBranchProtectionOption
    <$> arbitraryReducedMaybe n -- createBranchProtectionOptionApprovalsWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionApprovalsWhitelistUsername :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionBlockOnOfficialReviewRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionBlockOnOutdatedBranch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionBlockOnRejectedReviews :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionBranchName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionDismissStaleApprovals :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionEnableApprovalsWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionEnableMergeWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionEnablePush :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionEnablePushWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionEnableStatusCheck :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionMergeWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionMergeWhitelistUsernames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionProtectedFilePatterns :: Maybe Text
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionPushWhitelistDeployKeys :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionPushWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionPushWhitelistUsernames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionRequireSignedCommits :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionRequiredApprovals :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionRuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionStatusCheckContexts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createBranchProtectionOptionUnprotectedFilePatterns :: Maybe Text
  
instance Arbitrary CreateBranchRepoOption where
  arbitrary = sized genCreateBranchRepoOption

genCreateBranchRepoOption :: Int -> Gen CreateBranchRepoOption
genCreateBranchRepoOption n =
  CreateBranchRepoOption
    <$> arbitrary -- createBranchRepoOptionNewBranchName :: Text
    <*> arbitraryReducedMaybe n -- createBranchRepoOptionOldBranchName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createBranchRepoOptionOldRefName :: Maybe Text
  
instance Arbitrary CreateEmailOption where
  arbitrary = sized genCreateEmailOption

genCreateEmailOption :: Int -> Gen CreateEmailOption
genCreateEmailOption n =
  CreateEmailOption
    <$> arbitraryReducedMaybe n -- createEmailOptionEmails :: Maybe [Text]
  
instance Arbitrary CreateFileOptions where
  arbitrary = sized genCreateFileOptions

genCreateFileOptions :: Int -> Gen CreateFileOptions
genCreateFileOptions n =
  CreateFileOptions
    <$> arbitraryReducedMaybe n -- createFileOptionsAuthor :: Maybe Identity
    <*> arbitraryReducedMaybe n -- createFileOptionsBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- createFileOptionsCommitter :: Maybe Identity
    <*> arbitrary -- createFileOptionsContent :: Text
    <*> arbitraryReducedMaybe n -- createFileOptionsDates :: Maybe CommitDateOptions
    <*> arbitraryReducedMaybe n -- createFileOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- createFileOptionsNewBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- createFileOptionsSignoff :: Maybe Bool
  
instance Arbitrary CreateForkOption where
  arbitrary = sized genCreateForkOption

genCreateForkOption :: Int -> Gen CreateForkOption
genCreateForkOption n =
  CreateForkOption
    <$> arbitraryReducedMaybe n -- createForkOptionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createForkOptionOrganization :: Maybe Text
  
instance Arbitrary CreateGPGKeyOption where
  arbitrary = sized genCreateGPGKeyOption

genCreateGPGKeyOption :: Int -> Gen CreateGPGKeyOption
genCreateGPGKeyOption n =
  CreateGPGKeyOption
    <$> arbitrary -- createGPGKeyOptionArmoredPublicKey :: Text
    <*> arbitraryReducedMaybe n -- createGPGKeyOptionArmoredSignature :: Maybe Text
  
instance Arbitrary CreateHookOption where
  arbitrary = sized genCreateHookOption

genCreateHookOption :: Int -> Gen CreateHookOption
genCreateHookOption n =
  CreateHookOption
    <$> arbitraryReducedMaybe n -- createHookOptionActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createHookOptionAuthorizationHeader :: Maybe Text
    <*> arbitraryReducedMaybe n -- createHookOptionBranchFilter :: Maybe Text
    <*> arbitrary -- createHookOptionConfig :: (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- createHookOptionEvents :: Maybe [Text]
    <*> arbitrary -- createHookOptionType :: E'Type
  
instance Arbitrary CreateIssueCommentOption where
  arbitrary = sized genCreateIssueCommentOption

genCreateIssueCommentOption :: Int -> Gen CreateIssueCommentOption
genCreateIssueCommentOption n =
  CreateIssueCommentOption
    <$> arbitrary -- createIssueCommentOptionBody :: Text
  
instance Arbitrary CreateIssueOption where
  arbitrary = sized genCreateIssueOption

genCreateIssueOption :: Int -> Gen CreateIssueOption
genCreateIssueOption n =
  CreateIssueOption
    <$> arbitraryReducedMaybe n -- createIssueOptionAssignee :: Maybe Text
    <*> arbitraryReducedMaybe n -- createIssueOptionAssignees :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createIssueOptionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- createIssueOptionClosed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createIssueOptionDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- createIssueOptionLabels :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- createIssueOptionMilestone :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createIssueOptionRef :: Maybe Text
    <*> arbitrary -- createIssueOptionTitle :: Text
  
instance Arbitrary CreateKeyOption where
  arbitrary = sized genCreateKeyOption

genCreateKeyOption :: Int -> Gen CreateKeyOption
genCreateKeyOption n =
  CreateKeyOption
    <$> arbitrary -- createKeyOptionKey :: Text
    <*> arbitraryReducedMaybe n -- createKeyOptionReadOnly :: Maybe Bool
    <*> arbitrary -- createKeyOptionTitle :: Text
  
instance Arbitrary CreateLabelOption where
  arbitrary = sized genCreateLabelOption

genCreateLabelOption :: Int -> Gen CreateLabelOption
genCreateLabelOption n =
  CreateLabelOption
    <$> arbitrary -- createLabelOptionColor :: Text
    <*> arbitraryReducedMaybe n -- createLabelOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createLabelOptionExclusive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createLabelOptionIsArchived :: Maybe Bool
    <*> arbitrary -- createLabelOptionName :: Text
  
instance Arbitrary CreateMilestoneOption where
  arbitrary = sized genCreateMilestoneOption

genCreateMilestoneOption :: Int -> Gen CreateMilestoneOption
genCreateMilestoneOption n =
  CreateMilestoneOption
    <$> arbitraryReducedMaybe n -- createMilestoneOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createMilestoneOptionDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- createMilestoneOptionState :: Maybe E'State
    <*> arbitraryReducedMaybe n -- createMilestoneOptionTitle :: Maybe Text
  
instance Arbitrary CreateOAuth2ApplicationOptions where
  arbitrary = sized genCreateOAuth2ApplicationOptions

genCreateOAuth2ApplicationOptions :: Int -> Gen CreateOAuth2ApplicationOptions
genCreateOAuth2ApplicationOptions n =
  CreateOAuth2ApplicationOptions
    <$> arbitraryReducedMaybe n -- createOAuth2ApplicationOptionsConfidentialClient :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createOAuth2ApplicationOptionsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createOAuth2ApplicationOptionsRedirectUris :: Maybe [Text]
  
instance Arbitrary CreateOrUpdateSecretOption where
  arbitrary = sized genCreateOrUpdateSecretOption

genCreateOrUpdateSecretOption :: Int -> Gen CreateOrUpdateSecretOption
genCreateOrUpdateSecretOption n =
  CreateOrUpdateSecretOption
    <$> arbitrary -- createOrUpdateSecretOptionData :: Text
  
instance Arbitrary CreateOrgOption where
  arbitrary = sized genCreateOrgOption

genCreateOrgOption :: Int -> Gen CreateOrgOption
genCreateOrgOption n =
  CreateOrgOption
    <$> arbitraryReducedMaybe n -- createOrgOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createOrgOptionEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- createOrgOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createOrgOptionLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- createOrgOptionRepoAdminChangeTeamAccess :: Maybe Bool
    <*> arbitrary -- createOrgOptionUsername :: Text
    <*> arbitraryReducedMaybe n -- createOrgOptionVisibility :: Maybe E'Visibility
    <*> arbitraryReducedMaybe n -- createOrgOptionWebsite :: Maybe Text
  
instance Arbitrary CreatePullRequestOption where
  arbitrary = sized genCreatePullRequestOption

genCreatePullRequestOption :: Int -> Gen CreatePullRequestOption
genCreatePullRequestOption n =
  CreatePullRequestOption
    <$> arbitraryReducedMaybe n -- createPullRequestOptionAssignee :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPullRequestOptionAssignees :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createPullRequestOptionBase :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPullRequestOptionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPullRequestOptionDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- createPullRequestOptionHead :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPullRequestOptionLabels :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- createPullRequestOptionMilestone :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createPullRequestOptionTitle :: Maybe Text
  
instance Arbitrary CreatePullReviewComment where
  arbitrary = sized genCreatePullReviewComment

genCreatePullReviewComment :: Int -> Gen CreatePullReviewComment
genCreatePullReviewComment n =
  CreatePullReviewComment
    <$> arbitraryReducedMaybe n -- createPullReviewCommentBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPullReviewCommentNewPosition :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createPullReviewCommentOldPosition :: Maybe Integer
    <*> arbitraryReducedMaybe n -- createPullReviewCommentPath :: Maybe Text
  
instance Arbitrary CreatePullReviewOptions where
  arbitrary = sized genCreatePullReviewOptions

genCreatePullReviewOptions :: Int -> Gen CreatePullReviewOptions
genCreatePullReviewOptions n =
  CreatePullReviewOptions
    <$> arbitraryReducedMaybe n -- createPullReviewOptionsBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPullReviewOptionsComments :: Maybe [CreatePullReviewComment]
    <*> arbitraryReducedMaybe n -- createPullReviewOptionsCommitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPullReviewOptionsEvent :: Maybe Text
  
instance Arbitrary CreatePushMirrorOption where
  arbitrary = sized genCreatePushMirrorOption

genCreatePushMirrorOption :: Int -> Gen CreatePushMirrorOption
genCreatePushMirrorOption n =
  CreatePushMirrorOption
    <$> arbitraryReducedMaybe n -- createPushMirrorOptionInterval :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPushMirrorOptionRemoteAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPushMirrorOptionRemotePassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPushMirrorOptionRemoteUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- createPushMirrorOptionSyncOnCommit :: Maybe Bool
  
instance Arbitrary CreateReleaseOption where
  arbitrary = sized genCreateReleaseOption

genCreateReleaseOption :: Int -> Gen CreateReleaseOption
genCreateReleaseOption n =
  CreateReleaseOption
    <$> arbitraryReducedMaybe n -- createReleaseOptionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- createReleaseOptionDraft :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createReleaseOptionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createReleaseOptionPrerelease :: Maybe Bool
    <*> arbitrary -- createReleaseOptionTagName :: Text
    <*> arbitraryReducedMaybe n -- createReleaseOptionTargetCommitish :: Maybe Text
  
instance Arbitrary CreateRepoOption where
  arbitrary = sized genCreateRepoOption

genCreateRepoOption :: Int -> Gen CreateRepoOption
genCreateRepoOption n =
  CreateRepoOption
    <$> arbitraryReducedMaybe n -- createRepoOptionAutoInit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createRepoOptionDefaultBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- createRepoOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createRepoOptionGitignores :: Maybe Text
    <*> arbitraryReducedMaybe n -- createRepoOptionIssueLabels :: Maybe Text
    <*> arbitraryReducedMaybe n -- createRepoOptionLicense :: Maybe Text
    <*> arbitrary -- createRepoOptionName :: Text
    <*> arbitraryReducedMaybe n -- createRepoOptionPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createRepoOptionReadme :: Maybe Text
    <*> arbitraryReducedMaybe n -- createRepoOptionTemplate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createRepoOptionTrustModel :: Maybe E'TrustModel
  
instance Arbitrary CreateStatusOption where
  arbitrary = sized genCreateStatusOption

genCreateStatusOption :: Int -> Gen CreateStatusOption
genCreateStatusOption n =
  CreateStatusOption
    <$> arbitraryReducedMaybe n -- createStatusOptionContext :: Maybe Text
    <*> arbitraryReducedMaybe n -- createStatusOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createStatusOptionState :: Maybe Text
    <*> arbitraryReducedMaybe n -- createStatusOptionTargetUrl :: Maybe Text
  
instance Arbitrary CreateTagOption where
  arbitrary = sized genCreateTagOption

genCreateTagOption :: Int -> Gen CreateTagOption
genCreateTagOption n =
  CreateTagOption
    <$> arbitraryReducedMaybe n -- createTagOptionMessage :: Maybe Text
    <*> arbitrary -- createTagOptionTagName :: Text
    <*> arbitraryReducedMaybe n -- createTagOptionTarget :: Maybe Text
  
instance Arbitrary CreateTeamOption where
  arbitrary = sized genCreateTeamOption

genCreateTeamOption :: Int -> Gen CreateTeamOption
genCreateTeamOption n =
  CreateTeamOption
    <$> arbitraryReducedMaybe n -- createTeamOptionCanCreateOrgRepo :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createTeamOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createTeamOptionIncludesAllRepositories :: Maybe Bool
    <*> arbitrary -- createTeamOptionName :: Text
    <*> arbitraryReducedMaybe n -- createTeamOptionPermission :: Maybe E'Permission
    <*> arbitraryReducedMaybe n -- createTeamOptionUnits :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createTeamOptionUnitsMap :: Maybe (Map.Map String Text)
  
instance Arbitrary CreateUserOption where
  arbitrary = sized genCreateUserOption

genCreateUserOption :: Int -> Gen CreateUserOption
genCreateUserOption n =
  CreateUserOption
    <$> arbitraryReducedMaybe n -- createUserOptionCreatedAt :: Maybe DateTime
    <*> arbitrary -- createUserOptionEmail :: Text
    <*> arbitraryReducedMaybe n -- createUserOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createUserOptionLoginName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createUserOptionMustChangePassword :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createUserOptionPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- createUserOptionRestricted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createUserOptionSendNotify :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createUserOptionSourceId :: Maybe Integer
    <*> arbitrary -- createUserOptionUsername :: Text
    <*> arbitraryReducedMaybe n -- createUserOptionVisibility :: Maybe Text
  
instance Arbitrary CreateWikiPageOptions where
  arbitrary = sized genCreateWikiPageOptions

genCreateWikiPageOptions :: Int -> Gen CreateWikiPageOptions
genCreateWikiPageOptions n =
  CreateWikiPageOptions
    <$> arbitraryReducedMaybe n -- createWikiPageOptionsContentBase64 :: Maybe Text
    <*> arbitraryReducedMaybe n -- createWikiPageOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- createWikiPageOptionsTitle :: Maybe Text
  
instance Arbitrary Cron where
  arbitrary = sized genCron

genCron :: Int -> Gen Cron
genCron n =
  Cron
    <$> arbitraryReducedMaybe n -- cronExecTimes :: Maybe Integer
    <*> arbitraryReducedMaybe n -- cronName :: Maybe Text
    <*> arbitraryReducedMaybe n -- cronNext :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- cronPrev :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- cronSchedule :: Maybe Text
  
instance Arbitrary DeleteEmailOption where
  arbitrary = sized genDeleteEmailOption

genDeleteEmailOption :: Int -> Gen DeleteEmailOption
genDeleteEmailOption n =
  DeleteEmailOption
    <$> arbitraryReducedMaybe n -- deleteEmailOptionEmails :: Maybe [Text]
  
instance Arbitrary DeleteFileOptions where
  arbitrary = sized genDeleteFileOptions

genDeleteFileOptions :: Int -> Gen DeleteFileOptions
genDeleteFileOptions n =
  DeleteFileOptions
    <$> arbitraryReducedMaybe n -- deleteFileOptionsAuthor :: Maybe Identity
    <*> arbitraryReducedMaybe n -- deleteFileOptionsBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteFileOptionsCommitter :: Maybe Identity
    <*> arbitraryReducedMaybe n -- deleteFileOptionsDates :: Maybe CommitDateOptions
    <*> arbitraryReducedMaybe n -- deleteFileOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteFileOptionsNewBranch :: Maybe Text
    <*> arbitrary -- deleteFileOptionsSha :: Text
    <*> arbitraryReducedMaybe n -- deleteFileOptionsSignoff :: Maybe Bool
  
instance Arbitrary DeployKey where
  arbitrary = sized genDeployKey

genDeployKey :: Int -> Gen DeployKey
genDeployKey n =
  DeployKey
    <$> arbitraryReducedMaybe n -- deployKeyCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- deployKeyFingerprint :: Maybe Text
    <*> arbitraryReducedMaybe n -- deployKeyId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- deployKeyKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- deployKeyKeyId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- deployKeyReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- deployKeyRepository :: Maybe Repository
    <*> arbitraryReducedMaybe n -- deployKeyTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- deployKeyUrl :: Maybe Text
  
instance Arbitrary DismissPullReviewOptions where
  arbitrary = sized genDismissPullReviewOptions

genDismissPullReviewOptions :: Int -> Gen DismissPullReviewOptions
genDismissPullReviewOptions n =
  DismissPullReviewOptions
    <$> arbitraryReducedMaybe n -- dismissPullReviewOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- dismissPullReviewOptionsPriors :: Maybe Bool
  
instance Arbitrary EditAttachmentOptions where
  arbitrary = sized genEditAttachmentOptions

genEditAttachmentOptions :: Int -> Gen EditAttachmentOptions
genEditAttachmentOptions n =
  EditAttachmentOptions
    <$> arbitraryReducedMaybe n -- editAttachmentOptionsName :: Maybe Text
  
instance Arbitrary EditBranchProtectionOption where
  arbitrary = sized genEditBranchProtectionOption

genEditBranchProtectionOption :: Int -> Gen EditBranchProtectionOption
genEditBranchProtectionOption n =
  EditBranchProtectionOption
    <$> arbitraryReducedMaybe n -- editBranchProtectionOptionApprovalsWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionApprovalsWhitelistUsername :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionBlockOnOfficialReviewRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionBlockOnOutdatedBranch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionBlockOnRejectedReviews :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionDismissStaleApprovals :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionEnableApprovalsWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionEnableMergeWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionEnablePush :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionEnablePushWhitelist :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionEnableStatusCheck :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionMergeWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionMergeWhitelistUsernames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionProtectedFilePatterns :: Maybe Text
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionPushWhitelistDeployKeys :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionPushWhitelistTeams :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionPushWhitelistUsernames :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionRequireSignedCommits :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionRequiredApprovals :: Maybe Integer
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionStatusCheckContexts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editBranchProtectionOptionUnprotectedFilePatterns :: Maybe Text
  
instance Arbitrary EditDeadlineOption where
  arbitrary = sized genEditDeadlineOption

genEditDeadlineOption :: Int -> Gen EditDeadlineOption
genEditDeadlineOption n =
  EditDeadlineOption
    <$> arbitraryReduced n -- editDeadlineOptionDueDate :: DateTime
  
instance Arbitrary EditGitHookOption where
  arbitrary = sized genEditGitHookOption

genEditGitHookOption :: Int -> Gen EditGitHookOption
genEditGitHookOption n =
  EditGitHookOption
    <$> arbitraryReducedMaybe n -- editGitHookOptionContent :: Maybe Text
  
instance Arbitrary EditHookOption where
  arbitrary = sized genEditHookOption

genEditHookOption :: Int -> Gen EditHookOption
genEditHookOption n =
  EditHookOption
    <$> arbitraryReducedMaybe n -- editHookOptionActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editHookOptionAuthorizationHeader :: Maybe Text
    <*> arbitraryReducedMaybe n -- editHookOptionBranchFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- editHookOptionConfig :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- editHookOptionEvents :: Maybe [Text]
  
instance Arbitrary EditIssueCommentOption where
  arbitrary = sized genEditIssueCommentOption

genEditIssueCommentOption :: Int -> Gen EditIssueCommentOption
genEditIssueCommentOption n =
  EditIssueCommentOption
    <$> arbitrary -- editIssueCommentOptionBody :: Text
  
instance Arbitrary EditIssueOption where
  arbitrary = sized genEditIssueOption

genEditIssueOption :: Int -> Gen EditIssueOption
genEditIssueOption n =
  EditIssueOption
    <$> arbitraryReducedMaybe n -- editIssueOptionAssignee :: Maybe Text
    <*> arbitraryReducedMaybe n -- editIssueOptionAssignees :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editIssueOptionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- editIssueOptionDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- editIssueOptionMilestone :: Maybe Integer
    <*> arbitraryReducedMaybe n -- editIssueOptionRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- editIssueOptionState :: Maybe Text
    <*> arbitraryReducedMaybe n -- editIssueOptionTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- editIssueOptionUnsetDueDate :: Maybe Bool
  
instance Arbitrary EditLabelOption where
  arbitrary = sized genEditLabelOption

genEditLabelOption :: Int -> Gen EditLabelOption
genEditLabelOption n =
  EditLabelOption
    <$> arbitraryReducedMaybe n -- editLabelOptionColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- editLabelOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editLabelOptionExclusive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editLabelOptionIsArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editLabelOptionName :: Maybe Text
  
instance Arbitrary EditMilestoneOption where
  arbitrary = sized genEditMilestoneOption

genEditMilestoneOption :: Int -> Gen EditMilestoneOption
genEditMilestoneOption n =
  EditMilestoneOption
    <$> arbitraryReducedMaybe n -- editMilestoneOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editMilestoneOptionDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- editMilestoneOptionState :: Maybe Text
    <*> arbitraryReducedMaybe n -- editMilestoneOptionTitle :: Maybe Text
  
instance Arbitrary EditOrgOption where
  arbitrary = sized genEditOrgOption

genEditOrgOption :: Int -> Gen EditOrgOption
genEditOrgOption n =
  EditOrgOption
    <$> arbitraryReducedMaybe n -- editOrgOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editOrgOptionEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- editOrgOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editOrgOptionLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- editOrgOptionRepoAdminChangeTeamAccess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editOrgOptionVisibility :: Maybe E'Visibility
    <*> arbitraryReducedMaybe n -- editOrgOptionWebsite :: Maybe Text
  
instance Arbitrary EditPullRequestOption where
  arbitrary = sized genEditPullRequestOption

genEditPullRequestOption :: Int -> Gen EditPullRequestOption
genEditPullRequestOption n =
  EditPullRequestOption
    <$> arbitraryReducedMaybe n -- editPullRequestOptionAllowMaintainerEdit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editPullRequestOptionAssignee :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionAssignees :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editPullRequestOptionBase :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- editPullRequestOptionLabels :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- editPullRequestOptionMilestone :: Maybe Integer
    <*> arbitraryReducedMaybe n -- editPullRequestOptionState :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionUnsetDueDate :: Maybe Bool
  
instance Arbitrary EditReactionOption where
  arbitrary = sized genEditReactionOption

genEditReactionOption :: Int -> Gen EditReactionOption
genEditReactionOption n =
  EditReactionOption
    <$> arbitraryReducedMaybe n -- editReactionOptionContent :: Maybe Text
  
instance Arbitrary EditReleaseOption where
  arbitrary = sized genEditReleaseOption

genEditReleaseOption :: Int -> Gen EditReleaseOption
genEditReleaseOption n =
  EditReleaseOption
    <$> arbitraryReducedMaybe n -- editReleaseOptionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- editReleaseOptionDraft :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editReleaseOptionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editReleaseOptionPrerelease :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editReleaseOptionTagName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editReleaseOptionTargetCommitish :: Maybe Text
  
instance Arbitrary EditRepoOption where
  arbitrary = sized genEditRepoOption

genEditRepoOption :: Int -> Gen EditRepoOption
genEditRepoOption n =
  EditRepoOption
    <$> arbitraryReducedMaybe n -- editRepoOptionAllowManualMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowMergeCommits :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowRebase :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowRebaseExplicit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowRebaseUpdate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowSquashMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAutodetectManualMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionDefaultAllowMaintainerEdit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionDefaultBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionDefaultDeleteBranchAfterMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionDefaultMergeStyle :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionEnablePrune :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionExternalTracker :: Maybe ExternalTracker
    <*> arbitraryReducedMaybe n -- editRepoOptionExternalWiki :: Maybe ExternalWiki
    <*> arbitraryReducedMaybe n -- editRepoOptionHasActions :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasIssues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasPackages :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasProjects :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasPullRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasReleases :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasWiki :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionIgnoreWhitespaceConflicts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionInternalTracker :: Maybe InternalTracker
    <*> arbitraryReducedMaybe n -- editRepoOptionMirrorInterval :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionTemplate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionWebsite :: Maybe Text
  
instance Arbitrary EditTeamOption where
  arbitrary = sized genEditTeamOption

genEditTeamOption :: Int -> Gen EditTeamOption
genEditTeamOption n =
  EditTeamOption
    <$> arbitraryReducedMaybe n -- editTeamOptionCanCreateOrgRepo :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editTeamOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editTeamOptionIncludesAllRepositories :: Maybe Bool
    <*> arbitrary -- editTeamOptionName :: Text
    <*> arbitraryReducedMaybe n -- editTeamOptionPermission :: Maybe E'Permission
    <*> arbitraryReducedMaybe n -- editTeamOptionUnits :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editTeamOptionUnitsMap :: Maybe (Map.Map String Text)
  
instance Arbitrary EditUserOption where
  arbitrary = sized genEditUserOption

genEditUserOption :: Int -> Gen EditUserOption
genEditUserOption n =
  EditUserOption
    <$> arbitraryReducedMaybe n -- editUserOptionActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionAllowCreateOrganization :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionAllowGitHook :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionAllowImportLocal :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionLocation :: Maybe Text
    <*> arbitrary -- editUserOptionLoginName :: Text
    <*> arbitraryReducedMaybe n -- editUserOptionMaxRepoCreation :: Maybe Integer
    <*> arbitraryReducedMaybe n -- editUserOptionMustChangePassword :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionProhibitLogin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionRestricted :: Maybe Bool
    <*> arbitrary -- editUserOptionSourceId :: Integer
    <*> arbitraryReducedMaybe n -- editUserOptionVisibility :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionWebsite :: Maybe Text
  
instance Arbitrary Email where
  arbitrary = sized genEmail

genEmail :: Int -> Gen Email
genEmail n =
  Email
    <$> arbitraryReducedMaybe n -- emailEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailPrimary :: Maybe Bool
    <*> arbitraryReducedMaybe n -- emailUserId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- emailUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailVerified :: Maybe Bool
  
instance Arbitrary ExternalTracker where
  arbitrary = sized genExternalTracker

genExternalTracker :: Int -> Gen ExternalTracker
genExternalTracker n =
  ExternalTracker
    <$> arbitraryReducedMaybe n -- externalTrackerExternalTrackerFormat :: Maybe Text
    <*> arbitraryReducedMaybe n -- externalTrackerExternalTrackerRegexpPattern :: Maybe Text
    <*> arbitraryReducedMaybe n -- externalTrackerExternalTrackerStyle :: Maybe Text
    <*> arbitraryReducedMaybe n -- externalTrackerExternalTrackerUrl :: Maybe Text
  
instance Arbitrary ExternalWiki where
  arbitrary = sized genExternalWiki

genExternalWiki :: Int -> Gen ExternalWiki
genExternalWiki n =
  ExternalWiki
    <$> arbitraryReducedMaybe n -- externalWikiExternalWikiUrl :: Maybe Text
  
instance Arbitrary FileCommitResponse where
  arbitrary = sized genFileCommitResponse

genFileCommitResponse :: Int -> Gen FileCommitResponse
genFileCommitResponse n =
  FileCommitResponse
    <$> arbitraryReducedMaybe n -- fileCommitResponseAuthor :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- fileCommitResponseCommitter :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- fileCommitResponseCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- fileCommitResponseHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileCommitResponseMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileCommitResponseParents :: Maybe [CommitMeta]
    <*> arbitraryReducedMaybe n -- fileCommitResponseSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileCommitResponseTree :: Maybe CommitMeta
    <*> arbitraryReducedMaybe n -- fileCommitResponseUrl :: Maybe Text
  
instance Arbitrary FileDeleteResponse where
  arbitrary = sized genFileDeleteResponse

genFileDeleteResponse :: Int -> Gen FileDeleteResponse
genFileDeleteResponse n =
  FileDeleteResponse
    <$> arbitraryReducedMaybe n -- fileDeleteResponseCommit :: Maybe FileCommitResponse
    <*> arbitraryReducedMaybeValue n -- fileDeleteResponseContent :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- fileDeleteResponseVerification :: Maybe PayloadCommitVerification
  
instance Arbitrary FileLinksResponse where
  arbitrary = sized genFileLinksResponse

genFileLinksResponse :: Int -> Gen FileLinksResponse
genFileLinksResponse n =
  FileLinksResponse
    <$> arbitraryReducedMaybe n -- fileLinksResponseGit :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileLinksResponseHtml :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileLinksResponseSelf :: Maybe Text
  
instance Arbitrary FileResponse where
  arbitrary = sized genFileResponse

genFileResponse :: Int -> Gen FileResponse
genFileResponse n =
  FileResponse
    <$> arbitraryReducedMaybe n -- fileResponseCommit :: Maybe FileCommitResponse
    <*> arbitraryReducedMaybe n -- fileResponseContent :: Maybe ContentsResponse
    <*> arbitraryReducedMaybe n -- fileResponseVerification :: Maybe PayloadCommitVerification
  
instance Arbitrary FilesResponse where
  arbitrary = sized genFilesResponse

genFilesResponse :: Int -> Gen FilesResponse
genFilesResponse n =
  FilesResponse
    <$> arbitraryReducedMaybe n -- filesResponseCommit :: Maybe FileCommitResponse
    <*> arbitraryReducedMaybe n -- filesResponseFiles :: Maybe [ContentsResponse]
    <*> arbitraryReducedMaybe n -- filesResponseVerification :: Maybe PayloadCommitVerification
  
instance Arbitrary GPGKey where
  arbitrary = sized genGPGKey

genGPGKey :: Int -> Gen GPGKey
genGPGKey n =
  GPGKey
    <$> arbitraryReducedMaybe n -- gPGKeyCanCertify :: Maybe Bool
    <*> arbitraryReducedMaybe n -- gPGKeyCanEncryptComms :: Maybe Bool
    <*> arbitraryReducedMaybe n -- gPGKeyCanEncryptStorage :: Maybe Bool
    <*> arbitraryReducedMaybe n -- gPGKeyCanSign :: Maybe Bool
    <*> arbitraryReducedMaybe n -- gPGKeyCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- gPGKeyEmails :: Maybe [GPGKeyEmail]
    <*> arbitraryReducedMaybe n -- gPGKeyExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- gPGKeyId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- gPGKeyKeyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- gPGKeyPrimaryKeyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- gPGKeyPublicKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- gPGKeySubkeys :: Maybe [GPGKey]
    <*> arbitraryReducedMaybe n -- gPGKeyVerified :: Maybe Bool
  
instance Arbitrary GPGKeyEmail where
  arbitrary = sized genGPGKeyEmail

genGPGKeyEmail :: Int -> Gen GPGKeyEmail
genGPGKeyEmail n =
  GPGKeyEmail
    <$> arbitraryReducedMaybe n -- gPGKeyEmailEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- gPGKeyEmailVerified :: Maybe Bool
  
instance Arbitrary GeneralAPISettings where
  arbitrary = sized genGeneralAPISettings

genGeneralAPISettings :: Int -> Gen GeneralAPISettings
genGeneralAPISettings n =
  GeneralAPISettings
    <$> arbitraryReducedMaybe n -- generalAPISettingsDefaultGitTreesPerPage :: Maybe Integer
    <*> arbitraryReducedMaybe n -- generalAPISettingsDefaultMaxBlobSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- generalAPISettingsDefaultPagingNum :: Maybe Integer
    <*> arbitraryReducedMaybe n -- generalAPISettingsMaxResponseItems :: Maybe Integer
  
instance Arbitrary GeneralAttachmentSettings where
  arbitrary = sized genGeneralAttachmentSettings

genGeneralAttachmentSettings :: Int -> Gen GeneralAttachmentSettings
genGeneralAttachmentSettings n =
  GeneralAttachmentSettings
    <$> arbitraryReducedMaybe n -- generalAttachmentSettingsAllowedTypes :: Maybe Text
    <*> arbitraryReducedMaybe n -- generalAttachmentSettingsEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generalAttachmentSettingsMaxFiles :: Maybe Integer
    <*> arbitraryReducedMaybe n -- generalAttachmentSettingsMaxSize :: Maybe Integer
  
instance Arbitrary GeneralRepoSettings where
  arbitrary = sized genGeneralRepoSettings

genGeneralRepoSettings :: Int -> Gen GeneralRepoSettings
genGeneralRepoSettings n =
  GeneralRepoSettings
    <$> arbitraryReducedMaybe n -- generalRepoSettingsHttpGitDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generalRepoSettingsLfsDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generalRepoSettingsMigrationsDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generalRepoSettingsMirrorsDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generalRepoSettingsStarsDisabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generalRepoSettingsTimeTrackingDisabled :: Maybe Bool
  
instance Arbitrary GeneralUISettings where
  arbitrary = sized genGeneralUISettings

genGeneralUISettings :: Int -> Gen GeneralUISettings
genGeneralUISettings n =
  GeneralUISettings
    <$> arbitraryReducedMaybe n -- generalUISettingsAllowedReactions :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- generalUISettingsCustomEmojis :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- generalUISettingsDefaultTheme :: Maybe Text
  
instance Arbitrary GenerateRepoOption where
  arbitrary = sized genGenerateRepoOption

genGenerateRepoOption :: Int -> Gen GenerateRepoOption
genGenerateRepoOption n =
  GenerateRepoOption
    <$> arbitraryReducedMaybe n -- generateRepoOptionAvatar :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generateRepoOptionDefaultBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- generateRepoOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- generateRepoOptionGitContent :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generateRepoOptionGitHooks :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generateRepoOptionLabels :: Maybe Bool
    <*> arbitrary -- generateRepoOptionName :: Text
    <*> arbitrary -- generateRepoOptionOwner :: Text
    <*> arbitraryReducedMaybe n -- generateRepoOptionPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generateRepoOptionProtectedBranch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generateRepoOptionTopics :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generateRepoOptionWebhooks :: Maybe Bool
  
instance Arbitrary GitBlobResponse where
  arbitrary = sized genGitBlobResponse

genGitBlobResponse :: Int -> Gen GitBlobResponse
genGitBlobResponse n =
  GitBlobResponse
    <$> arbitraryReducedMaybe n -- gitBlobResponseContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitBlobResponseEncoding :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitBlobResponseSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitBlobResponseSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- gitBlobResponseUrl :: Maybe Text
  
instance Arbitrary GitEntry where
  arbitrary = sized genGitEntry

genGitEntry :: Int -> Gen GitEntry
genGitEntry n =
  GitEntry
    <$> arbitraryReducedMaybe n -- gitEntryMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitEntryPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitEntrySha :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitEntrySize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- gitEntryType :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitEntryUrl :: Maybe Text
  
instance Arbitrary GitHook where
  arbitrary = sized genGitHook

genGitHook :: Int -> Gen GitHook
genGitHook n =
  GitHook
    <$> arbitraryReducedMaybe n -- gitHookContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitHookIsActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- gitHookName :: Maybe Text
  
instance Arbitrary GitObject where
  arbitrary = sized genGitObject

genGitObject :: Int -> Gen GitObject
genGitObject n =
  GitObject
    <$> arbitraryReducedMaybe n -- gitObjectSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitObjectType :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitObjectUrl :: Maybe Text
  
instance Arbitrary GitTreeResponse where
  arbitrary = sized genGitTreeResponse

genGitTreeResponse :: Int -> Gen GitTreeResponse
genGitTreeResponse n =
  GitTreeResponse
    <$> arbitraryReducedMaybe n -- gitTreeResponsePage :: Maybe Integer
    <*> arbitraryReducedMaybe n -- gitTreeResponseSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitTreeResponseTotalCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- gitTreeResponseTree :: Maybe [GitEntry]
    <*> arbitraryReducedMaybe n -- gitTreeResponseTruncated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- gitTreeResponseUrl :: Maybe Text
  
instance Arbitrary GitignoreTemplateInfo where
  arbitrary = sized genGitignoreTemplateInfo

genGitignoreTemplateInfo :: Int -> Gen GitignoreTemplateInfo
genGitignoreTemplateInfo n =
  GitignoreTemplateInfo
    <$> arbitraryReducedMaybe n -- gitignoreTemplateInfoName :: Maybe Text
    <*> arbitraryReducedMaybe n -- gitignoreTemplateInfoSource :: Maybe Text
  
instance Arbitrary Hook where
  arbitrary = sized genHook

genHook :: Int -> Gen Hook
genHook n =
  Hook
    <$> arbitraryReducedMaybe n -- hookActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- hookAuthorizationHeader :: Maybe Text
    <*> arbitraryReducedMaybe n -- hookBranchFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- hookConfig :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- hookCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- hookEvents :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- hookId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- hookType :: Maybe Text
    <*> arbitraryReducedMaybe n -- hookUpdatedAt :: Maybe DateTime
  
instance Arbitrary Identity where
  arbitrary = sized genIdentity

genIdentity :: Int -> Gen Identity
genIdentity n =
  Identity
    <$> arbitraryReducedMaybe n -- identityEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityName :: Maybe Text
  
instance Arbitrary InternalTracker where
  arbitrary = sized genInternalTracker

genInternalTracker :: Int -> Gen InternalTracker
genInternalTracker n =
  InternalTracker
    <$> arbitraryReducedMaybe n -- internalTrackerAllowOnlyContributorsToTrackTime :: Maybe Bool
    <*> arbitraryReducedMaybe n -- internalTrackerEnableIssueDependencies :: Maybe Bool
    <*> arbitraryReducedMaybe n -- internalTrackerEnableTimeTracker :: Maybe Bool
  
instance Arbitrary Issue where
  arbitrary = sized genIssue

genIssue :: Int -> Gen Issue
genIssue n =
  Issue
    <$> arbitraryReducedMaybe n -- issueAssets :: Maybe [Attachment]
    <*> arbitraryReducedMaybe n -- issueAssignee :: Maybe User
    <*> arbitraryReducedMaybe n -- issueAssignees :: Maybe [User]
    <*> arbitraryReducedMaybe n -- issueBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueClosedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueComments :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issueCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issueIsLocked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- issueLabels :: Maybe [Label]
    <*> arbitraryReducedMaybe n -- issueMilestone :: Maybe Milestone
    <*> arbitraryReducedMaybe n -- issueNumber :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issueOriginalAuthor :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueOriginalAuthorId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issuePinOrder :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issuePullRequest :: Maybe PullRequestMeta
    <*> arbitraryReducedMaybe n -- issueRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueRepository :: Maybe RepositoryMeta
    <*> arbitraryReducedMaybe n -- issueState :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueUser :: Maybe User
  
instance Arbitrary IssueConfig where
  arbitrary = sized genIssueConfig

genIssueConfig :: Int -> Gen IssueConfig
genIssueConfig n =
  IssueConfig
    <$> arbitraryReducedMaybe n -- issueConfigBlankIssuesEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- issueConfigContactLinks :: Maybe [IssueConfigContactLink]
  
instance Arbitrary IssueConfigContactLink where
  arbitrary = sized genIssueConfigContactLink

genIssueConfigContactLink :: Int -> Gen IssueConfigContactLink
genIssueConfigContactLink n =
  IssueConfigContactLink
    <$> arbitraryReducedMaybe n -- issueConfigContactLinkAbout :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueConfigContactLinkName :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueConfigContactLinkUrl :: Maybe Text
  
instance Arbitrary IssueConfigValidation where
  arbitrary = sized genIssueConfigValidation

genIssueConfigValidation :: Int -> Gen IssueConfigValidation
genIssueConfigValidation n =
  IssueConfigValidation
    <$> arbitraryReducedMaybe n -- issueConfigValidationMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueConfigValidationValid :: Maybe Bool
  
instance Arbitrary IssueDeadline where
  arbitrary = sized genIssueDeadline

genIssueDeadline :: Int -> Gen IssueDeadline
genIssueDeadline n =
  IssueDeadline
    <$> arbitraryReducedMaybe n -- issueDeadlineDueDate :: Maybe DateTime
  
instance Arbitrary IssueFormField where
  arbitrary = sized genIssueFormField

genIssueFormField :: Int -> Gen IssueFormField
genIssueFormField n =
  IssueFormField
    <$> arbitraryReducedMaybe n -- issueFormFieldAttributes :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- issueFormFieldId :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueFormFieldType :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueFormFieldValidations :: Maybe (Map.Map String A.Value)
  
instance Arbitrary IssueLabelsOption where
  arbitrary = sized genIssueLabelsOption

genIssueLabelsOption :: Int -> Gen IssueLabelsOption
genIssueLabelsOption n =
  IssueLabelsOption
    <$> arbitraryReducedMaybe n -- issueLabelsOptionLabels :: Maybe [Integer]
  
instance Arbitrary IssueMeta where
  arbitrary = sized genIssueMeta

genIssueMeta :: Int -> Gen IssueMeta
genIssueMeta n =
  IssueMeta
    <$> arbitraryReducedMaybe n -- issueMetaIndex :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issueMetaOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueMetaRepo :: Maybe Text
  
instance Arbitrary IssueTemplate where
  arbitrary = sized genIssueTemplate

genIssueTemplate :: Int -> Gen IssueTemplate
genIssueTemplate n =
  IssueTemplate
    <$> arbitraryReducedMaybe n -- issueTemplateAbout :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueTemplateBody :: Maybe [IssueFormField]
    <*> arbitraryReducedMaybe n -- issueTemplateContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueTemplateFileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueTemplateLabels :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- issueTemplateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueTemplateRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueTemplateTitle :: Maybe Text
  
instance Arbitrary Label where
  arbitrary = sized genLabel

genLabel :: Int -> Gen Label
genLabel n =
  Label
    <$> arbitraryReducedMaybe n -- labelColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelExclusive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- labelId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- labelIsArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- labelName :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelUrl :: Maybe Text
  
instance Arbitrary LabelTemplate where
  arbitrary = sized genLabelTemplate

genLabelTemplate :: Int -> Gen LabelTemplate
genLabelTemplate n =
  LabelTemplate
    <$> arbitraryReducedMaybe n -- labelTemplateColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelTemplateDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelTemplateExclusive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- labelTemplateName :: Maybe Text
  
instance Arbitrary LicenseTemplateInfo where
  arbitrary = sized genLicenseTemplateInfo

genLicenseTemplateInfo :: Int -> Gen LicenseTemplateInfo
genLicenseTemplateInfo n =
  LicenseTemplateInfo
    <$> arbitraryReducedMaybe n -- licenseTemplateInfoBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- licenseTemplateInfoImplementation :: Maybe Text
    <*> arbitraryReducedMaybe n -- licenseTemplateInfoKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- licenseTemplateInfoName :: Maybe Text
    <*> arbitraryReducedMaybe n -- licenseTemplateInfoUrl :: Maybe Text
  
instance Arbitrary LicensesTemplateListEntry where
  arbitrary = sized genLicensesTemplateListEntry

genLicensesTemplateListEntry :: Int -> Gen LicensesTemplateListEntry
genLicensesTemplateListEntry n =
  LicensesTemplateListEntry
    <$> arbitraryReducedMaybe n -- licensesTemplateListEntryKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- licensesTemplateListEntryName :: Maybe Text
    <*> arbitraryReducedMaybe n -- licensesTemplateListEntryUrl :: Maybe Text
  
instance Arbitrary MarkdownOption where
  arbitrary = sized genMarkdownOption

genMarkdownOption :: Int -> Gen MarkdownOption
genMarkdownOption n =
  MarkdownOption
    <$> arbitraryReducedMaybe n -- markdownOptionContext :: Maybe Text
    <*> arbitraryReducedMaybe n -- markdownOptionMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- markdownOptionText :: Maybe Text
    <*> arbitraryReducedMaybe n -- markdownOptionWiki :: Maybe Bool
  
instance Arbitrary MarkupOption where
  arbitrary = sized genMarkupOption

genMarkupOption :: Int -> Gen MarkupOption
genMarkupOption n =
  MarkupOption
    <$> arbitraryReducedMaybe n -- markupOptionContext :: Maybe Text
    <*> arbitraryReducedMaybe n -- markupOptionFilePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- markupOptionMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- markupOptionText :: Maybe Text
    <*> arbitraryReducedMaybe n -- markupOptionWiki :: Maybe Bool
  
instance Arbitrary MergePullRequestOption where
  arbitrary = sized genMergePullRequestOption

genMergePullRequestOption :: Int -> Gen MergePullRequestOption
genMergePullRequestOption n =
  MergePullRequestOption
    <$> arbitrary -- mergePullRequestOptionDo :: E'Do
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionMergeCommitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionMergeMessageField :: Maybe Text
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionMergeTitleField :: Maybe Text
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionDeleteBranchAfterMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionForceMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionHeadCommitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionMergeWhenChecksSucceed :: Maybe Bool
  
instance Arbitrary MigrateRepoOptions where
  arbitrary = sized genMigrateRepoOptions

genMigrateRepoOptions :: Int -> Gen MigrateRepoOptions
genMigrateRepoOptions n =
  MigrateRepoOptions
    <$> arbitraryReducedMaybe n -- migrateRepoOptionsAuthPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsAuthToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsAuthUsername :: Maybe Text
    <*> arbitrary -- migrateRepoOptionsCloneAddr :: Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsIssues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsLabels :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsLfs :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsLfsEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsMilestones :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsMirror :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsMirrorInterval :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsPullRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsReleases :: Maybe Bool
    <*> arbitrary -- migrateRepoOptionsRepoName :: Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsRepoOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsService :: Maybe E'Service
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsUid :: Maybe Integer
    <*> arbitraryReducedMaybe n -- migrateRepoOptionsWiki :: Maybe Bool
  
instance Arbitrary Milestone where
  arbitrary = sized genMilestone

genMilestone :: Int -> Gen Milestone
genMilestone n =
  Milestone
    <$> arbitraryReducedMaybe n -- milestoneClosedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- milestoneClosedIssues :: Maybe Integer
    <*> arbitraryReducedMaybe n -- milestoneCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- milestoneDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- milestoneDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- milestoneId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- milestoneOpenIssues :: Maybe Integer
    <*> arbitraryReducedMaybe n -- milestoneState :: Maybe Text
    <*> arbitraryReducedMaybe n -- milestoneTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- milestoneUpdatedAt :: Maybe DateTime
  
instance Arbitrary NewIssuePinsAllowed where
  arbitrary = sized genNewIssuePinsAllowed

genNewIssuePinsAllowed :: Int -> Gen NewIssuePinsAllowed
genNewIssuePinsAllowed n =
  NewIssuePinsAllowed
    <$> arbitraryReducedMaybe n -- newIssuePinsAllowedIssues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- newIssuePinsAllowedPullRequests :: Maybe Bool
  
instance Arbitrary NodeInfo where
  arbitrary = sized genNodeInfo

genNodeInfo :: Int -> Gen NodeInfo
genNodeInfo n =
  NodeInfo
    <$> arbitraryReducedMaybeValue n -- nodeInfoMetadata :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- nodeInfoOpenRegistrations :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nodeInfoProtocols :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nodeInfoServices :: Maybe NodeInfoServices
    <*> arbitraryReducedMaybe n -- nodeInfoSoftware :: Maybe NodeInfoSoftware
    <*> arbitraryReducedMaybe n -- nodeInfoUsage :: Maybe NodeInfoUsage
    <*> arbitraryReducedMaybe n -- nodeInfoVersion :: Maybe Text
  
instance Arbitrary NodeInfoServices where
  arbitrary = sized genNodeInfoServices

genNodeInfoServices :: Int -> Gen NodeInfoServices
genNodeInfoServices n =
  NodeInfoServices
    <$> arbitraryReducedMaybe n -- nodeInfoServicesInbound :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nodeInfoServicesOutbound :: Maybe [Text]
  
instance Arbitrary NodeInfoSoftware where
  arbitrary = sized genNodeInfoSoftware

genNodeInfoSoftware :: Int -> Gen NodeInfoSoftware
genNodeInfoSoftware n =
  NodeInfoSoftware
    <$> arbitraryReducedMaybe n -- nodeInfoSoftwareHomepage :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeInfoSoftwareName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeInfoSoftwareRepository :: Maybe Text
    <*> arbitraryReducedMaybe n -- nodeInfoSoftwareVersion :: Maybe Text
  
instance Arbitrary NodeInfoUsage where
  arbitrary = sized genNodeInfoUsage

genNodeInfoUsage :: Int -> Gen NodeInfoUsage
genNodeInfoUsage n =
  NodeInfoUsage
    <$> arbitraryReducedMaybe n -- nodeInfoUsageLocalComments :: Maybe Integer
    <*> arbitraryReducedMaybe n -- nodeInfoUsageLocalPosts :: Maybe Integer
    <*> arbitraryReducedMaybe n -- nodeInfoUsageUsers :: Maybe NodeInfoUsageUsers
  
instance Arbitrary NodeInfoUsageUsers where
  arbitrary = sized genNodeInfoUsageUsers

genNodeInfoUsageUsers :: Int -> Gen NodeInfoUsageUsers
genNodeInfoUsageUsers n =
  NodeInfoUsageUsers
    <$> arbitraryReducedMaybe n -- nodeInfoUsageUsersActiveHalfyear :: Maybe Integer
    <*> arbitraryReducedMaybe n -- nodeInfoUsageUsersActiveMonth :: Maybe Integer
    <*> arbitraryReducedMaybe n -- nodeInfoUsageUsersTotal :: Maybe Integer
  
instance Arbitrary Note where
  arbitrary = sized genNote

genNote :: Int -> Gen Note
genNote n =
  Note
    <$> arbitraryReducedMaybe n -- noteCommit :: Maybe Commit
    <*> arbitraryReducedMaybe n -- noteMessage :: Maybe Text
  
instance Arbitrary NotificationCount where
  arbitrary = sized genNotificationCount

genNotificationCount :: Int -> Gen NotificationCount
genNotificationCount n =
  NotificationCount
    <$> arbitraryReducedMaybe n -- notificationCountNew :: Maybe Integer
  
instance Arbitrary NotificationSubject where
  arbitrary = sized genNotificationSubject

genNotificationSubject :: Int -> Gen NotificationSubject
genNotificationSubject n =
  NotificationSubject
    <$> arbitraryReducedMaybe n -- notificationSubjectHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- notificationSubjectLatestCommentHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- notificationSubjectLatestCommentUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- notificationSubjectState :: Maybe Text
    <*> arbitraryReducedMaybe n -- notificationSubjectTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- notificationSubjectType :: Maybe Text
    <*> arbitraryReducedMaybe n -- notificationSubjectUrl :: Maybe Text
  
instance Arbitrary NotificationThread where
  arbitrary = sized genNotificationThread

genNotificationThread :: Int -> Gen NotificationThread
genNotificationThread n =
  NotificationThread
    <$> arbitraryReducedMaybe n -- notificationThreadId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- notificationThreadPinned :: Maybe Bool
    <*> arbitraryReducedMaybe n -- notificationThreadRepository :: Maybe Repository
    <*> arbitraryReducedMaybe n -- notificationThreadSubject :: Maybe NotificationSubject
    <*> arbitraryReducedMaybe n -- notificationThreadUnread :: Maybe Bool
    <*> arbitraryReducedMaybe n -- notificationThreadUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- notificationThreadUrl :: Maybe Text
  
instance Arbitrary OAuth2Application where
  arbitrary = sized genOAuth2Application

genOAuth2Application :: Int -> Gen OAuth2Application
genOAuth2Application n =
  OAuth2Application
    <$> arbitraryReducedMaybe n -- oAuth2ApplicationClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ApplicationClientSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ApplicationConfidentialClient :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ApplicationCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ApplicationId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oAuth2ApplicationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ApplicationRedirectUris :: Maybe [Text]
  
instance Arbitrary Organization where
  arbitrary = sized genOrganization

genOrganization :: Int -> Gen Organization
genOrganization n =
  Organization
    <$> arbitraryReducedMaybe n -- organizationAvatarUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- organizationLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationRepoAdminChangeTeamAccess :: Maybe Bool
    <*> arbitraryReducedMaybe n -- organizationUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationVisibility :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationWebsite :: Maybe Text
  
instance Arbitrary OrganizationPermissions where
  arbitrary = sized genOrganizationPermissions

genOrganizationPermissions :: Int -> Gen OrganizationPermissions
genOrganizationPermissions n =
  OrganizationPermissions
    <$> arbitraryReducedMaybe n -- organizationPermissionsCanCreateRepository :: Maybe Bool
    <*> arbitraryReducedMaybe n -- organizationPermissionsCanRead :: Maybe Bool
    <*> arbitraryReducedMaybe n -- organizationPermissionsCanWrite :: Maybe Bool
    <*> arbitraryReducedMaybe n -- organizationPermissionsIsAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- organizationPermissionsIsOwner :: Maybe Bool
  
instance Arbitrary PRBranchInfo where
  arbitrary = sized genPRBranchInfo

genPRBranchInfo :: Int -> Gen PRBranchInfo
genPRBranchInfo n =
  PRBranchInfo
    <$> arbitraryReducedMaybe n -- pRBranchInfoLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- pRBranchInfoRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- pRBranchInfoRepo :: Maybe Repository
    <*> arbitraryReducedMaybe n -- pRBranchInfoRepoId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pRBranchInfoSha :: Maybe Text
  
instance Arbitrary Package where
  arbitrary = sized genPackage

genPackage :: Int -> Gen Package
genPackage n =
  Package
    <$> arbitraryReducedMaybe n -- packageCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- packageCreator :: Maybe User
    <*> arbitraryReducedMaybe n -- packageHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- packageName :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageOwner :: Maybe User
    <*> arbitraryReducedMaybe n -- packageRepository :: Maybe Repository
    <*> arbitraryReducedMaybe n -- packageType :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageVersion :: Maybe Text
  
instance Arbitrary PackageFile where
  arbitrary = sized genPackageFile

genPackageFile :: Int -> Gen PackageFile
genPackageFile n =
  PackageFile
    <$> arbitraryReducedMaybe n -- packageFileSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- packageFileId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- packageFileMd5 :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageFileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageFileSha1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageFileSha256 :: Maybe Text
    <*> arbitraryReducedMaybe n -- packageFileSha512 :: Maybe Text
  
instance Arbitrary PayloadCommit where
  arbitrary = sized genPayloadCommit

genPayloadCommit :: Int -> Gen PayloadCommit
genPayloadCommit n =
  PayloadCommit
    <$> arbitraryReducedMaybe n -- payloadCommitAdded :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- payloadCommitAuthor :: Maybe PayloadUser
    <*> arbitraryReducedMaybe n -- payloadCommitCommitter :: Maybe PayloadUser
    <*> arbitraryReducedMaybe n -- payloadCommitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadCommitMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadCommitModified :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- payloadCommitRemoved :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- payloadCommitTimestamp :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- payloadCommitUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadCommitVerification :: Maybe PayloadCommitVerification
  
instance Arbitrary PayloadCommitVerification where
  arbitrary = sized genPayloadCommitVerification

genPayloadCommitVerification :: Int -> Gen PayloadCommitVerification
genPayloadCommitVerification n =
  PayloadCommitVerification
    <$> arbitraryReducedMaybe n -- payloadCommitVerificationPayload :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadCommitVerificationReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadCommitVerificationSignature :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadCommitVerificationSigner :: Maybe PayloadUser
    <*> arbitraryReducedMaybe n -- payloadCommitVerificationVerified :: Maybe Bool
  
instance Arbitrary PayloadUser where
  arbitrary = sized genPayloadUser

genPayloadUser :: Int -> Gen PayloadUser
genPayloadUser n =
  PayloadUser
    <$> arbitraryReducedMaybe n -- payloadUserEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadUserName :: Maybe Text
    <*> arbitraryReducedMaybe n -- payloadUserUsername :: Maybe Text
  
instance Arbitrary Permission where
  arbitrary = sized genPermission

genPermission :: Int -> Gen Permission
genPermission n =
  Permission
    <$> arbitraryReducedMaybe n -- permissionAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- permissionPull :: Maybe Bool
    <*> arbitraryReducedMaybe n -- permissionPush :: Maybe Bool
  
instance Arbitrary PublicKey where
  arbitrary = sized genPublicKey

genPublicKey :: Int -> Gen PublicKey
genPublicKey n =
  PublicKey
    <$> arbitraryReducedMaybe n -- publicKeyCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- publicKeyFingerprint :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicKeyId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- publicKeyKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicKeyKeyType :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicKeyReadOnly :: Maybe Bool
    <*> arbitraryReducedMaybe n -- publicKeyTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicKeyUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- publicKeyUser :: Maybe User
  
instance Arbitrary PullRequest where
  arbitrary = sized genPullRequest

genPullRequest :: Int -> Gen PullRequest
genPullRequest n =
  PullRequest
    <$> arbitraryReducedMaybe n -- pullRequestAllowMaintainerEdit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullRequestAssignee :: Maybe User
    <*> arbitraryReducedMaybe n -- pullRequestAssignees :: Maybe [User]
    <*> arbitraryReducedMaybe n -- pullRequestBase :: Maybe PRBranchInfo
    <*> arbitraryReducedMaybe n -- pullRequestBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestClosedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullRequestComments :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullRequestCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullRequestDiffUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullRequestHead :: Maybe PRBranchInfo
    <*> arbitraryReducedMaybe n -- pullRequestHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullRequestIsLocked :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullRequestLabels :: Maybe [Label]
    <*> arbitraryReducedMaybe n -- pullRequestMergeBase :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestMergeCommitSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestMergeable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullRequestMerged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullRequestMergedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullRequestMergedBy :: Maybe User
    <*> arbitraryReducedMaybe n -- pullRequestMilestone :: Maybe Milestone
    <*> arbitraryReducedMaybe n -- pullRequestNumber :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullRequestPatchUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestPinOrder :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullRequestRequestedReviewers :: Maybe [User]
    <*> arbitraryReducedMaybe n -- pullRequestState :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullRequestUser :: Maybe User
  
instance Arbitrary PullRequestMeta where
  arbitrary = sized genPullRequestMeta

genPullRequestMeta :: Int -> Gen PullRequestMeta
genPullRequestMeta n =
  PullRequestMeta
    <$> arbitraryReducedMaybe n -- pullRequestMetaMerged :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullRequestMetaMergedAt :: Maybe DateTime
  
instance Arbitrary PullReview where
  arbitrary = sized genPullReview

genPullReview :: Int -> Gen PullReview
genPullReview n =
  PullReview
    <$> arbitraryReducedMaybe n -- pullReviewBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentsCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullReviewCommitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewDismissed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullReviewHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullReviewOfficial :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullReviewPullRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewStale :: Maybe Bool
    <*> arbitraryReducedMaybe n -- pullReviewState :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewSubmittedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullReviewTeam :: Maybe Team
    <*> arbitraryReducedMaybe n -- pullReviewUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullReviewUser :: Maybe User
  
instance Arbitrary PullReviewComment where
  arbitrary = sized genPullReviewComment

genPullReviewComment :: Int -> Gen PullReviewComment
genPullReviewComment n =
  PullReviewComment
    <$> arbitraryReducedMaybe n -- pullReviewCommentBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentCommitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullReviewCommentDiffHunk :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullReviewCommentOriginalCommitId :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentOriginalPosition :: Maybe Int
    <*> arbitraryReducedMaybe n -- pullReviewCommentPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentPosition :: Maybe Int
    <*> arbitraryReducedMaybe n -- pullReviewCommentPullRequestReviewId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- pullReviewCommentPullRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- pullReviewCommentResolver :: Maybe User
    <*> arbitraryReducedMaybe n -- pullReviewCommentUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pullReviewCommentUser :: Maybe User
  
instance Arbitrary PullReviewRequestOptions where
  arbitrary = sized genPullReviewRequestOptions

genPullReviewRequestOptions :: Int -> Gen PullReviewRequestOptions
genPullReviewRequestOptions n =
  PullReviewRequestOptions
    <$> arbitraryReducedMaybe n -- pullReviewRequestOptionsReviewers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- pullReviewRequestOptionsTeamReviewers :: Maybe [Text]
  
instance Arbitrary PushMirror where
  arbitrary = sized genPushMirror

genPushMirror :: Int -> Gen PushMirror
genPushMirror n =
  PushMirror
    <$> arbitraryReducedMaybe n -- pushMirrorCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushMirrorInterval :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushMirrorLastError :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushMirrorLastUpdate :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushMirrorRemoteAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushMirrorRemoteName :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushMirrorRepoName :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushMirrorSyncOnCommit :: Maybe Bool
  
instance Arbitrary Reaction where
  arbitrary = sized genReaction

genReaction :: Int -> Gen Reaction
genReaction n =
  Reaction
    <$> arbitraryReducedMaybe n -- reactionContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- reactionCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- reactionUser :: Maybe User
  
instance Arbitrary Reference where
  arbitrary = sized genReference

genReference :: Int -> Gen Reference
genReference n =
  Reference
    <$> arbitraryReducedMaybe n -- referenceObject :: Maybe GitObject
    <*> arbitraryReducedMaybe n -- referenceRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- referenceUrl :: Maybe Text
  
instance Arbitrary Release where
  arbitrary = sized genRelease

genRelease :: Int -> Gen Release
genRelease n =
  Release
    <$> arbitraryReducedMaybe n -- releaseAssets :: Maybe [Attachment]
    <*> arbitraryReducedMaybe n -- releaseAuthor :: Maybe User
    <*> arbitraryReducedMaybe n -- releaseBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- releaseDraft :: Maybe Bool
    <*> arbitraryReducedMaybe n -- releaseHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- releaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- releasePrerelease :: Maybe Bool
    <*> arbitraryReducedMaybe n -- releasePublishedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- releaseTagName :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseTarballUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseTargetCommitish :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseUploadUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseZipballUrl :: Maybe Text
  
instance Arbitrary RenameUserOption where
  arbitrary = sized genRenameUserOption

genRenameUserOption :: Int -> Gen RenameUserOption
genRenameUserOption n =
  RenameUserOption
    <$> arbitrary -- renameUserOptionNewUsername :: Text
  
instance Arbitrary RepoCollaboratorPermission where
  arbitrary = sized genRepoCollaboratorPermission

genRepoCollaboratorPermission :: Int -> Gen RepoCollaboratorPermission
genRepoCollaboratorPermission n =
  RepoCollaboratorPermission
    <$> arbitraryReducedMaybe n -- repoCollaboratorPermissionPermission :: Maybe Text
    <*> arbitraryReducedMaybe n -- repoCollaboratorPermissionRoleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- repoCollaboratorPermissionUser :: Maybe User
  
instance Arbitrary RepoCommit where
  arbitrary = sized genRepoCommit

genRepoCommit :: Int -> Gen RepoCommit
genRepoCommit n =
  RepoCommit
    <$> arbitraryReducedMaybe n -- repoCommitAuthor :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- repoCommitCommitter :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- repoCommitMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- repoCommitTree :: Maybe CommitMeta
    <*> arbitraryReducedMaybe n -- repoCommitUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repoCommitVerification :: Maybe PayloadCommitVerification
  
instance Arbitrary RepoTopicOptions where
  arbitrary = sized genRepoTopicOptions

genRepoTopicOptions :: Int -> Gen RepoTopicOptions
genRepoTopicOptions n =
  RepoTopicOptions
    <$> arbitraryReducedMaybe n -- repoTopicOptionsTopics :: Maybe [Text]
  
instance Arbitrary RepoTransfer where
  arbitrary = sized genRepoTransfer

genRepoTransfer :: Int -> Gen RepoTransfer
genRepoTransfer n =
  RepoTransfer
    <$> arbitraryReducedMaybe n -- repoTransferDoer :: Maybe User
    <*> arbitraryReducedMaybe n -- repoTransferRecipient :: Maybe User
    <*> arbitraryReducedMaybe n -- repoTransferTeams :: Maybe [Team]
  
instance Arbitrary Repository where
  arbitrary = sized genRepository

genRepository :: Int -> Gen Repository
genRepository n =
  Repository
    <$> arbitraryReducedMaybe n -- repositoryAllowMergeCommits :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAllowRebase :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAllowRebaseExplicit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAllowRebaseUpdate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAllowSquashMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryArchivedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- repositoryAvatarUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryCloneUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- repositoryDefaultAllowMaintainerEdit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryDefaultBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryDefaultDeleteBranchAfterMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryDefaultMergeStyle :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryEmpty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryExternalTracker :: Maybe ExternalTracker
    <*> arbitraryReducedMaybe n -- repositoryExternalWiki :: Maybe ExternalWiki
    <*> arbitraryReducedMaybe n -- repositoryFork :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryForksCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryHasActions :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasIssues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasPackages :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasProjects :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasPullRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasReleases :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasWiki :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryIgnoreWhitespaceConflicts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryInternal :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryInternalTracker :: Maybe InternalTracker
    <*> arbitraryReducedMaybe n -- repositoryLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryLanguagesUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryMirror :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryMirrorInterval :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryMirrorUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- repositoryName :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryOpenIssuesCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryOpenPrCounter :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryOriginalUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryOwner :: Maybe User
    <*> arbitraryReducedMaybe n -- repositoryParent :: Maybe Repository
    <*> arbitraryReducedMaybe n -- repositoryPermissions :: Maybe Permission
    <*> arbitraryReducedMaybe n -- repositoryPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryReleaseCounter :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryRepoTransfer :: Maybe RepoTransfer
    <*> arbitraryReducedMaybe n -- repositorySize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositorySshUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryStarsCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryTemplate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- repositoryUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryWatchersCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryWebsite :: Maybe Text
  
instance Arbitrary RepositoryMeta where
  arbitrary = sized genRepositoryMeta

genRepositoryMeta :: Int -> Gen RepositoryMeta
genRepositoryMeta n =
  RepositoryMeta
    <$> arbitraryReducedMaybe n -- repositoryMetaFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryMetaId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryMetaName :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryMetaOwner :: Maybe Text
  
instance Arbitrary SearchResults where
  arbitrary = sized genSearchResults

genSearchResults :: Int -> Gen SearchResults
genSearchResults n =
  SearchResults
    <$> arbitraryReducedMaybe n -- searchResultsData :: Maybe [Repository]
    <*> arbitraryReducedMaybe n -- searchResultsOk :: Maybe Bool
  
instance Arbitrary Secret where
  arbitrary = sized genSecret

genSecret :: Int -> Gen Secret
genSecret n =
  Secret
    <$> arbitraryReducedMaybe n -- secretCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- secretName :: Maybe Text
  
instance Arbitrary ServerVersion where
  arbitrary = sized genServerVersion

genServerVersion :: Int -> Gen ServerVersion
genServerVersion n =
  ServerVersion
    <$> arbitraryReducedMaybe n -- serverVersionVersion :: Maybe Text
  
instance Arbitrary StopWatch where
  arbitrary = sized genStopWatch

genStopWatch :: Int -> Gen StopWatch
genStopWatch n =
  StopWatch
    <$> arbitraryReducedMaybe n -- stopWatchCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- stopWatchDuration :: Maybe Text
    <*> arbitraryReducedMaybe n -- stopWatchIssueIndex :: Maybe Integer
    <*> arbitraryReducedMaybe n -- stopWatchIssueTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- stopWatchRepoName :: Maybe Text
    <*> arbitraryReducedMaybe n -- stopWatchRepoOwnerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- stopWatchSeconds :: Maybe Integer
  
instance Arbitrary SubmitPullReviewOptions where
  arbitrary = sized genSubmitPullReviewOptions

genSubmitPullReviewOptions :: Int -> Gen SubmitPullReviewOptions
genSubmitPullReviewOptions n =
  SubmitPullReviewOptions
    <$> arbitraryReducedMaybe n -- submitPullReviewOptionsBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- submitPullReviewOptionsEvent :: Maybe Text
  
instance Arbitrary Tag where
  arbitrary = sized genTag

genTag :: Int -> Gen Tag
genTag n =
  Tag
    <$> arbitraryReducedMaybe n -- tagCommit :: Maybe CommitMeta
    <*> arbitraryReducedMaybe n -- tagId :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagTarballUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagZipballUrl :: Maybe Text
  
instance Arbitrary Team where
  arbitrary = sized genTeam

genTeam :: Int -> Gen Team
genTeam n =
  Team
    <$> arbitraryReducedMaybe n -- teamCanCreateOrgRepo :: Maybe Bool
    <*> arbitraryReducedMaybe n -- teamDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- teamIncludesAllRepositories :: Maybe Bool
    <*> arbitraryReducedMaybe n -- teamName :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamOrganization :: Maybe Organization
    <*> arbitraryReducedMaybe n -- teamPermission :: Maybe E'Permission2
    <*> arbitraryReducedMaybe n -- teamUnits :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- teamUnitsMap :: Maybe (Map.Map String Text)
  
instance Arbitrary TeamSearch200Response where
  arbitrary = sized genTeamSearch200Response

genTeamSearch200Response :: Int -> Gen TeamSearch200Response
genTeamSearch200Response n =
  TeamSearch200Response
    <$> arbitraryReducedMaybe n -- teamSearch200ResponseData :: Maybe [Team]
    <*> arbitraryReducedMaybe n -- teamSearch200ResponseOk :: Maybe Bool
  
instance Arbitrary TimelineComment where
  arbitrary = sized genTimelineComment

genTimelineComment :: Int -> Gen TimelineComment
genTimelineComment n =
  TimelineComment
    <$> arbitraryReducedMaybe n -- timelineCommentAssignee :: Maybe User
    <*> arbitraryReducedMaybe n -- timelineCommentAssigneeTeam :: Maybe Team
    <*> arbitraryReducedMaybe n -- timelineCommentBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- timelineCommentDependentIssue :: Maybe Issue
    <*> arbitraryReducedMaybe n -- timelineCommentHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- timelineCommentIssueUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentLabel :: Maybe Label
    <*> arbitraryReducedMaybe n -- timelineCommentMilestone :: Maybe Milestone
    <*> arbitraryReducedMaybe n -- timelineCommentNewRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentNewTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentOldMilestone :: Maybe Milestone
    <*> arbitraryReducedMaybe n -- timelineCommentOldProjectId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- timelineCommentOldRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentOldTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentProjectId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- timelineCommentPullRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentRefAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentRefComment :: Maybe Comment
    <*> arbitraryReducedMaybe n -- timelineCommentRefCommitSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentRefIssue :: Maybe Issue
    <*> arbitraryReducedMaybe n -- timelineCommentRemovedAssignee :: Maybe Bool
    <*> arbitraryReducedMaybe n -- timelineCommentResolveDoer :: Maybe User
    <*> arbitraryReducedMaybe n -- timelineCommentReviewId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- timelineCommentTrackedTime :: Maybe TrackedTime
    <*> arbitraryReducedMaybe n -- timelineCommentType :: Maybe Text
    <*> arbitraryReducedMaybe n -- timelineCommentUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- timelineCommentUser :: Maybe User
  
instance Arbitrary TopicName where
  arbitrary = sized genTopicName

genTopicName :: Int -> Gen TopicName
genTopicName n =
  TopicName
    <$> arbitraryReducedMaybe n -- topicNameTopics :: Maybe [Text]
  
instance Arbitrary TopicResponse where
  arbitrary = sized genTopicResponse

genTopicResponse :: Int -> Gen TopicResponse
genTopicResponse n =
  TopicResponse
    <$> arbitraryReducedMaybe n -- topicResponseCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- topicResponseId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- topicResponseRepoCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- topicResponseTopicName :: Maybe Text
    <*> arbitraryReducedMaybe n -- topicResponseUpdated :: Maybe DateTime
  
instance Arbitrary TrackedTime where
  arbitrary = sized genTrackedTime

genTrackedTime :: Int -> Gen TrackedTime
genTrackedTime n =
  TrackedTime
    <$> arbitraryReducedMaybe n -- trackedTimeCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trackedTimeId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- trackedTimeIssue :: Maybe Issue
    <*> arbitraryReducedMaybe n -- trackedTimeIssueId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- trackedTimeTime :: Maybe Integer
    <*> arbitraryReducedMaybe n -- trackedTimeUserId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- trackedTimeUserName :: Maybe Text
  
instance Arbitrary TransferRepoOption where
  arbitrary = sized genTransferRepoOption

genTransferRepoOption :: Int -> Gen TransferRepoOption
genTransferRepoOption n =
  TransferRepoOption
    <$> arbitrary -- transferRepoOptionNewOwner :: Text
    <*> arbitraryReducedMaybe n -- transferRepoOptionTeamIds :: Maybe [Integer]
  
instance Arbitrary UpdateFileOptions where
  arbitrary = sized genUpdateFileOptions

genUpdateFileOptions :: Int -> Gen UpdateFileOptions
genUpdateFileOptions n =
  UpdateFileOptions
    <$> arbitraryReducedMaybe n -- updateFileOptionsAuthor :: Maybe Identity
    <*> arbitraryReducedMaybe n -- updateFileOptionsBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsCommitter :: Maybe Identity
    <*> arbitrary -- updateFileOptionsContent :: Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsDates :: Maybe CommitDateOptions
    <*> arbitraryReducedMaybe n -- updateFileOptionsFromPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsNewBranch :: Maybe Text
    <*> arbitrary -- updateFileOptionsSha :: Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsSignoff :: Maybe Bool
  
instance Arbitrary UpdateRepoAvatarOption where
  arbitrary = sized genUpdateRepoAvatarOption

genUpdateRepoAvatarOption :: Int -> Gen UpdateRepoAvatarOption
genUpdateRepoAvatarOption n =
  UpdateRepoAvatarOption
    <$> arbitraryReducedMaybe n -- updateRepoAvatarOptionImage :: Maybe Text
  
instance Arbitrary UpdateUserAvatarOption where
  arbitrary = sized genUpdateUserAvatarOption

genUpdateUserAvatarOption :: Int -> Gen UpdateUserAvatarOption
genUpdateUserAvatarOption n =
  UpdateUserAvatarOption
    <$> arbitraryReducedMaybe n -- updateUserAvatarOptionImage :: Maybe Text
  
instance Arbitrary User where
  arbitrary = sized genUser

genUser :: Int -> Gen User
genUser n =
  User
    <$> arbitraryReducedMaybe n -- userActive :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userAvatarUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- userEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userFollowersCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userFollowingCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userIsAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLastLogin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLogin :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLoginName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProhibitLogin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userRestricted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userStarredReposCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userVisibility :: Maybe Text
    <*> arbitraryReducedMaybe n -- userWebsite :: Maybe Text
  
instance Arbitrary UserHeatmapData where
  arbitrary = sized genUserHeatmapData

genUserHeatmapData :: Int -> Gen UserHeatmapData
genUserHeatmapData n =
  UserHeatmapData
    <$> arbitraryReducedMaybe n -- userHeatmapDataContributions :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userHeatmapDataTimestamp :: Maybe Integer
  
instance Arbitrary UserSearch200Response where
  arbitrary = sized genUserSearch200Response

genUserSearch200Response :: Int -> Gen UserSearch200Response
genUserSearch200Response n =
  UserSearch200Response
    <$> arbitraryReducedMaybe n -- userSearch200ResponseData :: Maybe [User]
    <*> arbitraryReducedMaybe n -- userSearch200ResponseOk :: Maybe Bool
  
instance Arbitrary UserSettings where
  arbitrary = sized genUserSettings

genUserSettings :: Int -> Gen UserSettings
genUserSettings n =
  UserSettings
    <$> arbitraryReducedMaybe n -- userSettingsDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsDiffViewStyle :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsHideActivity :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSettingsHideEmail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSettingsLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsTheme :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsWebsite :: Maybe Text
  
instance Arbitrary UserSettingsOptions where
  arbitrary = sized genUserSettingsOptions

genUserSettingsOptions :: Int -> Gen UserSettingsOptions
genUserSettingsOptions n =
  UserSettingsOptions
    <$> arbitraryReducedMaybe n -- userSettingsOptionsDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsOptionsDiffViewStyle :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsOptionsFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsOptionsHideActivity :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSettingsOptionsHideEmail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSettingsOptionsLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsOptionsLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsOptionsTheme :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSettingsOptionsWebsite :: Maybe Text
  
instance Arbitrary WatchInfo where
  arbitrary = sized genWatchInfo

genWatchInfo :: Int -> Gen WatchInfo
genWatchInfo n =
  WatchInfo
    <$> arbitraryReducedMaybe n -- watchInfoCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- watchInfoIgnored :: Maybe Bool
    <*> arbitraryReducedMaybeValue n -- watchInfoReason :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- watchInfoRepositoryUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- watchInfoSubscribed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- watchInfoUrl :: Maybe Text
  
instance Arbitrary WikiCommit where
  arbitrary = sized genWikiCommit

genWikiCommit :: Int -> Gen WikiCommit
genWikiCommit n =
  WikiCommit
    <$> arbitraryReducedMaybe n -- wikiCommitAuthor :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- wikiCommitCommiter :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- wikiCommitMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiCommitSha :: Maybe Text
  
instance Arbitrary WikiCommitList where
  arbitrary = sized genWikiCommitList

genWikiCommitList :: Int -> Gen WikiCommitList
genWikiCommitList n =
  WikiCommitList
    <$> arbitraryReducedMaybe n -- wikiCommitListCommits :: Maybe [WikiCommit]
    <*> arbitraryReducedMaybe n -- wikiCommitListCount :: Maybe Integer
  
instance Arbitrary WikiPage where
  arbitrary = sized genWikiPage

genWikiPage :: Int -> Gen WikiPage
genWikiPage n =
  WikiPage
    <$> arbitraryReducedMaybe n -- wikiPageCommitCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- wikiPageContentBase64 :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiPageFooter :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiPageHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiPageLastCommit :: Maybe WikiCommit
    <*> arbitraryReducedMaybe n -- wikiPageSidebar :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiPageSubUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiPageTitle :: Maybe Text
  
instance Arbitrary WikiPageMetaData where
  arbitrary = sized genWikiPageMetaData

genWikiPageMetaData :: Int -> Gen WikiPageMetaData
genWikiPageMetaData n =
  WikiPageMetaData
    <$> arbitraryReducedMaybe n -- wikiPageMetaDataHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiPageMetaDataLastCommit :: Maybe WikiCommit
    <*> arbitraryReducedMaybe n -- wikiPageMetaDataSubUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- wikiPageMetaDataTitle :: Maybe Text
  



instance Arbitrary E'DiffType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Do where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Operation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Permission where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Permission2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Service where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Sort where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Sort2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Style where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SubjectType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'TrustModel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Visibility where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Whitespace where
  arbitrary = arbitraryBoundedEnum

