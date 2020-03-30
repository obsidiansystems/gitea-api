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
 
instance Arbitrary AccessToken where
  arbitrary = sized genAccessToken

genAccessToken :: Int -> Gen AccessToken
genAccessToken n =
  AccessToken
    <$> arbitraryReducedMaybe n -- accessTokenId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- accessTokenName :: Maybe Text
    <*> arbitraryReducedMaybe n -- accessTokenSha1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- accessTokenTokenLastEight :: Maybe Text
  
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
    <$> arbitrary -- addTimeOptionTime :: Integer
  
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
    <*> arbitraryReducedMaybe n -- branchName :: Maybe Text
  
instance Arbitrary Comment where
  arbitrary = sized genComment

genComment :: Int -> Gen Comment
genComment n =
  Comment
    <$> arbitraryReducedMaybe n -- commentBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- commentCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- commentHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- commentId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- commentIssueUrl :: Maybe Text
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
    <*> arbitraryReducedMaybe n -- commitHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitParents :: Maybe [CommitMeta]
    <*> arbitraryReducedMaybe n -- commitSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitUrl :: Maybe Text
  
instance Arbitrary CommitMeta where
  arbitrary = sized genCommitMeta

genCommitMeta :: Int -> Gen CommitMeta
genCommitMeta n =
  CommitMeta
    <$> arbitraryReducedMaybe n -- commitMetaSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- commitMetaUrl :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- contentsResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponsePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseSha :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- contentsResponseSubmoduleGitUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseTarget :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- contentsResponseUrl :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- createFileOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- createFileOptionsNewBranch :: Maybe Text
  
instance Arbitrary CreateForkOption where
  arbitrary = sized genCreateForkOption

genCreateForkOption :: Int -> Gen CreateForkOption
genCreateForkOption n =
  CreateForkOption
    <$> arbitraryReducedMaybe n -- createForkOptionOrganization :: Maybe Text
  
instance Arbitrary CreateGPGKeyOption where
  arbitrary = sized genCreateGPGKeyOption

genCreateGPGKeyOption :: Int -> Gen CreateGPGKeyOption
genCreateGPGKeyOption n =
  CreateGPGKeyOption
    <$> arbitrary -- createGPGKeyOptionArmoredPublicKey :: Text
  
instance Arbitrary CreateHookOption where
  arbitrary = sized genCreateHookOption

genCreateHookOption :: Int -> Gen CreateHookOption
genCreateHookOption n =
  CreateHookOption
    <$> arbitraryReducedMaybe n -- createHookOptionActive :: Maybe Bool
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
    <*> arbitrary -- createLabelOptionName :: Text
  
instance Arbitrary CreateMilestoneOption where
  arbitrary = sized genCreateMilestoneOption

genCreateMilestoneOption :: Int -> Gen CreateMilestoneOption
genCreateMilestoneOption n =
  CreateMilestoneOption
    <$> arbitraryReducedMaybe n -- createMilestoneOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createMilestoneOptionDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- createMilestoneOptionTitle :: Maybe Text
  
instance Arbitrary CreateOrgOption where
  arbitrary = sized genCreateOrgOption

genCreateOrgOption :: Int -> Gen CreateOrgOption
genCreateOrgOption n =
  CreateOrgOption
    <$> arbitraryReducedMaybe n -- createOrgOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createOrgOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createOrgOptionLocation :: Maybe Text
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
    <*> arbitraryReducedMaybe n -- createRepoOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createRepoOptionGitignores :: Maybe Text
    <*> arbitraryReducedMaybe n -- createRepoOptionLicense :: Maybe Text
    <*> arbitrary -- createRepoOptionName :: Text
    <*> arbitraryReducedMaybe n -- createRepoOptionPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createRepoOptionReadme :: Maybe Text
  
instance Arbitrary CreateStatusOption where
  arbitrary = sized genCreateStatusOption

genCreateStatusOption :: Int -> Gen CreateStatusOption
genCreateStatusOption n =
  CreateStatusOption
    <$> arbitraryReducedMaybe n -- createStatusOptionContext :: Maybe Text
    <*> arbitraryReducedMaybe n -- createStatusOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- createStatusOptionState :: Maybe Text
    <*> arbitraryReducedMaybe n -- createStatusOptionTargetUrl :: Maybe Text
  
instance Arbitrary CreateTeamOption where
  arbitrary = sized genCreateTeamOption

genCreateTeamOption :: Int -> Gen CreateTeamOption
genCreateTeamOption n =
  CreateTeamOption
    <$> arbitraryReducedMaybe n -- createTeamOptionDescription :: Maybe Text
    <*> arbitrary -- createTeamOptionName :: Text
    <*> arbitraryReducedMaybe n -- createTeamOptionPermission :: Maybe E'Permission
    <*> arbitraryReducedMaybe n -- createTeamOptionUnits :: Maybe [Text]
  
instance Arbitrary CreateUserOption where
  arbitrary = sized genCreateUserOption

genCreateUserOption :: Int -> Gen CreateUserOption
genCreateUserOption n =
  CreateUserOption
    <$> arbitrary -- createUserOptionEmail :: Text
    <*> arbitraryReducedMaybe n -- createUserOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createUserOptionLoginName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createUserOptionMustChangePassword :: Maybe Bool
    <*> arbitrary -- createUserOptionPassword :: Text
    <*> arbitraryReducedMaybe n -- createUserOptionSendNotify :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createUserOptionSourceId :: Maybe Integer
    <*> arbitrary -- createUserOptionUsername :: Text
  
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
    <*> arbitraryReducedMaybe n -- deleteFileOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- deleteFileOptionsNewBranch :: Maybe Text
    <*> arbitrary -- deleteFileOptionsSha :: Text
  
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
  
instance Arbitrary EditAttachmentOptions where
  arbitrary = sized genEditAttachmentOptions

genEditAttachmentOptions :: Int -> Gen EditAttachmentOptions
genEditAttachmentOptions n =
  EditAttachmentOptions
    <$> arbitraryReducedMaybe n -- editAttachmentOptionsName :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- editIssueOptionState :: Maybe Text
    <*> arbitraryReducedMaybe n -- editIssueOptionTitle :: Maybe Text
  
instance Arbitrary EditLabelOption where
  arbitrary = sized genEditLabelOption

genEditLabelOption :: Int -> Gen EditLabelOption
genEditLabelOption n =
  EditLabelOption
    <$> arbitraryReducedMaybe n -- editLabelOptionColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- editLabelOptionDescription :: Maybe Text
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
    <*> arbitraryReducedMaybe n -- editOrgOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editOrgOptionLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- editOrgOptionVisibility :: Maybe E'Visibility
    <*> arbitraryReducedMaybe n -- editOrgOptionWebsite :: Maybe Text
  
instance Arbitrary EditPullRequestOption where
  arbitrary = sized genEditPullRequestOption

genEditPullRequestOption :: Int -> Gen EditPullRequestOption
genEditPullRequestOption n =
  EditPullRequestOption
    <$> arbitraryReducedMaybe n -- editPullRequestOptionAssignee :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionAssignees :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- editPullRequestOptionBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- editPullRequestOptionLabels :: Maybe [Integer]
    <*> arbitraryReducedMaybe n -- editPullRequestOptionMilestone :: Maybe Integer
    <*> arbitraryReducedMaybe n -- editPullRequestOptionState :: Maybe Text
    <*> arbitraryReducedMaybe n -- editPullRequestOptionTitle :: Maybe Text
  
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
    <$> arbitraryReducedMaybe n -- editRepoOptionAllowMergeCommits :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowRebase :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowRebaseExplicit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionAllowSquashMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionDefaultBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionHasIssues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasPullRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionHasWiki :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionIgnoreWhitespaceConflicts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editRepoOptionPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editRepoOptionWebsite :: Maybe Text
  
instance Arbitrary EditTeamOption where
  arbitrary = sized genEditTeamOption

genEditTeamOption :: Int -> Gen EditTeamOption
genEditTeamOption n =
  EditTeamOption
    <$> arbitraryReducedMaybe n -- editTeamOptionDescription :: Maybe Text
    <*> arbitrary -- editTeamOptionName :: Text
    <*> arbitraryReducedMaybe n -- editTeamOptionPermission :: Maybe E'Permission
    <*> arbitraryReducedMaybe n -- editTeamOptionUnits :: Maybe [Text]
  
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
    <*> arbitrary -- editUserOptionEmail :: Text
    <*> arbitraryReducedMaybe n -- editUserOptionFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionLoginName :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionMaxRepoCreation :: Maybe Integer
    <*> arbitraryReducedMaybe n -- editUserOptionMustChangePassword :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- editUserOptionProhibitLogin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- editUserOptionSourceId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- editUserOptionWebsite :: Maybe Text
  
instance Arbitrary Email where
  arbitrary = sized genEmail

genEmail :: Int -> Gen Email
genEmail n =
  Email
    <$> arbitraryReducedMaybe n -- emailEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailPrimary :: Maybe Bool
    <*> arbitraryReducedMaybe n -- emailVerified :: Maybe Bool
  
instance Arbitrary FileCommitResponse where
  arbitrary = sized genFileCommitResponse

genFileCommitResponse :: Int -> Gen FileCommitResponse
genFileCommitResponse n =
  FileCommitResponse
    <$> arbitraryReducedMaybe n -- fileCommitResponseAuthor :: Maybe CommitUser
    <*> arbitraryReducedMaybe n -- fileCommitResponseCommitter :: Maybe CommitUser
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
  
instance Arbitrary GPGKeyEmail where
  arbitrary = sized genGPGKeyEmail

genGPGKeyEmail :: Int -> Gen GPGKeyEmail
genGPGKeyEmail n =
  GPGKeyEmail
    <$> arbitraryReducedMaybe n -- gPGKeyEmailEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- gPGKeyEmailVerified :: Maybe Bool
  
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
  
instance Arbitrary Hook where
  arbitrary = sized genHook

genHook :: Int -> Gen Hook
genHook n =
  Hook
    <$> arbitraryReducedMaybe n -- hookActive :: Maybe Bool
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
  
instance Arbitrary InlineObject where
  arbitrary = sized genInlineObject

genInlineObject :: Int -> Gen InlineObject
genInlineObject n =
  InlineObject
    <$> arbitrary -- inlineObjectName :: Text
  
instance Arbitrary InlineResponse200 where
  arbitrary = sized genInlineResponse200

genInlineResponse200 :: Int -> Gen InlineResponse200
genInlineResponse200 n =
  InlineResponse200
    <$> arbitraryReducedMaybe n -- inlineResponse200Data :: Maybe [User]
    <*> arbitraryReducedMaybe n -- inlineResponse200Ok :: Maybe Bool
  
instance Arbitrary Issue where
  arbitrary = sized genIssue

genIssue :: Int -> Gen Issue
genIssue n =
  Issue
    <$> arbitraryReducedMaybe n -- issueAssignee :: Maybe User
    <*> arbitraryReducedMaybe n -- issueAssignees :: Maybe [User]
    <*> arbitraryReducedMaybe n -- issueBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueClosedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueComments :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issueCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueDueDate :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issueLabels :: Maybe [Label]
    <*> arbitraryReducedMaybe n -- issueMilestone :: Maybe Milestone
    <*> arbitraryReducedMaybe n -- issueNumber :: Maybe Integer
    <*> arbitraryReducedMaybe n -- issuePullRequest :: Maybe PullRequestMeta
    <*> arbitraryReducedMaybe n -- issueState :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- issueUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- issueUser :: Maybe User
  
instance Arbitrary IssueDeadline where
  arbitrary = sized genIssueDeadline

genIssueDeadline :: Int -> Gen IssueDeadline
genIssueDeadline n =
  IssueDeadline
    <$> arbitraryReducedMaybe n -- issueDeadlineDueDate :: Maybe DateTime
  
instance Arbitrary IssueLabelsOption where
  arbitrary = sized genIssueLabelsOption

genIssueLabelsOption :: Int -> Gen IssueLabelsOption
genIssueLabelsOption n =
  IssueLabelsOption
    <$> arbitraryReducedMaybe n -- issueLabelsOptionLabels :: Maybe [Integer]
  
instance Arbitrary Label where
  arbitrary = sized genLabel

genLabel :: Int -> Gen Label
genLabel n =
  Label
    <$> arbitraryReducedMaybe n -- labelColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- labelName :: Maybe Text
    <*> arbitraryReducedMaybe n -- labelUrl :: Maybe Text
  
instance Arbitrary MarkdownOption where
  arbitrary = sized genMarkdownOption

genMarkdownOption :: Int -> Gen MarkdownOption
genMarkdownOption n =
  MarkdownOption
    <$> arbitraryReducedMaybe n -- markdownOptionContext :: Maybe Text
    <*> arbitraryReducedMaybe n -- markdownOptionMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- markdownOptionText :: Maybe Text
    <*> arbitraryReducedMaybe n -- markdownOptionWiki :: Maybe Bool
  
instance Arbitrary MergePullRequestOption where
  arbitrary = sized genMergePullRequestOption

genMergePullRequestOption :: Int -> Gen MergePullRequestOption
genMergePullRequestOption n =
  MergePullRequestOption
    <$> arbitrary -- mergePullRequestOptionDo :: E'Do
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionMergeMessageField :: Maybe Text
    <*> arbitraryReducedMaybe n -- mergePullRequestOptionMergeTitleField :: Maybe Text
  
instance Arbitrary MigrateRepoForm where
  arbitrary = sized genMigrateRepoForm

genMigrateRepoForm :: Int -> Gen MigrateRepoForm
genMigrateRepoForm n =
  MigrateRepoForm
    <$> arbitraryReducedMaybe n -- migrateRepoFormAuthPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoFormAuthUsername :: Maybe Text
    <*> arbitrary -- migrateRepoFormCloneAddr :: Text
    <*> arbitraryReducedMaybe n -- migrateRepoFormDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrateRepoFormIssues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoFormLabels :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoFormMilestones :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoFormMirror :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoFormPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoFormPullRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- migrateRepoFormReleases :: Maybe Bool
    <*> arbitrary -- migrateRepoFormRepoName :: Text
    <*> arbitrary -- migrateRepoFormUid :: Integer
    <*> arbitraryReducedMaybe n -- migrateRepoFormWiki :: Maybe Bool
  
instance Arbitrary Milestone where
  arbitrary = sized genMilestone

genMilestone :: Int -> Gen Milestone
genMilestone n =
  Milestone
    <$> arbitraryReducedMaybe n -- milestoneClosedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- milestoneClosedIssues :: Maybe Integer
    <*> arbitraryReducedMaybe n -- milestoneDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- milestoneDueOn :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- milestoneId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- milestoneOpenIssues :: Maybe Integer
    <*> arbitraryReducedMaybe n -- milestoneState :: Maybe Text
    <*> arbitraryReducedMaybe n -- milestoneTitle :: Maybe Text
  
instance Arbitrary Organization where
  arbitrary = sized genOrganization

genOrganization :: Int -> Gen Organization
genOrganization n =
  Organization
    <$> arbitraryReducedMaybe n -- organizationAvatarUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- organizationLocation :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationVisibility :: Maybe Text
    <*> arbitraryReducedMaybe n -- organizationWebsite :: Maybe Text
  
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
    <$> arbitraryReducedMaybe n -- pullRequestAssignee :: Maybe User
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
    <*> arbitraryReducedMaybe n -- releaseId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- releaseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- releasePrerelease :: Maybe Bool
    <*> arbitraryReducedMaybe n -- releasePublishedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- releaseTagName :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseTarballUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseTargetCommitish :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- releaseZipballUrl :: Maybe Text
  
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
  
instance Arbitrary Repository where
  arbitrary = sized genRepository

genRepository :: Int -> Gen Repository
genRepository n =
  Repository
    <$> arbitraryReducedMaybe n -- repositoryAllowMergeCommits :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAllowRebase :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAllowRebaseExplicit :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAllowSquashMerge :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryAvatarUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryCloneUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- repositoryDefaultBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryEmpty :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryFork :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryForksCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryHasIssues :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasPullRequests :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHasWiki :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryHtmlUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryIgnoreWhitespaceConflicts :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryMirror :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositoryName :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryOpenIssuesCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryOwner :: Maybe User
    <*> arbitraryReducedMaybe n -- repositoryParent :: Maybe Repository
    <*> arbitraryReducedMaybe n -- repositoryPermissions :: Maybe Permission
    <*> arbitraryReducedMaybe n -- repositoryPrivate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- repositorySize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositorySshUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- repositoryStarsCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- repositoryWatchersCount :: Maybe Integer
    <*> arbitraryReducedMaybe n -- repositoryWebsite :: Maybe Text
  
instance Arbitrary SearchResults where
  arbitrary = sized genSearchResults

genSearchResults :: Int -> Gen SearchResults
genSearchResults n =
  SearchResults
    <$> arbitraryReducedMaybe n -- searchResultsData :: Maybe [Repository]
    <*> arbitraryReducedMaybe n -- searchResultsOk :: Maybe Bool
  
instance Arbitrary ServerVersion where
  arbitrary = sized genServerVersion

genServerVersion :: Int -> Gen ServerVersion
genServerVersion n =
  ServerVersion
    <$> arbitraryReducedMaybe n -- serverVersionVersion :: Maybe Text
  
instance Arbitrary Status where
  arbitrary = sized genStatus

genStatus :: Int -> Gen Status
genStatus n =
  Status
    <$> arbitraryReducedMaybe n -- statusContext :: Maybe Text
    <*> arbitraryReducedMaybe n -- statusCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- statusCreator :: Maybe User
    <*> arbitraryReducedMaybe n -- statusDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- statusId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- statusStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- statusTargetUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- statusUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- statusUrl :: Maybe Text
  
instance Arbitrary Tag where
  arbitrary = sized genTag

genTag :: Int -> Gen Tag
genTag n =
  Tag
    <$> arbitraryReducedMaybe n -- tagCommit :: Maybe CommitMeta
    <*> arbitraryReducedMaybe n -- tagId :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagTarballUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- tagZipballUrl :: Maybe Text
  
instance Arbitrary Team where
  arbitrary = sized genTeam

genTeam :: Int -> Gen Team
genTeam n =
  Team
    <$> arbitraryReducedMaybe n -- teamDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- teamName :: Maybe Text
    <*> arbitraryReducedMaybe n -- teamOrganization :: Maybe Organization
    <*> arbitraryReducedMaybe n -- teamPermission :: Maybe E'Permission2
    <*> arbitraryReducedMaybe n -- teamUnits :: Maybe [Text]
  
instance Arbitrary TrackedTime where
  arbitrary = sized genTrackedTime

genTrackedTime :: Int -> Gen TrackedTime
genTrackedTime n =
  TrackedTime
    <$> arbitraryReducedMaybe n -- trackedTimeCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trackedTimeId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- trackedTimeIssueId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- trackedTimeTime :: Maybe Integer
    <*> arbitraryReducedMaybe n -- trackedTimeUserId :: Maybe Integer
  
instance Arbitrary UpdateFileOptions where
  arbitrary = sized genUpdateFileOptions

genUpdateFileOptions :: Int -> Gen UpdateFileOptions
genUpdateFileOptions n =
  UpdateFileOptions
    <$> arbitraryReducedMaybe n -- updateFileOptionsAuthor :: Maybe Identity
    <*> arbitraryReducedMaybe n -- updateFileOptionsBranch :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsCommitter :: Maybe Identity
    <*> arbitrary -- updateFileOptionsContent :: Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsFromPath :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- updateFileOptionsNewBranch :: Maybe Text
    <*> arbitrary -- updateFileOptionsSha :: Text
  
instance Arbitrary User where
  arbitrary = sized genUser

genUser :: Int -> Gen User
genUser n =
  User
    <$> arbitraryReducedMaybe n -- userAvatarUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- userCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userFullName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userId :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userIsAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLastLogin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userLogin :: Maybe Text
  
instance Arbitrary UserHeatmapData where
  arbitrary = sized genUserHeatmapData

genUserHeatmapData :: Int -> Gen UserHeatmapData
genUserHeatmapData n =
  UserHeatmapData
    <$> arbitraryReducedMaybe n -- userHeatmapDataContributions :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userHeatmapDataTimestamp :: Maybe Integer
  
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
  



instance Arbitrary E'Do where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Permission where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Permission2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Sort where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Visibility where
  arbitrary = arbitraryBoundedEnum

