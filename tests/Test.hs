{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Gitea.Model
import Gitea.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy APIError)
      propMimeEq MimeJSON (Proxy :: Proxy AccessToken)
      propMimeEq MimeJSON (Proxy :: Proxy ActionTask)
      propMimeEq MimeJSON (Proxy :: Proxy ActionTaskResponse)
      propMimeEq MimeJSON (Proxy :: Proxy ActionVariable)
      propMimeEq MimeJSON (Proxy :: Proxy Activity)
      propMimeEq MimeJSON (Proxy :: Proxy ActivityPub)
      propMimeEq MimeJSON (Proxy :: Proxy AddCollaboratorOption)
      propMimeEq MimeJSON (Proxy :: Proxy AddTimeOption)
      propMimeEq MimeJSON (Proxy :: Proxy AnnotatedTag)
      propMimeEq MimeJSON (Proxy :: Proxy AnnotatedTagObject)
      propMimeEq MimeJSON (Proxy :: Proxy Attachment)
      propMimeEq MimeJSON (Proxy :: Proxy Badge)
      propMimeEq MimeJSON (Proxy :: Proxy Branch)
      propMimeEq MimeJSON (Proxy :: Proxy BranchProtection)
      propMimeEq MimeJSON (Proxy :: Proxy ChangeFileOperation)
      propMimeEq MimeJSON (Proxy :: Proxy ChangeFilesOptions)
      propMimeEq MimeJSON (Proxy :: Proxy ChangedFile)
      propMimeEq MimeJSON (Proxy :: Proxy CombinedStatus)
      propMimeEq MimeJSON (Proxy :: Proxy Comment)
      propMimeEq MimeJSON (Proxy :: Proxy Commit)
      propMimeEq MimeJSON (Proxy :: Proxy CommitAffectedFiles)
      propMimeEq MimeJSON (Proxy :: Proxy CommitDateOptions)
      propMimeEq MimeJSON (Proxy :: Proxy CommitMeta)
      propMimeEq MimeJSON (Proxy :: Proxy CommitStats)
      propMimeEq MimeJSON (Proxy :: Proxy CommitStatus)
      propMimeEq MimeJSON (Proxy :: Proxy CommitUser)
      propMimeEq MimeJSON (Proxy :: Proxy Compare)
      propMimeEq MimeJSON (Proxy :: Proxy ContentsResponse)
      propMimeEq MimeJSON (Proxy :: Proxy CreateAccessTokenOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateBranchProtectionOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateBranchRepoOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateEmailOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateFileOptions)
      propMimeEq MimeJSON (Proxy :: Proxy CreateForkOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateGPGKeyOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateHookOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateIssueCommentOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateIssueOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateKeyOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateLabelOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateMilestoneOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateOAuth2ApplicationOptions)
      propMimeEq MimeJSON (Proxy :: Proxy CreateOrUpdateSecretOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateOrgOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreatePullRequestOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreatePullReviewComment)
      propMimeEq MimeJSON (Proxy :: Proxy CreatePullReviewOptions)
      propMimeEq MimeJSON (Proxy :: Proxy CreatePushMirrorOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateReleaseOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateRepoOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateStatusOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateTagOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateTagProtectionOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateTeamOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateUserOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateVariableOption)
      propMimeEq MimeJSON (Proxy :: Proxy CreateWikiPageOptions)
      propMimeEq MimeJSON (Proxy :: Proxy Cron)
      propMimeEq MimeJSON (Proxy :: Proxy DeleteEmailOption)
      propMimeEq MimeJSON (Proxy :: Proxy DeleteFileOptions)
      propMimeEq MimeJSON (Proxy :: Proxy DeployKey)
      propMimeEq MimeJSON (Proxy :: Proxy DismissPullReviewOptions)
      propMimeEq MimeJSON (Proxy :: Proxy EditAttachmentOptions)
      propMimeEq MimeJSON (Proxy :: Proxy EditBranchProtectionOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditDeadlineOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditGitHookOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditHookOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditIssueCommentOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditIssueOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditLabelOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditMilestoneOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditOrgOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditPullRequestOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditReactionOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditReleaseOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditRepoOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditTagProtectionOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditTeamOption)
      propMimeEq MimeJSON (Proxy :: Proxy EditUserOption)
      propMimeEq MimeJSON (Proxy :: Proxy Email)
      propMimeEq MimeJSON (Proxy :: Proxy ExternalTracker)
      propMimeEq MimeJSON (Proxy :: Proxy ExternalWiki)
      propMimeEq MimeJSON (Proxy :: Proxy FileCommitResponse)
      propMimeEq MimeJSON (Proxy :: Proxy FileDeleteResponse)
      propMimeEq MimeJSON (Proxy :: Proxy FileLinksResponse)
      propMimeEq MimeJSON (Proxy :: Proxy FileResponse)
      propMimeEq MimeJSON (Proxy :: Proxy FilesResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GPGKey)
      propMimeEq MimeJSON (Proxy :: Proxy GPGKeyEmail)
      propMimeEq MimeJSON (Proxy :: Proxy GeneralAPISettings)
      propMimeEq MimeJSON (Proxy :: Proxy GeneralAttachmentSettings)
      propMimeEq MimeJSON (Proxy :: Proxy GeneralRepoSettings)
      propMimeEq MimeJSON (Proxy :: Proxy GeneralUISettings)
      propMimeEq MimeJSON (Proxy :: Proxy GenerateRepoOption)
      propMimeEq MimeJSON (Proxy :: Proxy GitBlobResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GitEntry)
      propMimeEq MimeJSON (Proxy :: Proxy GitHook)
      propMimeEq MimeJSON (Proxy :: Proxy GitObject)
      propMimeEq MimeJSON (Proxy :: Proxy GitTreeResponse)
      propMimeEq MimeJSON (Proxy :: Proxy GitignoreTemplateInfo)
      propMimeEq MimeJSON (Proxy :: Proxy Hook)
      propMimeEq MimeJSON (Proxy :: Proxy Identity)
      propMimeEq MimeJSON (Proxy :: Proxy InternalTracker)
      propMimeEq MimeJSON (Proxy :: Proxy Issue)
      propMimeEq MimeJSON (Proxy :: Proxy IssueConfig)
      propMimeEq MimeJSON (Proxy :: Proxy IssueConfigContactLink)
      propMimeEq MimeJSON (Proxy :: Proxy IssueConfigValidation)
      propMimeEq MimeJSON (Proxy :: Proxy IssueDeadline)
      propMimeEq MimeJSON (Proxy :: Proxy IssueFormField)
      propMimeEq MimeJSON (Proxy :: Proxy IssueLabelsOption)
      propMimeEq MimeJSON (Proxy :: Proxy IssueMeta)
      propMimeEq MimeJSON (Proxy :: Proxy IssueTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy Label)
      propMimeEq MimeJSON (Proxy :: Proxy LabelTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy LicenseTemplateInfo)
      propMimeEq MimeJSON (Proxy :: Proxy LicensesTemplateListEntry)
      propMimeEq MimeJSON (Proxy :: Proxy MarkdownOption)
      propMimeEq MimeJSON (Proxy :: Proxy MarkupOption)
      propMimeEq MimeJSON (Proxy :: Proxy MergePullRequestOption)
      propMimeEq MimeJSON (Proxy :: Proxy MergeUpstreamRequest)
      propMimeEq MimeJSON (Proxy :: Proxy MergeUpstreamResponse)
      propMimeEq MimeJSON (Proxy :: Proxy MigrateRepoOptions)
      propMimeEq MimeJSON (Proxy :: Proxy Milestone)
      propMimeEq MimeJSON (Proxy :: Proxy NewIssuePinsAllowed)
      propMimeEq MimeJSON (Proxy :: Proxy NodeInfo)
      propMimeEq MimeJSON (Proxy :: Proxy NodeInfoServices)
      propMimeEq MimeJSON (Proxy :: Proxy NodeInfoSoftware)
      propMimeEq MimeJSON (Proxy :: Proxy NodeInfoUsage)
      propMimeEq MimeJSON (Proxy :: Proxy NodeInfoUsageUsers)
      propMimeEq MimeJSON (Proxy :: Proxy Note)
      propMimeEq MimeJSON (Proxy :: Proxy NotificationCount)
      propMimeEq MimeJSON (Proxy :: Proxy NotificationSubject)
      propMimeEq MimeJSON (Proxy :: Proxy NotificationThread)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2Application)
      propMimeEq MimeJSON (Proxy :: Proxy Organization)
      propMimeEq MimeJSON (Proxy :: Proxy OrganizationPermissions)
      propMimeEq MimeJSON (Proxy :: Proxy PRBranchInfo)
      propMimeEq MimeJSON (Proxy :: Proxy Package)
      propMimeEq MimeJSON (Proxy :: Proxy PackageFile)
      propMimeEq MimeJSON (Proxy :: Proxy PayloadCommit)
      propMimeEq MimeJSON (Proxy :: Proxy PayloadCommitVerification)
      propMimeEq MimeJSON (Proxy :: Proxy PayloadUser)
      propMimeEq MimeJSON (Proxy :: Proxy Permission)
      propMimeEq MimeJSON (Proxy :: Proxy PublicKey)
      propMimeEq MimeJSON (Proxy :: Proxy PullRequest)
      propMimeEq MimeJSON (Proxy :: Proxy PullRequestMeta)
      propMimeEq MimeJSON (Proxy :: Proxy PullReview)
      propMimeEq MimeJSON (Proxy :: Proxy PullReviewComment)
      propMimeEq MimeJSON (Proxy :: Proxy PullReviewRequestOptions)
      propMimeEq MimeJSON (Proxy :: Proxy PushMirror)
      propMimeEq MimeJSON (Proxy :: Proxy Reaction)
      propMimeEq MimeJSON (Proxy :: Proxy Reference)
      propMimeEq MimeJSON (Proxy :: Proxy Release)
      propMimeEq MimeJSON (Proxy :: Proxy RenameUserOption)
      propMimeEq MimeJSON (Proxy :: Proxy RepoCollaboratorPermission)
      propMimeEq MimeJSON (Proxy :: Proxy RepoCommit)
      propMimeEq MimeJSON (Proxy :: Proxy RepoCreateReleaseAttachmentRequest)
      propMimeEq MimeJSON (Proxy :: Proxy RepoTopicOptions)
      propMimeEq MimeJSON (Proxy :: Proxy RepoTransfer)
      propMimeEq MimeJSON (Proxy :: Proxy Repository)
      propMimeEq MimeJSON (Proxy :: Proxy RepositoryMeta)
      propMimeEq MimeJSON (Proxy :: Proxy SearchResults)
      propMimeEq MimeJSON (Proxy :: Proxy Secret)
      propMimeEq MimeJSON (Proxy :: Proxy ServerVersion)
      propMimeEq MimeJSON (Proxy :: Proxy StopWatch)
      propMimeEq MimeJSON (Proxy :: Proxy SubmitPullReviewOptions)
      propMimeEq MimeJSON (Proxy :: Proxy Tag)
      propMimeEq MimeJSON (Proxy :: Proxy TagProtection)
      propMimeEq MimeJSON (Proxy :: Proxy Team)
      propMimeEq MimeJSON (Proxy :: Proxy TeamSearch200Response)
      propMimeEq MimeJSON (Proxy :: Proxy TimelineComment)
      propMimeEq MimeJSON (Proxy :: Proxy TopicName)
      propMimeEq MimeJSON (Proxy :: Proxy TopicResponse)
      propMimeEq MimeJSON (Proxy :: Proxy TrackedTime)
      propMimeEq MimeJSON (Proxy :: Proxy TransferRepoOption)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateBranchProtectionPriories)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateBranchRepoOption)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateFileOptions)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateRepoAvatarOption)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateUserAvatarOption)
      propMimeEq MimeJSON (Proxy :: Proxy UpdateVariableOption)
      propMimeEq MimeJSON (Proxy :: Proxy User)
      propMimeEq MimeJSON (Proxy :: Proxy UserBadgeOption)
      propMimeEq MimeJSON (Proxy :: Proxy UserHeatmapData)
      propMimeEq MimeJSON (Proxy :: Proxy UserSearch200Response)
      propMimeEq MimeJSON (Proxy :: Proxy UserSettings)
      propMimeEq MimeJSON (Proxy :: Proxy UserSettingsOptions)
      propMimeEq MimeJSON (Proxy :: Proxy WatchInfo)
      propMimeEq MimeJSON (Proxy :: Proxy WikiCommit)
      propMimeEq MimeJSON (Proxy :: Proxy WikiCommitList)
      propMimeEq MimeJSON (Proxy :: Proxy WikiPage)
      propMimeEq MimeJSON (Proxy :: Proxy WikiPageMetaData)
      
