{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- From github package: https://hackage.haskell.org/package/github
-- Copyright 2011-2013 Mike Burns, 2013-2015 John Wiegley, 2016-2020 Oleg Grenrus
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Mike Burns nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
module GitHub where

import           Prelude.Compat

import           Control.DeepSeq (NFData (..))
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           Data.Vector     (Vector)

import qualified Data.Text       as T

import           Data.Aeson

-------------------------------------------------------------------------------
-- Tags
-------------------------------------------------------------------------------

data User

-------------------------------------------------------------------------------
-- Id
-------------------------------------------------------------------------------

newtype Id entity = Id Int
    deriving (Show)

untagId :: Id entity -> Int
untagId (Id name) = name

instance NFData (Id entity) where
    rnf (Id s) = rnf s

instance FromJSON (Id entity) where
    parseJSON = fmap Id . parseJSON

instance ToJSON (Id entity) where
    toJSON = toJSON . untagId

-------------------------------------------------------------------------------
-- Name
-------------------------------------------------------------------------------

newtype Name entity = N Text
    deriving (Show)

untagName :: Name entity -> Text
untagName (N name) = name

instance NFData (Name entity) where
    rnf (N s) = rnf s

instance FromJSON (Name entity) where
    parseJSON = fmap N . parseJSON

instance ToJSON (Name entity) where
    toJSON = toJSON . untagName

-------------------------------------------------------------------------------
-- SimpleUser
-------------------------------------------------------------------------------

data SimpleUser = SimpleUser
    { simpleUserId        :: !(Id User)
    , simpleUserLogin     :: !(Name User)
    , simpleUserAvatarUrl :: !URL
    , simpleUserUrl       :: !URL
    }
  deriving (Show)

instance NFData SimpleUser where
    rnf x = x `seq` ()

instance FromJSON SimpleUser where
    parseJSON = withObject "SimpleUser" $ \obj -> SimpleUser
        <$> obj .: "id"
        <*> obj .: "login"
        <*> obj .: "avatar_url"
        <*> obj .: "url"

-------------------------------------------------------------------------------
-- URL
-------------------------------------------------------------------------------

newtype URL = URL Text
    deriving (Show)

instance NFData URL where
    rnf (URL t) = rnf t

instance FromJSON URL where
    parseJSON = withText "URL" (pure . URL)

-------------------------------------------------------------------------------
-- IssueState
-------------------------------------------------------------------------------

-- | 'GitHub.Data.Issues.Issue' or 'GitHub.Data.PullRequests.PullRequest' state
data IssueState
    = StateOpen
    | StateClosed
  deriving (Show)

instance ToJSON IssueState where
    toJSON StateOpen   = String "open"
    toJSON StateClosed = String "closed"

instance FromJSON IssueState where
    parseJSON = withText "IssueState" $ \t -> case T.toLower t of
        "open"   -> pure StateOpen
        "closed" -> pure StateClosed
        _        -> fail $ "Unknown IssueState: " <> T.unpack t

instance NFData IssueState where rnf x = x `seq` ()

-------------------------------------------------------------------------------
-- IssueNumber
-------------------------------------------------------------------------------

newtype IssueNumber = IssueNumber Int
  deriving (Show)

unIssueNumber :: IssueNumber -> Int
unIssueNumber (IssueNumber i) = i

instance NFData IssueNumber where
    rnf (IssueNumber s) = rnf s

instance FromJSON IssueNumber where
    parseJSON = fmap IssueNumber . parseJSON

instance ToJSON IssueNumber where
    toJSON = toJSON . unIssueNumber

-------------------------------------------------------------------------------
-- IssueLabel
-------------------------------------------------------------------------------

data IssueLabel = IssueLabel
    { labelColor :: !Text
    , labelUrl   :: !URL
    , labelName  :: !(Name IssueLabel)
    , labelDesc  :: !(Maybe Text)
    }
  deriving (Show)

instance NFData IssueLabel where
    rnf IssueLabel {..} = rnf labelDesc

instance FromJSON IssueLabel where
    parseJSON = withObject "IssueLabel" $ \o -> IssueLabel
        <$> o .: "color"
        <*> o .:? "url" .!= URL "" -- in events there aren't URL
        <*> o .: "name"
        <*> o .:? "description"

-------------------------------------------------------------------------------
-- PullRequestReference
-------------------------------------------------------------------------------

data PullRequestReference = PullRequestReference
    { pullRequestReferenceHtmlUrl  :: !(Maybe URL)
    , pullRequestReferencePatchUrl :: !(Maybe URL)
    , pullRequestReferenceDiffUrl  :: !(Maybe URL)
    }
    deriving (Show)

instance NFData PullRequestReference where
    rnf PullRequestReference {..} =
        rnf pullRequestReferenceHtmlUrl `seq`
        rnf pullRequestReferencePatchUrl `seq`
        rnf pullRequestReferenceDiffUrl

instance FromJSON PullRequestReference where
    parseJSON = withObject "PullRequestReference" $ \o -> PullRequestReference
        <$> o .:? "html_url"
        <*> o .:? "patch_url"
        <*> o .:? "diff_url"

-------------------------------------------------------------------------------
-- Issue
-------------------------------------------------------------------------------

data Issue = Issue
    { issueClosedAt    :: !(Maybe UTCTime)
    , issueUpdatedAt   :: !UTCTime
    , issueEventsUrl   :: !URL
    , issueHtmlUrl     :: !(Maybe URL)
    , issueClosedBy    :: !(Maybe SimpleUser)
    , issueLabels      :: !(Vector IssueLabel)
    , issueNumber      :: !IssueNumber
    , issueAssignees   :: !(Vector SimpleUser)
    , issueUser        :: !SimpleUser
    , issueTitle       :: !Text
    , issuePullRequest :: !(Maybe PullRequestReference)
    , issueUrl         :: !URL
    , issueCreatedAt   :: !UTCTime
    , issueBody        :: !(Maybe Text)
    , issueState       :: !IssueState
    , issueId          :: !(Id Issue)
    , issueComments    :: !Int
    -- , issueMilestone   :: !(Maybe Milestone)
    }
  deriving (Show)

instance NFData Issue where
    rnf Issue {..} =
        rnf issueClosedAt `seq`
        rnf issueUpdatedAt

instance FromJSON Issue where
    parseJSON = withObject "Issue" $ \o -> Issue
        <$> o .:? "closed_at"
        <*> o .: "updated_at"
        <*> o .: "events_url"
        <*> o .: "html_url"
        <*> o .:? "closed_by"
        <*> o .: "labels"
        <*> o .: "number"
        <*> o .: "assignees"
        <*> o .: "user"
        <*> o .: "title"
        <*> o .:? "pull_request"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .: "body"
        <*> o .: "state"
        <*> o .: "id"
        <*> o .: "comments"
        -- <*> o .:? "milestone"
