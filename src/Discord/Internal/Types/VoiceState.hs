{-# LANGUAGE OverloadedStrings #-}

module Discord.Internal.Types.VoiceState where

import Data.Aeson
import Data.Text (Text)
import Data.Time.ISO8601 (parseISO8601)
import Data.Time (UTCTime)
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User (GuildMember)

data VoiceState = VoiceState
        { voiceGuildId                 :: Maybe GuildId
        , voiceChannelId               :: Maybe ChannelId
        , voiceUserId                  :: UserId
        , voiceMember                  :: Maybe GuildMember
        , voiceSessionId               :: Text
        , voiceDeaf                    :: Bool
        , voiceMute                    :: Bool
        , voiceSelfDeaf                :: Bool
        , voiceSelfMute                :: Bool
        , voiceStreaming               :: Maybe Bool
        , voiceSelfVideo               :: Bool
        , voiceSupress                 :: Bool
        , voiceRequestToSpeakTimestamp :: Maybe UTCTime
        } deriving (Show, Read, Eq, Ord)

instance FromJSON VoiceState where
  parseJSON = withObject "VoiceState" $ \o -> do
    VoiceState <$> o .:? "guild_id"
               <*> o .:? "channel_id"
               <*> o .:  "user_id"
               <*> o .:? "member"
               <*> o .:  "session_id"
               <*> o .:  "deaf"
               <*> o .:  "mute"
               <*> o .:  "self_deaf"
               <*> o .:  "self_mute"
               <*> o .:? "self_stream"
               <*> o .:  "self_video"
               <*> o .:  "suppress"
               <*> do stamp <- o .:? "request_to_speak_timestamp"
                      pure $ stamp >>= parseISO8601
