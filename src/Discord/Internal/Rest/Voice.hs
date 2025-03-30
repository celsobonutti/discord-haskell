{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Voice API interactions
module Discord.Internal.Rest.Voice
  ( VoiceRequest(..)
  ) where


import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Types.VoiceState

instance Request (VoiceRequest a) where
  majorRoute = voiceMajorRoute
  jsonRequest = voiceJsonRequest

-- | Data constructor for requests
data VoiceRequest a where
  -- | List all available 'VoiceRegion's.
  ListVoiceRegions :: VoiceRequest [VoiceRegion]
  GetUserVoiceState :: UserId -> GuildId -> VoiceRequest VoiceState

voiceMajorRoute :: VoiceRequest a -> String
voiceMajorRoute c = case c of
  (ListVoiceRegions) -> "whatever"
  (GetUserVoiceState guildId userId) -> "voice-states " <> show guildId <> " " <> show userId

voices :: R.Url 'R.Https
voices = baseUrl /: "voice"

voiceJsonRequest :: VoiceRequest r -> JsonRequest
voiceJsonRequest c = case c of
  (ListVoiceRegions) -> Get (voices /: "regions") mempty
  (GetUserVoiceState userId guildId) -> Get (baseUrl /: "guilds" /~ guildId /: "voice-states" /~ userId) mempty
