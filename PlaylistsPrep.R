library(spotifyr)
library(tidyverse)
library(dplyr)
library(plotly)

# Get playlists
SpiceGirls <- unique(get_playlist_audio_features("", "37i9dQZF1DWWUJdr9ahsbf"))
SpiceGirls <- SpiceGirls[-1,]
TheSupremes <- unique(get_playlist_audio_features("", "37i9dQZF1DZ06evO30nCXS"))
twoNE1 <- unique(get_playlist_audio_features("", "37i9dQZF1DZ06evO0Jta80"))
AKB48 <- unique(get_playlist_audio_features("", "37i9dQZF1DZ06evNZWreOe"))

# 1 = spice girls, 2 = the supremes, 3 = 2NE1, 4 = AKB48
which_group <- rep(c("Spice Girls (UK)", "The Supremes (US)", "2NE1 (KR)", "AKB48 (JP)"), 
                   times=c(42, 50, 36, 50))
# combine playlists
CompletePlaylist <- rbind(SpiceGirls, TheSupremes, twoNE1, AKB48)
# remove duplicates
CompletePlaylist <- unique(CompletePlaylist)
# add column with generation
CompletePlaylist <- cbind(CompletePlaylist, which_group)