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
                   times=c(49, 43, 50, 36))
# combine playlists
CompletePlaylist <- rbind(SpiceGirls, TheSupremes, twoNE1, AKB48)
# remove duplicates
CompletePlaylist <- unique(CompletePlaylist)
# add column with generation
CompletePlaylist <- cbind(CompletePlaylist, which_group)
# numerical playlist
#numeric <- CompletePlaylist %>% 
#  select(where(is.numeric)) %>%
#  group_by(which_group) %>%
#  summarize(mean_dance = mean(energy))
#numeric['mean_dance']

t <- list(
  family = "Consolas",
  size = 32,
  color = 'black')

coleurs <- c("#0bb4ff", "#9b19f5", "#ffa300", "#e60049")
hovertext <- paste(CompletePlaylist$track.name, CompletePlaylist$which_group, sep = "<br>")
bubblechart <- plot_ly(CompletePlaylist, x = ~valence, y = ~energy, color = ~which_group,
                       colors = coleurs, text = ~paste("",track.name, "<br>", which_group,
                                                       "<br> Valence: ", valence, "<br> Energy: ",
                                                       energy),  hoverinfo = 'text',
                       type = 'scatter', mode = 'markers')

bubblechart <- bubblechart %>% layout(title = list(text = 'Valence vs energy as mood indicator',
                                                   font = t, yanchor = 'top'),
                                      xaxis = list(title = list(text = 'Valence', font = 'Consolas'),
                                                   zeroline = FALSE,
                                                   range = c(0, 1.1),
                                                   showgrid = FALSE),
                                      yaxis = list(title = list(text = 'Energy', font = 'Consolas'),
                                                   zeroline = FALSE,
                                                   range = c(0, 1.1),
                                                   showgrid = FALSE), showlegend = FALSE)
                                      #legend = list(title=list(x = 1, y = 1, text=' Girl groups',font = 'Consolas')))