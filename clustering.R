library(tidyverse)
library(tidymodels)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(compmus)
library("dendextend")
#source('PlaylistsPrep.R')

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}

#uniq_name = make.names(CompletePlaylist$track.name, unique = TRUE)
#row.names(CompletePlaylist) = uniq_name

#CompletePlaylist <- 
#  row.names(CompletePlaylist)=NULL

halloween <-
  CompletePlaylist |>
  add_audio_analysis() |>
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))

halloween_juice <-
  recipe(
    track.name ~
      danceability +
      energy +
      loudness +
      #speechiness +
      #acousticness +
      #instrumentalness +
      #liveness +
      valence +
      tempo +
      duration +
      C + `C#|Db` + D + `D#|Eb` +
      E + `F` + `F#|Gb` + G +
      `G#|Ab` + A + `A#|Bb` + B +
      c01 + c02 + c03 + c04 + c05 + c06 +
      c07 + c08 + c09 + c10 + c11 + c12,
    data = halloween
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  #step_range(all_predictors()) |> 
  prep(halloween |> mutate(track.name = str_trunc(track.name, 20))) |>
  juice() #|>
  #column_to_rownames("track.name")

#rownames(halloween_juice) <- rep(c("Spice Girls (UK)", "The Supremes (US)", "2NE1 (KR)", "AKB48 (JP)"), 
                                # times=c(42, 50, 36, 50))
#uniq_name = make.names(halloween$which_group, unique = TRUE)
#row.names(halloween_juice) = uniq_name

hc <- hclust(dist(halloween_juice, method = "euclidean"), method = "complete")
hcd <- as.dendrogram(hc)
labels(hcd) <- c("The Supremes", "The Supremes", "AKB48", "The Supremes", "The Supremes",
"2NE1", "Spice Girls", "Spice Girls", "Spice Girls", "2NE1", "AKB48", "AKB48",
"AKB48", "AKB48", "Spice Girls", "Spice Girls", "Spice Girls", "Spice Girls",
"Spice Girls", "Spice Girls", "Spice Girls", "The Supremes", "The Supremes",
"The Supremes", "The Supremes", "The Supremes", "The Supremes", "The Supremes",
"The Supremes", "The Supremes", "The Supremes", "The Supremes", "The Supremes",
"The Supremes", "The Supremes", "The Supremes", "The Supremes", "The Supremes",
"The Supremes", "The Supremes", "The Supremes", "The Supremes", "The Supremes",
"The Supremes", "The Supremes", "The Supremes", "The Supremes", "The Supremes",
"The Supremes", "The Supremes", "The Supremes", "The Supremes", "2NE1", "2NE1",
"2NE1", "2NE1", "2NE1", "2NE1", "2NE1", "Spice Girls", "Spice Girls", "2NE1",
"2NE1", "2NE1", "2NE1", "2NE1", "2NE1", "2NE1", "Spice Girls", "The Supremes",
"The Supremes", "Spice Girls", "AKB48", "The Supremes", "The Supremes", "AKB48",
"The Supremes", "The Supremes", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", 
"AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48",
"AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48",
"AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "Spice Girls",
"The Supremes", "Spice Girls", "The Supremes", "2NE1", "2NE1", "2NE1", "2NE1",
"Spice Girls", "2NE1", "2NE1", "Spice Girls", "2NE1", "2NE1", "2NE1", "2NE1",
"2NE1", "2NE1", "The Supremes", "Spice Girls", "The Supremes", "The Supremes",
"The Supremes", "Spice Girls", "AKB48", "2NE1", "2NE1", "2NE1", "Spice Girls",
"Spice Girls", "AKB48", "Spice Girls", "Spice Girls", "Spice Girls", "Spice Girls",
"Spice Girls", "Spice Girls", "Spice Girls", "Spice Girls", "Spice Girls",
"The Supremes", "Spice Girls", "Spice Girls", "2NE1", "2NE1", "The Supremes",
"2NE1", "Spice Girls", "2NE1", "The Supremes", "AKB48", "AKB48", "AKB48",
"AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "AKB48", "2NE1",
"Spice Girls", "Spice Girls", "Spice Girls", "AKB48", "Spice Girls", "Spice Girls",
"Spice Girls", "Spice Girls", "Spice Girls")
#par(mfrow=c(3,1))

#main <- plot(hcd, main="Main")
#upper <- plot(cut(hcd, h=11)$upper, 
     #main="Upper tree of cut at h=75")

#lower <- plot(cut(hcd, h=11)$lower[[8]], 
    #main="Second branch of lower tree with cut at h=75")

#lower <- plot(cut(hcd, h=11)$lower[[2]], 
             # main="Second branch of lower tree with cut at h=75")
