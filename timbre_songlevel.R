library(tidyverse)
library(spotifyr)
library(compmus)

bebop <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "55s8gstHcaCyfU47mQgLrB"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
bigband <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "2cjIvuw4VVOQSeUAZfNiqY"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
jazz <-
  bebop |>
  mutate(genre = "Bebop") |>
  bind_rows(bigband |> mutate(genre = "Big Band"))