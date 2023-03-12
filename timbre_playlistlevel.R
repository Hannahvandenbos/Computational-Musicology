library(tidyverse)
library(spotifyr)
library(compmus)

hits <-
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

jazz |>
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) |>
  select(genre, timbre) |>
  compmus_gather_timbre() |>
  ggplot(aes(x = basis, y = value, fill = genre)) +
  geom_violin() +
  scale_fill_viridis_d() +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Genre")