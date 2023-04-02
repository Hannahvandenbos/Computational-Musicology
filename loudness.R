library(tidyverse)
library(spotifyr)
library(compmus)
library(plotly)
#library("gridExtra")
#library(cowplot)
#source('PlaylistsPrep.R')

wannabe <- get_tidy_audio_analysis("1Je1IMUlBXcx1Fz0WE7oPT")
Canthurrylove <- get_tidy_audio_analysis("1OppEieGNdItZbE14gLBEv")
Iamthebest <- get_tidy_audio_analysis("26EM9sZnQkLLQxixGd88KE")
japanese <- get_tidy_audio_analysis("37yHxMij3cpN8SdO915LUI")

wannabe_cepstrogram <-
  wannabe |> 
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe"))

Canthurrylove_cepstrogram <-
  Canthurrylove |> 
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe"))

Iamthebest_cepstrogram <-
  Iamthebest |> 
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe"))

japanese_cepstrogram <-
  japanese |> 
  compmus_align(bars, segments) |>                     # Change `bars`
  select(bars) |>                                      #   in all three
  unnest(bars) |>                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) |>
  compmus_gather_timbre() |>
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe"))