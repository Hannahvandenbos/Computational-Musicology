library(tidyverse)
library(spotifyr)
library(compmus)
library(plotly)

# UK timbre and chroma matrices
wannabe <-
  get_tidy_audio_analysis("1Je1IMUlBXcx1Fz0WE7oPT") |> # Change URI.
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
          method = "mean"              # Change summary & norm.
      )
  )

wannabe_graph <- bind_rows(
  wannabe |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  wannabe |>
    compmus_self_similarity(timbre, "euclidean") |>
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe")) +
  labs(x = "Time (s)", y = "Time (s)")

# US timbre and chroma matrices
Canthurrylove <-
  get_tidy_audio_analysis("1OppEieGNdItZbE14gLBEv") |> # Change URI.
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
          method = "mean"             # Change summary & norm.
      )
  )

Canthurrylove_graph <- bind_rows(
  Canthurrylove |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  Canthurrylove |>
    compmus_self_similarity(timbre, "euclidean") |>
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe")) +
  labs(x = "Time (s)", y = "Time (s)")

# Korean timbre and chroma matrices
Iamthebest <-
  get_tidy_audio_analysis("26EM9sZnQkLLQxixGd88KE") |> # Change URI.
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
          method = "mean"             # Change summary & norm.
      )
  )

Iamthebest_graph <- bind_rows(
  Iamthebest |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  Iamthebest |>
    compmus_self_similarity(timbre, "euclidean") |>
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe")) +
  labs(x = "Time (s)", y = "Time (s)")

# Japanese timbre and chroma matrices
japanese <-
  get_tidy_audio_analysis("37yHxMij3cpN8SdO915LUI") |> # Change URI.
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
          method = "mean"              # Change summary & norm.
      )
  )

japanese_graph <- bind_rows(
  japanese |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  japanese |>
    compmus_self_similarity(timbre, "euclidean") |>
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |>
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#f4ebfe",
                                        colour = "#f4ebfe")) +
  labs(x = "Time (s)", y = "Time (s)")
