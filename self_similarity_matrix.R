library(tidyverse)
library(spotifyr)
library(compmus)
library(plotly)
library("gridExtra")
library(cowplot)
source('PlaylistsPrep.R')

# UK timbre and chroma matrices
wannabe <- get_tidy_audio_analysis("1Je1IMUlBXcx1Fz0WE7oPT")
wannabe_matrix <-
  wannabe |> # Change URI.
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
  wannabe_matrix |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  wannabe_matrix |>
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
  get_tidy_audio_analysis("1OppEieGNdItZbE14gLBEv")
Canthurrylove_matrix <- Canthurrylove |> # Change URI.
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
  Canthurrylove_matrix |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  Canthurrylove_matrix |>
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
  get_tidy_audio_analysis("26EM9sZnQkLLQxixGd88KE")
Iamthebest_matrix<- Iamthebest |> # Change URI.
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
  Iamthebest_matrix |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  Iamthebest_matrix |>
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
  get_tidy_audio_analysis("37yHxMij3cpN8SdO915LUI")
japanese_matrix <- japanese |> # Change URI.
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
  japanese_matrix |>
    compmus_self_similarity(pitches, "cosine") |>
    mutate(d = d / max(d), type = "Chroma"),
  japanese_matrix |>
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

# Tempograms
SG <- CompletePlaylist %>% filter(which_group == "Spice Girls (UK)")
TS <- CompletePlaylist %>% filter(which_group == "The Supremes (US)")
NE <- CompletePlaylist %>% filter(which_group == "2NE1 (KR)")
AK <- CompletePlaylist %>% filter(which_group == "AKB48 (JP)")
wannabe_tempogram <- wannabe |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C", guide = "none") +
  labs(title = "'Wannabe' Tempogram", x = "Time (s)", y = "Tempo (BPM)") +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        plot.title = element_text(hjust = 0.5))

sg_tempo <- ggplot(SG, aes(x = tempo)) +
  geom_histogram(binwidth = 15, alpha = .75, fill = "#e60049", 
                 position = 'identity') +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        panel.grid.major = element_line(color = 'gray', linetype = "dotted"),
        panel.grid.minor = element_line(color = 'gray', linetype = "dotted"),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Spice Girls Songs Tempo",
       x = "Tempo", y = "Number of Songs")

spicegirls_tempo <- plot_grid(sg_tempo, wannabe_tempogram, align = "h", 
                              ncol = 2, rel_widths = c(3/8, 5/8))

Canthurrylove_tempogram <- Canthurrylove |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C", guide = "none") +
  labs(title = "'You can't hurry love' Tempogram", x = "Time (s)", y = "Tempo (BPM)") +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        plot.title = element_text(hjust = 0.5))

ts_tempo <- ggplot(TS, aes(x = tempo)) +
  geom_histogram(binwidth = 15, alpha = .75, fill = "#0bb4ff", 
                 position = 'identity') +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        panel.grid.major = element_line(color = 'gray', linetype = "dotted"),
        panel.grid.minor = element_line(color = 'gray', linetype = "dotted"),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "The Supremes Songs Tempo",
       x = "Tempo", y = "Number of Songs")

TheSupremes_tempo <- plot_grid(ts_tempo, Canthurrylove_tempogram, align = "h", 
                              ncol = 2, rel_widths = c(3/8, 5/8))

Iamthebest_tempogram <- Iamthebest |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C", guide = "none") +
  labs(title = "'I am the best' Tempogram", x = "Time (s)", y = "Tempo (BPM)") +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        plot.title = element_text(hjust = 0.5))

ne_tempo <- ggplot(NE, aes(x = tempo)) +
  geom_histogram(binwidth = 15, alpha = .75, fill = "#ffa300", 
                 position = 'identity') +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        panel.grid.major = element_line(color = 'gray', linetype = "dotted"),
        panel.grid.minor = element_line(color = 'gray', linetype = "dotted"),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "2NE1 Songs Tempo",
       x = "Tempo", y = "Number of Songs")

twoNE1_tempo <- plot_grid(ne_tempo, Iamthebest_tempogram, align = "h", 
                               ncol = 2, rel_widths = c(3/8, 5/8))

japanese_tempogram <- japanese |>
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(option = "C", guide = "none") +
  labs(title = "'恋するフォーチュンクッキー' Tempogram", x = "Time (s)", y = "Tempo (BPM)") +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        plot.title = element_text(hjust = 0.5))

ak_tempo <- ggplot(AK, aes(x = tempo)) +
  geom_histogram(binwidth = 15, alpha = .75, fill = "#9b19f5", 
                 position = 'identity') +
  theme(panel.background = element_rect(fill = "#f4ebfe", colour = "#f4ebfe"),
        panel.grid.major = element_line(color = 'gray', linetype = "dotted"),
        panel.grid.minor = element_line(color = 'gray', linetype = "dotted"),
        axis.line=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "AKB48 Songs Tempo",
       x = "Tempo", y = "Number of Songs")

AK_tempo <- plot_grid(ak_tempo, japanese_tempogram, align = "h", 
                          ncol = 2, rel_widths = c(3/8, 5/8))




