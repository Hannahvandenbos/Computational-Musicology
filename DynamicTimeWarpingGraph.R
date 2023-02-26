library(tidyverse)
library(spotifyr)
library(compmus)

# Load in orignal I hear a symphony
original <-
  get_tidy_audio_analysis("5SCB7L1alKg7ZEeSw7Sq9f") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

# Load in orchestra version of I hear a symphony
orchestra <-
  get_tidy_audio_analysis("6C6kuknWLMfHZDth5kDZOx") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

# Chromogram of original
original |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

# Chromogram of orchestra version
orchestra |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

# Dynamic Time Warping between the two
DTW <-
  compmus_long_distance(
    original |> mutate(pitches = map(pitches, compmus_normalise, "euclidean")),
    orchestra |> mutate(pitches = map(pitches, compmus_normalise, "euclidean")),
    feature = pitches,
    method = "angular"
  ) |>
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
  coord_equal() +
  labs(x = "The Supremes (original)", y = "The Supremes with Royal Philharmonic Orchestra", fill = "Magnitude") +
  theme(panel.background = element_rect(fill = '#f4ebfe'), 
        plot.title = element_text(hjust = 0.37, size = 15, family="Sen", margin = margin(0, 0, 20, 0)),
        text=element_text(size=12,  family="Sen", margin = margin(0, -5, 0, 0))) +
  scale_fill_viridis_c(option = "C") +
  ggtitle("Dynamic Time Warping: 'I hear a symphony' orignal vs with orchestra")

DTW
