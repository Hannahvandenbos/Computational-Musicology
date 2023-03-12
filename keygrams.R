library(tidyverse)
library(spotifyr)
library(compmus)
library(forcats)
source('PlaylistsPrep.R')

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )

# Spice Girls
SpiceGirls_keys <- SpiceGirls %>% 
  add_audio_analysis() %>%
  group_by(track.name) %>%
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  ) %>%
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "aitchison",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  group_by(track.name, start) %>%
  summarize(key_name = name[which(d == max(d))]) %>%
  ggplot(aes(x = fct_infreq(key_name))) +
  geom_bar(stat = 'count', fill="#e60049", alpha = 0.75) +
  xlab("Keys") + ylab("Count") +
  theme(panel.background = element_rect(fill = "#f4ebfe"))

# The Supremes
TheSupremes_keys <- TheSupremes %>% 
  add_audio_analysis() %>%
  group_by(track.name) %>%
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  ) %>%
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "aitchison",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  group_by(track.name, start) %>%
  summarize(key_name = name[which(d == max(d))]) %>%
  ggplot(aes(x = fct_infreq(key_name))) +
  geom_bar(stat = 'count', fill="#0bb4ff", alpha = 0.75) +
  xlab("Keys") + ylab("Count") +
  theme(panel.background = element_rect(fill = "#f4ebfe"))

# 2NE1
twoNE1_keys <- twoNE1 %>% 
  add_audio_analysis() %>%
  group_by(track.name) %>%
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  ) %>%
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "aitchison",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  group_by(track.name, start) %>%
  summarize(key_name = name[which(d == max(d))]) %>%
  ggplot(aes(x = fct_infreq(key_name))) +
  geom_bar(stat = 'count', fill="#ffa300", alpha = 0.75) +
  xlab("Keys") + ylab("Count") +
  theme(panel.background = element_rect(fill = "#f4ebfe"))

# AKB48
AKB48_keys <- AKB48 %>% 
  add_audio_analysis() %>%
  group_by(track.name) %>%
  compmus_align(sections, segments) |>
  select(sections) |>
  unnest(sections) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      )
  ) %>%
  compmus_match_pitch_template(
    key_templates,         # Change to chord_templates if descired
    method = "aitchison",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  group_by(track.name, start) %>%
  summarize(key_name = name[which(d == max(d))]) %>%
  ggplot(aes(x = fct_infreq(key_name))) +
  geom_bar(stat = 'count', fill="#9b19f5", alpha = 0.75) +
  xlab("Keys") + ylab("Count") +
  theme(panel.background = element_rect(fill = "#f4ebfe"))

