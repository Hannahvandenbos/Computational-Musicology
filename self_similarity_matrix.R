library(tidyverse)
library(spotifyr)
library(compmus)
library(plotly)

# Update title
annotations = list(
  list( 
    x = 0.23,  
    y = 1.03,  
    text = "Chroma matrix", 
    font = list(size = 18),
    xref = "paper",
    yref = "paper",
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.75,  
    y = 1.03,  
    text = "Timbre matrix", 
    font = list(size = 18),
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list(
    x = -0.068, 
    y = 0.5, 
    text = "Time (s)",
    font = list(size = 18),
    textangle = 270,
    showarrow = F, 
    xref='paper', 
    yref='paper', 
    size=48),
  list(
    x = 0.5, 
    y = -0.15, 
    text = "Time (s)",
    font = list(size = 18),
    showarrow = F, 
    xref='paper', 
    yref='paper', 
    size=48))

m <- list(
  l = 75,
  r = 25,
  b = 50,
  t = 65,
  pad = 4
)

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

wannabe_chroma <- wannabe |>
  compmus_self_similarity(pitches, "cosine") |> 
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
  scale_fill_viridis_c(option = "C", guide="none") +
  theme_minimal()

wannabe_timbre <- wannabe |>
  compmus_self_similarity(timbre, "euclidean") |> 
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
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_minimal()

wannabe_graph <- subplot(wannabe_chroma, wannabe_timbre, shareY = TRUE, shareX = TRUE, 
                         titleX = FALSE, titleY = FALSE, margin = 0.02)
wannabe_graph <- wannabe_graph %>% layout(plot_bgcolor="#f4ebfe", margin = m, showlegend = FALSE, 
                                         xaxis = list(scaleanchor="y", constraintoward = "top", constrain="range"),
                                         yaxis = list(scaleanchor="x", constraintoward = "top", constrain="domain"))

wannabe_graph <- wannabe_graph %>%layout(annotations = annotations) 
wannabe_graph

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

Canthurrylove_chroma <- Canthurrylove |>
  compmus_self_similarity(pitches, "cosine") |> 
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
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_minimal()

Canthurrylove_timbre <- Canthurrylove |>
  compmus_self_similarity(timbre, "euclidean") |> 
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
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_minimal()

Canthurrylove_graph <- subplot(Canthurrylove_chroma, Canthurrylove_timbre, shareY = TRUE, shareX = TRUE, 
                         titleX = FALSE, titleY = FALSE, margin = 0.02)
Canthurrylove_graph <- Canthurrylove_graph %>%layout(plot_bgcolor="#f4ebfe", margin = m, showlegend = FALSE, 
                                         yaxis = list(zeroline = FALSE, gridcolor = 'ffff',
                                                      scaleanchor="x", constraintoward = "top", constrain="domain"))

Canthurrylove_graph <- Canthurrylove_graph %>%layout(annotations = annotations) 

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

Iamthebest_chroma <- Iamthebest |>
  compmus_self_similarity(pitches, "cosine") |> 
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
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_minimal()

Iamthebest_timbre <- Iamthebest |>
  compmus_self_similarity(timbre, "euclidean") |> 
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
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_minimal()

Iamthebest_graph <- subplot(Iamthebest_chroma, Iamthebest_timbre, shareY = TRUE, shareX = TRUE, 
                               titleX = FALSE, titleY = FALSE, margin = 0.02)
Iamthebest_graph <- Iamthebest_graph %>%layout(plot_bgcolor="#f4ebfe", margin = m, showlegend = FALSE, 
                                                     yaxis = list(zeroline = FALSE, gridcolor = 'ffff',
                                                                  scaleanchor="x", constraintoward = "top", constrain="domain"))

Iamthebest_graph <- Iamthebest_graph %>%layout(annotations = annotations) 

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

japanese_chroma <- japanese |>
  compmus_self_similarity(pitches, "cosine") |> 
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
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_minimal()

japanese_timbre <- japanese |>
  compmus_self_similarity(timbre, "euclidean") |> 
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
  scale_fill_viridis_c(option = "C", guide = "none") +
  theme_minimal()

japanese_graph <- subplot(japanese_chroma, japanese_timbre, shareY = TRUE, shareX = TRUE, 
                            titleX = FALSE, titleY = FALSE, margin = 0.02)
japanese_graph <- japanese_graph %>%layout(plot_bgcolor="#f4ebfe", margin = m, showlegend = FALSE, 
                                               yaxis = list(zeroline = FALSE, gridcolor = 'ffff',
                                                            scaleanchor="x", constraintoward = "top", constrain="domain"))

japanese_graph <- japanese_graph %>%layout(annotations = annotations) 
