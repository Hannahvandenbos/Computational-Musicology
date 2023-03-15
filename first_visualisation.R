library(plotly)
library(ggplot2)
source('PlaylistsPrep.R')
library(dplyr)

# numerical playlist
#numeric <- CompletePlaylist %>% 
#  select(where(is.numeric)) %>%
#  group_by(which_group) %>%
#  summarize(mean_dance = mean(energy))
#numeric['mean_dance']

t <- list(
  family = "Consolas",
  size = 13,
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

x_layout <- (xaxis = list(title = list(text = 'Valence', font = 'Consolas'),
                                zeroline = FALSE,
                                range = c(0, 1.1),
                                showgrid = FALSE)) 

y_layout <- (yaxis = list(title = list(text = 'Energy', font = 'Consolas'),
                                zeroline = FALSE,
                                range = c(0, 1.1),
                                showgrid = FALSE))

SG <- CompletePlaylist %>% filter(which_group == "Spice Girls (UK)")
fig1 <- plot_ly(SG, x = ~valence, y = ~energy, size = ~danceability, color = 'SG',
                colors = "#e60049", alpha =0.75, text = ~paste("",track.name,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy, "<br> Danceability: ", danceability),
                hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

TS <- CompletePlaylist %>% filter(which_group == "The Supremes (US)")
fig2 <- plot_ly(TS, x = ~valence, y = ~energy, size = ~danceability, color = 'TS',
                colors = "#0bb4ff", alpha =0.75, text = ~paste("",track.name,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy, "<br> Danceability: ", danceability),
                hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

NE <- CompletePlaylist %>% filter(which_group == "2NE1 (KR)")
fig3 <- plot_ly(NE, x = ~valence, y = ~energy, size = ~danceability, color = 'NE',
                colors = "#ffa300", alpha =0.75, text = ~paste("",track.name,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy, "<br> Danceability: ", danceability),
                hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

AK <- CompletePlaylist %>% filter(which_group == "AKB48 (JP)")
fig4 <- plot_ly(AK, x = ~valence, y = ~energy, size = ~danceability, color = 'AK',
                colors = "#9b19f5", alpha =0.75, text = ~paste("",track.name,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy, "<br> Danceability: ", danceability), 
                hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

m <- list(
  l = 75,
  r = 25,
  b = 55,
  t = 25
  #pad = 4
)

fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2, shareY = TRUE, shareX = TRUE, titleX = FALSE,
               titleY = FALSE, margin = 0.02)
fig <- fig %>%layout(plot_bgcolor="#f4ebfe", margin = m, showlegend = FALSE)
# title = list(text = "Valence vs energy as mood indicator", font = list(size = 31), yanchor = 'bottom')


# Update title
annotations = list(
  list( 
    x = 0.23,  
    y = 1.0,  
    text = "Spice Girls (UK)",  
    xref = "paper",
    yref = "paper",
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.78,  
    y = 1,  
    text = "The Supremes (US)",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.23,  
    y = 0.48,  
    text = "2NE1 (KR)",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.78,  
    y = 0.48,  
    text = "AKB48 (JP)",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list(
    x = -0.06, 
    y = 0.5, 
    text = "Energy",
    font = list(size = 18),
    textangle = 270,
    showarrow = F, 
    xref='paper', 
    yref='paper', 
    size=48),
  list(
    x = 0.5, 
    y = -0.09, 
    text = "Valence",
    font = list(size = 18),
    showarrow = F, 
    xref='paper', 
    yref='paper', 
    size=48))

fig <- fig %>%layout(annotations = annotations) 

titles_and_stuff = list(
  list( 
    x = 0.118,  
    y = 1.05,  
    text = "Spice Girls (UK)",  
    xref = "paper",
    yref = "paper",
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.375,  
    y = 1.05,  
    text = "The Supremes (US)",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.63,  
    y = 1.05,  
    text = "2NE1 (KR)",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.885,  
    y = 1.05,  
    text = "AKB48 (JP)",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list(
    x = -0.05, 
    y = 0.5, 
    text = "Number of Songs",
    font = list(size = 18),
    textangle = 270,
    showarrow = F, 
    xref='paper', 
    yref='paper', 
    size=48),
  list(
    x = 0.5, 
    y = -0.2, 
    text = "Valence",
    font = list(size = 18),
    showarrow = F, 
    xref='paper', 
    yref='paper', 
    size=48))

m_tempo <- list(
  l = 75,
  r = 25,
  b = 50,
  t = 50
  #pad = 4
)

sg_tempo <- plot_ly(SG, x=~tempo, name=~which_group, color = 'SG', colors = "#e60049", alpha =0.75,
                    type= "histogram", hovertemplate = paste('Tempo range: %{x} <br>', 
                                                             'Number of songs: %{y}<extra></extra>'))
ts_tempo <- plot_ly(TS, x=~tempo, name=~which_group, color = 'TS', colors = "#0bb4ff", alpha =0.75, 
                    type= "histogram", hovertemplate = paste('Tempo range: %{x} <br>', 
                                                             'Number of songs: %{y}<extra></extra>'))
ne_tempo <- plot_ly(NE, x=~tempo, name=~which_group, color = 'NE', colors = "#ffa300", alpha =0.75,
                    type= "histogram", hovertemplate = paste('Tempo range: %{x} <br>', 
                                                             'Number of songs: %{y}<extra></extra>'))
ak_tempo <- plot_ly(AK, x=~tempo, name=~which_group, color = 'AK', colors = "#9b19f5", alpha=0.75,
                    type= "histogram", hovertemplate = paste('Tempo range: %{x} <br>', 
                                                             'Number of songs: %{y}<extra></extra>'))
tempo_plot <- subplot(sg_tempo, ts_tempo, ne_tempo, ak_tempo, nrows = 1, shareY = TRUE, 
                      shareX = TRUE, titleX = FALSE, titleY = FALSE, margin = 0.025) %>% 
  layout(plot_bgcolor="#f4ebfe", margin = m_tempo, showlegend = FALSE, 
         annotations = titles_and_stuff)
tempo_plot



modes <- CompletePlaylist %>% 
  mutate(mode_name = ifelse(mode == "1", "Major",
                            ifelse(mode == "0", "Minor", "no"))) %>%
  group_by(which_group) %>%
  mutate(which_group = factor(which_group, levels = c("Spice Girls (UK)", "The Supremes (US)", "2NE1 (KR)", "AKB48 (JP)"))) %>%
  ggplot(aes(x = which_group, fill=mode_name)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

sg_mode <- SG %>% 
  mutate(mode_name = ifelse(mode == "1", "Major",
                            ifelse(mode == "0", "Minor", "no"))) %>%
  ggplot(aes(x = which_group, fill=mode_name)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  xlab("") + ylab("")

ts_mode <- TS %>% 
  mutate(mode_name = ifelse(mode == "1", "Major",
                            ifelse(mode == "0", "Minor", "no"))) %>%
  ggplot(aes(x = which_group, fill=mode_name)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  xlab("") + ylab("")

ne_mode <- NE %>% 
  mutate(mode_name = ifelse(mode == "1", "Major",
                            ifelse(mode == "0", "Minor", "no"))) %>%
  ggplot(aes(x = which_group, fill=mode_name)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
  xlab("") + ylab("")

ak_mode <- AK %>% 
  mutate(mode_name = ifelse(mode == "1", "Major",
                            ifelse(mode == "0", "Minor", "no"))) %>%
  ggplot(aes(x = which_group, fill=mode_name)) +
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("#D55E00", "#CC79A7")) +
  xlab("") + ylab("")




mode_count <- c()

sg_mode <- plot_ly(SG, x= ~sum(mode), name=~which_group, color = 'SG', colors = "#e60049", alpha =0.75,
                    type= "histogram", hovertemplate = paste('Tempo range: %{x} <br>', 
                                                             'Number of songs: %{y}<extra></extra>'))


