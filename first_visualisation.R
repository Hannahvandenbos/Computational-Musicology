library(plotly)
source('PlaylistsPrep.R')

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
fig1 <- plot_ly(SG, x = ~valence, y = ~energy, color = 'SG',
                colors = "#e60049", text = ~paste("",track.name, "<br>", which_group,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy),  hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

TS <- CompletePlaylist %>% filter(which_group == "The Supremes (US)")
fig2 <- plot_ly(TS, x = ~valence, y = ~energy, color = 'TS',
                colors = "#0bb4ff", text = ~paste("",track.name, "<br>", which_group,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy),  hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

NE <- CompletePlaylist %>% filter(which_group == "2NE1 (KR)")
fig3 <- plot_ly(NE, x = ~valence, y = ~energy, color = 'NE',
                colors = "#ffa300", text = ~paste("",track.name, "<br>", which_group,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy),  hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

AK <- CompletePlaylist %>% filter(which_group == "AKB48 (JP)")
fig4 <- plot_ly(AK, x = ~valence, y = ~energy, color = 'AK',
                colors = "#9b19f5", text = ~paste("",track.name, "<br>", which_group,
                                                  "<br> Valence: ", valence, "<br> Energy: ",
                                                  energy),  hoverinfo = 'text',
                type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = x_layout, yaxis = y_layout)

m <- list(
  l = 50,
  r = 25,
  b = 50,
  t = 75
  #pad = 4
)
fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2, shareY = TRUE, shareX = TRUE, titleX = FALSE,
               titleY = FALSE, margin = 0.02)
fig <- fig %>%layout(title = list(text = "Valence vs energy as mood indicator", font = list(size = 31), yanchor = 'bottom'),
                     plot_bgcolor="#f4ebfe", margin = m, showlegend = FALSE)

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
fig
