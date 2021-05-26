#day 2! 
#most clutch punters
library(nflfastR)
library(tidyverse)
library(ggrepel)
library(ggimage)

seasons <- 1999:2020
allData <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

punts <- allData |> filter(play_type == 'punt',half_seconds_remaining < 300, game_half == 'Half2',!is.na(epa)) |>
  group_by(punter_player_id,punter_player_name) |>
  summarize(
    epa = mean(epa),
    wpa = mean(wpa),
    n_plays = n(),
    team = last(posteam)
  ) |> ungroup() |>
  filter(n_plays > 45)

punts <- punts |> left_join(teams_colors_logos,by = c('team' = 'team_abbr'))

ggplot(punts,aes(x = epa, y = wpa)) +
  geom_hline(yintercept = mean(punts$wpa),color="blue",linetype="dashed",alpha=0.5) +
  geom_vline(xintercept = mean(punts$epa),color="blue",linetype="dashed",alpha=0.5) +
  geom_point(color=punts$team_color, cex = punts$n_plays / 15, alpha = 0.7) + 
  geom_text_repel(aes(label=punter_player_name)) + 
  stat_smooth(geom='line',alpha=0.5,se=FALSE,method='lm') + 
  labs(x = "EPA per punt",y = "Win Probability Added per punt",
       title = "Which Punter Has the ~Clutch Gene~? Last 5 Minutes of the Game",
       caption = "Viz by JHamptAnalytics") + 
  theme_bw() + 
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
