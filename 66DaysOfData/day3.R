#Day 3! Let's look at Vegas game by game odds vs over/under wins
library(tidyverse)
library(ggimage)
library(ggrepel)
library(readr)

#odds csv from Dan Morse - Thanks Dan!
urlFile = "https://raw.githubusercontent.com/danmorse314/nfl-stuff/main/nfl_odds_2021.csv"
odds <- read_csv(url(urlFile))

#almost forgot to take out the bye week - very important!
odds <- odds |> filter(opponent != "Bye")

impliedWins <- odds |> group_by(team_abbr) |>
  summarise(avgWins = sum(pnorm(-1 * line,mean=0,sd=12.7))) |> ungroup()

VegasWins <- read.csv("GitHub/ouRAnalytics/66DaysOfData/VegasNFLWins.csv")

impliedWins <- impliedWins |> left_join(VegasWins,by = c('team_abbr' = 'TEAM'))

impliedWins <-  impliedWins |> mutate(simWinsOverVegas = avgWins - WINTOTAL)

colors <- odds |> group_by(team_abbr,team_color) |> summarize(totalSum = sum(line)) |> ungroup() |> 
  select(team_abbr,team_color)

impliedWins <-  impliedWins |> left_join(colors,by='team_abbr')

ggplot(impliedWins,aes(x=team_abbr,y=simWinsOverVegas)) +
  geom_bar(stat="identity",fill=impliedWins$team_color) +
  geom_text(aes(label = ifelse(simWinsOverVegas > 0,OVERODDS,UNDERODDS)),hjust=ifelse(impliedWins$simWinsOverVegas>0,-0.1,1.1),
            vjust=0.35,fontface = 'bold') +
  coord_flip() +
  xlab("Team") +
  ylab("Implied Game by Game Wins Over Vegas Win Total Line") +
  labs(title = "Can We Use Vegas To Beat Vegas?",
       subtitle = "Using a normal distribution with mean=0, Std.Dev.=12.7, and the spread, we can estimate the probability to win each game",
       caption = "Viz by JHamptAnalytics") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  ) 