# first r graph! 
library(tidyverse)
library(nflfastR)

data2020 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

filteredData <- data2020 |> filter(!is.na(epa),rush==1) |> 
  group_by(rusher,posteam) |> 
  summarize(mean_epa = mean(epa),success_rate=mean(success),ypc=mean(yards_gained),plays=n()) |>
  filter(plays > 40)

filteredData <- filteredData |> arrange(desc(plays)) |>
  group_by(posteam) |> slice(1:2) 

filteredData <-  filteredData |> arrange(posteam,desc(plays))

#graphData <- filteredData |> group_by(posteam) |> summarize(diff_epa = m)

graphData <- filteredData |> group_by(posteam) |>
  arrange(posteam,desc(plays)) |> mutate(diff=lag(`mean_epa`) - `mean_epa`)
  
graphData |> select(posteam,diff) |> filter(!is.na(diff)) |> head(10)

ggplot(data=graphData,aes(x=posteam,y=diff)) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Team") +
  ylab("EPA Difference") +
  labs(title = "Difference in EPA for team's top rusher (by carries) and backup",
       caption = "Viz by JHamptAnalytics")